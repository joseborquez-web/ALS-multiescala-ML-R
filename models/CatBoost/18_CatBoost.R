# ================================================================
# SCRIPT: Entrenamiento y evaluación de CatBoost (catboost.train)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script entrena y evalúa un modelo de clasificación basado
#   en CatBoost, diseñado para manejar datasets desbalanceados.
#   Incluye preprocesamiento manual, configuración de parámetros
#   y evaluación con métricas multiclase.
#
# Flujo:
#   1. Lectura de datos de entrenamiento y evaluación (.rds).
#   2. Selección de métricas definitivas.
#   3. División interna en train/valid.
#   4. Preprocesamiento: filtrado de vars, imputación mediana.
#   5. Definición de parámetros del modelo (CatBoost).
#   6. Entrenamiento sobre train + validación.
#   7. Evaluación en test: métricas, matriz de confusión, importancia.
#   8. Exportación de resultados (csv, html, gráficos, modelo rds).
#
# Paquetes requeridos:
#   rlang, tidymodels, themis, tidyverse, gt, fs, furrr,
#   future, progressr, catboost, jsonlite
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/resultados_CatBoost/*
# ================================================================

install.packages(c("rlang","tidymodels","themis","tidyverse","gt","fs",
                   "furrr","future","progressr","catboost","jsonlite"))

# -------------------------------------
# LIBRERÍAS NECESARIAS
# -------------------------------------
library(rlang)         # Utilidades de programación
library(tidymodels)    # Framework de ML
library(themis)        # Balanceo de clases
library(readr)         # Lectura/escritura de datos
library(dplyr)         # Manipulación de datos
library(ggplot2)       # Gráficos
library(gt)            # Tablas HTML
library(yardstick)     # Métricas de evaluación
library(fs)            # Manejo de rutas y directorios
library(future)        # Backend para paralelismo
library(furrr)         # Paralelización de procesos
library(progressr)     # Barra de progreso
library(recipes)       # Preprocesamiento
library(parsnip)       # Definición de modelos
library(workflows)     # Recetas + modelos
library(tune)          # Ajuste de hiperparámetros
library(dials)         # Rangos de hiperparámetros
library(rsample)       # Particiones y resampling
library(tibble)        # Tablas modernas
library(tidyverse)     # Colección de paquetes tidy
library(purrr)         # Programación funcional
library(tidyr)         # Transformación de datos
library(catboost)      # Algoritmo CatBoost
library(jsonlite)      # Manejo de JSON

options(future.globals.maxSize = 30 * 1024^3)  # 30 GiB


# -------------------------------------
# CONFIGURACIÓN DE PARALELISMO
# -------------------------------------
plan(multisession, workers = 3)


# -------------------------------------
# CONFIGURACIÓN GENERAL
# -------------------------------------
modo_muestra <- TRUE    # TRUE = usar muestra (ensayo rápido), FALSE = todo dataset
porcentaje_muestra <- 1 # % de datos de entrenamiento si modo_muestra=TRUE


# -------------------------------------
# RUTAS
# -------------------------------------
carpeta_base <- "./data/salida/algoritmos/"
archivo_train <- file.path(carpeta_base, "entrenamiento.rds")
archivo_test  <- file.path(carpeta_base, "evaluacion.rds")
carpeta_resultados_CatBoost <- file.path(carpeta_base, "resultados_CatBoost")
dir_create(carpeta_resultados_CatBoost)


# -------------------------------------
# CARGAR DATOS
# -------------------------------------
datos_train <- readRDS(archivo_train)
datos_test  <- readRDS(archivo_test)

if (modo_muestra) {
  set.seed(123)
  datos_train <- sample_frac(datos_train, porcentaje_muestra)
}

# Convertir variable objetivo en factor
datos_train$Classification <- as.factor(datos_train$Classification)
datos_test$Classification  <- factor(datos_test$Classification,
                                     levels = levels(datos_train$Classification))

# Guardamos niveles de clases para mapear a índices 0-based
niveles_clases <- levels(datos_train$Classification)


# -------------------------------------
# SELECCIONAR MÉTRICAS DEFINITIVAS
# -------------------------------------
variables_seleccionadas <- c(
  "Classification",         # Variable objetivo
  "Intensity",              # Intensidad LiDAR
  "Z_normalizada",          # Altura relativa normalizada
  "Nx_(4)",                 # Normal en X (radio 4)
  "1st order moment (4)",   # Momento de primer orden (radio 4)
  "Verticality_(4)",        # Verticalidad (radio 4)
  "Mean curvature (3)",     # Curvatura media (radio 3)
  "Omnivariance_(3)"        # Omnivarianza (radio 3)
)

datos_train <- datos_train %>% select(all_of(variables_seleccionadas))
datos_test  <- datos_test  %>% select(all_of(variables_seleccionadas))

gc()


# -------------------------------------
# SPLIT TRAIN/VALID
# -------------------------------------
# División 85/15 estratificada en train/valid
set.seed(123)
split <- rsample::initial_split(datos_train, prop = 0.85, strata = Classification)
train_i <- rsample::training(split)
valid_i <- rsample::testing(split)


# -------------------------------------
# PREPROCESAMIENTO
# -------------------------------------
# 1) Filtrar numéricas con varianza > 0
X_train <- train_i %>%
  select(-Classification) %>%
  select(where(is.numeric)) %>%
  select(where(~ var(.x, na.rm = TRUE) > 0))

# 2) Alinear columnas de valid/test con train
X_valid <- valid_i %>% select(all_of(colnames(X_train)))
X_test  <- datos_test %>% select(all_of(colnames(X_train)))

# 3) Imputación por mediana (aprendida en train)
meds <- sapply(X_train, function(col) median(col, na.rm = TRUE))
rellena_mediana <- function(df, meds) {
  for (nm in names(meds)) if (nm %in% names(df)) {
    idx <- is.na(df[[nm]])
    if (any(idx)) df[[nm]][idx] <- meds[[nm]]
  }
  df
}
X_train <- rellena_mediana(X_train, meds)
X_valid <- rellena_mediana(X_valid, meds)
X_test  <- rellena_mediana(X_test,  meds)

# 4) Etiquetas como enteros (0-based)
y_train <- as.integer(train_i$Classification) - 1
y_valid <- as.integer(valid_i$Classification) - 1
y_eval  <- as.integer(datos_test$Classification) - 1

# 5) Crear pools de CatBoost
pool_train <- catboost.load_pool(data = X_train, label = y_train)
pool_valid <- catboost.load_pool(data = X_valid, label = y_valid)
pool_eval  <- catboost.load_pool(data = X_test,  label = y_eval)


# -------------------------------------
# PARÁMETROS DEL MODELO
# -------------------------------------
parametros <- list(
  loss_function = "MultiClass",       # Clasificación multiclase
  eval_metric   = "TotalF1",          # F1 macro (mejor para clases desbalanceadas)
  iterations    = 2000,               # Nº de iteraciones (árboles)
  learning_rate = 0.02,               # Tasa de aprendizaje
  depth         = 8,                  # Profundidad de árbol
  l2_leaf_reg   = 6.0,                # Regularización L2
  random_seed   = 123,                # Semilla reproducible
  thread_count  = 1,                  # Nº de hilos (ajustable)
  auto_class_weights = "Balanced",    # Balanceo automático de clases
  bootstrap_type    = "Bayesian",     
  bagging_temperature = 1,
  od_type = "Iter",                   # Early stopping por iteraciones
  od_wait = 80,                       # Nº de iteraciones sin mejora
  verbose = 100                       # Frecuencia de logs
)

# Guardar parámetros en JSON
write_json(parametros, file.path(carpeta_resultados_CatBoost, "parametros_usados_catboost.json"),
           pretty = TRUE, auto_unbox = TRUE)


# -------------------------------------
# ENTRENAMIENTO
# -------------------------------------
modelo_catboost <- catboost.train(pool_train, test_pool = pool_valid, params = parametros)


# -------------------------------------
# PREDICCIÓN
# -------------------------------------
pred_idx <- catboost.predict(modelo_catboost, pool_eval, prediction_type = "Class")

predicciones_catboost <- tibble(
  .pred_class    = factor(niveles_clases[pred_idx + 1], levels = niveles_clases),
  Classification = factor(niveles_clases[y_eval + 1],   levels = niveles_clases)
)


# -------------------------------------
# MÉTRICAS DE EVALUACIÓN
# -------------------------------------
metricas_catboost <- predicciones_catboost %>%
  metrics(truth = Classification, estimate = .pred_class) %>%
  bind_rows(
    conf_mat(predicciones_catboost, truth = Classification, estimate = .pred_class) %>%
      summary()
  )


# -------------------------------------
# GUARDAR RESULTADOS
# -------------------------------------
write_csv(predicciones_catboost, file.path(carpeta_resultados_CatBoost, "predicciones_CatBoost.csv"))
write_csv(metricas_catboost,    file.path(carpeta_resultados_CatBoost, "metricas_CatBoost.csv"))

tabla_metricas_catboost <- metricas_catboost %>%
  select(.metric, .estimate) %>%
  mutate(.estimate = round(.estimate, 3)) %>%
  gt() %>%
  tab_header(title = "Métricas de desempeño - CatBoost")

gtsave(tabla_metricas_catboost, file.path(carpeta_resultados_CatBoost, "tabla_metricas_CatBoost.html"))


# -------------------------------------
# MATRIZ DE CONFUSIÓN NORMALIZADA
# -------------------------------------
nombres_clases <- c(
  "2"  = "Suelo", "6"  = "Edificio", "7"  = "Ruido",
  "9"  = "Agua", "21" = "Aerogenerador", "22" = "Torre eléctrica",
  "23" = "Cable eléctrico", "24" = "Vías", "25" = "Puente",
  "26" = "Placas solares", "27" = "Ferrocarril",
  "28" = "Invernadero", "29" = "Vegetación"
)

predicciones_catboost <- predicciones_catboost %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class), levels = names(nombres_clases))
  )

matriz_conf_cat <- predicciones_catboost %>%
  conf_mat(truth = Classification, estimate = .pred_class) %>%
  pluck("table") %>%
  as_tibble() %>%
  group_by(Truth) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Truth      = factor(nombres_clases[as.character(Truth)],      levels = unname(nombres_clases)),
    Prediction = factor(nombres_clases[as.character(Prediction)], levels = unname(nombres_clases))
  ) %>%
  complete(Truth, Prediction, fill = list(n = 0, prop = 0))

grafico_conf_cat <- ggplot(matriz_conf_cat, aes(x = Prediction, y = Truth, fill = prop)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% Relativo") +
  scale_y_discrete(limits = rev(levels(matriz_conf_cat$Truth))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Confusión Normalizada - CatBoost",
    caption = "Valores relativos por clase real (por fila)",
    x = "Clase predicha", y = "Clase real"
  )

ggsave(file.path(carpeta_resultados_CatBoost, "matriz_confusion_CatBoost_normalizada.jpg"),
       grafico_conf_cat, width = 10, height = 7, dpi = 1200)


# -------------------------------------
# IMPORTANCIA DE VARIABLES
# -------------------------------------
importancias <- catboost.get_feature_importance(modelo_catboost, pool = pool_train)
importancia_df_catboost <- tibble(
  Variable   = colnames(X_train),
  Importance = as.vector(importancias)
)

grafico_importancia_catboost <- ggplot(importancia_df_catboost, aes(x = reorder(Variable, Importance),
                                                                    y = Importance, fill = Importance)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Importancia de las Variables - CatBoost",
    x = "Variable", y = "Importancia",
    caption = "El número ( ) indica el radio usado en la métrica"
  )

ggsave(file.path(carpeta_resultados_CatBoost, "importancia_variables_CatBoost.jpg"),
       grafico_importancia_catboost, width = 10, height = 6, dpi = 1200)

saveRDS(modelo_catboost, file.path(carpeta_resultados_CatBoost, "modelo_final_catboost.rds"))
write_csv(importancia_df_catboost, file.path(carpeta_resultados_CatBoost, "importancia_variables_CatBoost.csv"))


# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ CatBoost entrenado, evaluado y resultados exportados:\n")
cat("📁 modelo_final_catboost.rds\n📁 predicciones_CatBoost.csv\n📁 metricas_CatBoost.csv\n📁 tabla_metricas_CatBoost.html\n📁 matriz_confusion_CatBoost_normalizada.jpg\n📁 importancia_variables_CatBoost.jpg\n📁 importancia_variables_CatBoost.csv\n\n")

print(metricas_catboost)
