# ================================================================
# SCRIPT: Entrenamiento y evaluación de XGBoost (boost_tree/xgboost)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script entrena y evalúa un modelo de clasificación basado
#   en XGBoost, implementado con tidymodels/parsnip.
#
# Flujo:
#   1. Lectura de datos de entrenamiento y evaluación (.rds).
#   2. Selección de métricas definitivas.
#   3. Preprocesamiento: eliminación de var. constantes + imputación.
#   4. Definición del modelo XGBoost.
#   5. Tuning de hiperparámetros con bootstraps estratificados.
#   6. Entrenamiento final con mejores parámetros.
#   7. Evaluación en test: métricas, matriz de confusión, importancia.
#   8. Exportación de resultados (csv, html, gráficos, modelo rds).
#
# Paquetes requeridos:
#   rlang, tidymodels, themis, tidyverse, gt, fs, furrr,
#   future, progressr, xgboost
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/resultados_XGB/*
# ================================================================

install.packages(c("rlang","tidymodels","themis","tidyverse","gt",
                   "fs","furrr","future","progressr","xgboost"))

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
library(workflows)     # Combina recetas y modelos
library(tune)          # Ajuste de hiperparámetros
library(dials)         # Definición de rangos de hiperparámetros
library(rsample)       # Particiones y resampling
library(tibble)        # Tablas modernas
library(tidyverse)     # Colección de paquetes tidy
library(purrr)         # Programación funcional
library(tidyr)         # Transformación de datos
library(xgboost)       # Algoritmo XGBoost

options(future.globals.maxSize = 30 * 1024^3)  # 30 GiB


# -------------------------------------
# CONFIGURACIÓN DE PARALELISMO
# -------------------------------------
plan(multisession, workers = 3)


# -------------------------------------
# CONFIGURACIÓN GENERAL
# -------------------------------------
modo_muestra <- TRUE    # TRUE = usar muestra (ensayo rápido), FALSE = usar todo
porcentaje_muestra <- 1 # % de datos de entrenamiento si modo_muestra=TRUE


# -------------------------------------
# RUTAS
# -------------------------------------
carpeta_base <- "./data/salida/algoritmos/"
archivo_train <- file.path(carpeta_base, "entrenamiento.rds")
archivo_test  <- file.path(carpeta_base, "evaluacion.rds")
carpeta_resultados_XGB <- file.path(carpeta_base, "resultados_XGB")
dir_create(carpeta_resultados_XGB)


# -------------------------------------
# CARGAR DATOS
# -------------------------------------
datos_train <- readRDS(archivo_train)
datos_test  <- readRDS(archivo_test)

if (modo_muestra) {
  set.seed(123)
  datos_train <- sample_frac(datos_train, porcentaje_muestra)
}

# Variable objetivo como factor
datos_train$Classification <- as.factor(datos_train$Classification)
datos_test$Classification  <- as.factor(datos_test$Classification)


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
# PREPROCESAMIENTO
# -------------------------------------
receta_xgb <- recipe(Classification ~ ., data = datos_train) %>%
  step_zv(all_predictors()) %>%                # Elimina predictores constantes
  step_impute_median(all_numeric_predictors()) # Imputa NA con mediana


# -------------------------------------
# MODELO Y WORKFLOW
# -------------------------------------
# Definimos XGBoost con hiperparámetros a tunear:
#   - tree_depth: profundidad máxima de cada árbol
#   - learn_rate: tasa de aprendizaje (shrinkage)
#   - loss_reduction: min. ganancia en reducción de pérdida
#   - sample_size: proporción de muestras por árbol
#   - mtry: nº de predictores por split
modelo_xgb <- boost_tree(
  trees = 300,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost", num_threads = 1) %>%
  set_mode("classification")

workflow_xgb <- workflow() %>%
  add_recipe(receta_xgb) %>%
  add_model(modelo_xgb)


# -------------------------------------
# GRILLA DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
grid_xgb <- grid_random(
  tree_depth(range = c(3L, 10L)),    # profundidad
  learn_rate(range = c(-2.5, -1.0)), # tasa aprendizaje (10^x)
  loss_reduction(range = c(-3, 0)),  # min. reducción pérdida (10^x)
  sample_prop(range = c(0.5, 1.0)),  # proporción de datos por árbol
  mtry(range = c(1L, 10L)),          # nº predictores por split
  size = 40
)


# -------------------------------------
# AJUSTE DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
rs_xgb <- bootstraps(datos_train, times = 5, strata = Classification)

set.seed(123)
handlers(global = TRUE)
with_progress({
  tuning_xgb <- tune_grid(
    workflow_xgb,
    resamples = rs_xgb,
    grid = grid_xgb,
    metrics = metric_set(accuracy, bal_accuracy, kap, precision, recall, f_meas),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
})

saveRDS(tuning_xgb, file.path(carpeta_resultados_XGB, "tuning_xgb.rds"))


# -------------------------------------
# ENTRENAMIENTO FINAL
# -------------------------------------
mejores_parametros_xgb <- select_best(tuning_xgb, metric = "bal_accuracy")
workflow_xgb_final     <- finalize_workflow(workflow_xgb, mejores_parametros_xgb)

modelo_final_xgb <- fit(workflow_xgb_final, data = datos_train)


# -------------------------------------
# PREDICCIONES SOBRE TEST
# -------------------------------------
predicciones_xgb <- predict(modelo_final_xgb, new_data = datos_test) %>%
  bind_cols(datos_test %>% select(Classification))


# -------------------------------------
# MÉTRICAS DE EVALUACIÓN
# -------------------------------------
metricas_xgb <- predicciones_xgb %>%
  metrics(truth = Classification, estimate = .pred_class) %>%
  bind_rows(
    conf_mat(predicciones_xgb, truth = Classification, estimate = .pred_class) %>%
      summary()
  )


# -------------------------------------
# GUARDAR RESULTADOS
# -------------------------------------
write_csv(predicciones_xgb, file.path(carpeta_resultados_XGB, "predicciones_XGB.csv"))
write_csv(metricas_xgb,    file.path(carpeta_resultados_XGB, "metricas_XGB.csv"))

tabla_metricas_xgb <- metricas_xgb %>%
  select(.metric, .estimate) %>%
  mutate(.estimate = round(.estimate, 3)) %>%
  gt() %>%
  tab_header(title = "Métricas de desempeño - XGBoost")

gtsave(tabla_metricas_xgb, file.path(carpeta_resultados_XGB, "tabla_metricas_XGB.html"))


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

predicciones_xgb <- predicciones_xgb %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class), levels = names(nombres_clases))
  )

matriz_conf_xgb <- predicciones_xgb %>%
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

grafico_conf_xgb <- ggplot(matriz_conf_xgb, aes(x = Prediction, y = Truth, fill = prop)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% Relativo") +
  scale_y_discrete(limits = rev(levels(matriz_conf_xgb$Truth))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Confusión Normalizada - XGBoost",
    caption = "Valores relativos por clase real (por fila)",
    x = "Clase predicha", y = "Clase real"
  )

ggsave(file.path(carpeta_resultados_XGB, "matriz_confusion_XGB_normalizada.jpg"),
       grafico_conf_xgb, width = 10, height = 7, dpi = 1200)


# -------------------------------------
# IMPORTANCIA DE VARIABLES
# -------------------------------------
modelo_boost <- extract_fit_engine(modelo_final_xgb)
importancia_xgb <- xgb.importance(model = modelo_boost)

# Armonizar nombres si vienen en minúscula
if (!"Feature" %in% names(importancia_xgb)) {
  importancia_xgb <- importancia_xgb %>% rename(Feature = feature, Gain = gain)
}

grafico_importancia_xgb <- ggplot(importancia_xgb, aes(x = reorder(Feature, Gain),
                                                       y = Gain, fill = Gain)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Importancia de las Variables - XGBoost",
    x = "Variable", y = "Importancia (por ganancia)",
    caption = "El número ( ) indica el radio usado en la métrica"
  )

ggsave(file.path(carpeta_resultados_XGB, "importancia_variables_XGB.jpg"),
       grafico_importancia_xgb, width = 10, height = 6, dpi = 1200)

saveRDS(modelo_final_xgb, file.path(carpeta_resultados_XGB, "modelo_final_xgb.rds"))
write_csv(importancia_xgb, file.path(carpeta_resultados_XGB, "importancia_variables_XGB.csv"))


# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ XGBoost entrenado, evaluado y resultados exportados:\n")
cat("📁 modelo_final_xgb.rds\n📁 predicciones_XGB.csv\n📁 metricas_XGB.csv\n📁 tabla_metricas_XGB.html\n📁 matriz_confusion_XGB_normalizada.jpg\n📁 importancia_variables_XGB.jpg\n📁 importancia_variables_XGB.csv\n\n")

print(metricas_xgb)
