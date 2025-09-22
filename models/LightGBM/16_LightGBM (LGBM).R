# ================================================================
# SCRIPT: Entrenamiento y evaluación de LightGBM (bonsai/lightgbm)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script entrena y evalúa un modelo de clasificación basado
#   en LightGBM, implementado con bonsai/tidymodels.
#
# Flujo:
#   1. Lectura de datos de entrenamiento y evaluación (.rds).
#   2. Selección de métricas definitivas.
#   3. Preprocesamiento: eliminación de var. constantes + imputación.
#   4. Definición del modelo LightGBM.
#   5. Tuning de hiperparámetros con bootstraps estratificados.
#   6. Entrenamiento final con mejores parámetros.
#   7. Evaluación en test: métricas, matriz de confusión, importancia.
#   8. Exportación de resultados (csv, html, gráficos, modelo rds).
#
# Paquetes requeridos:
#   rlang, tidymodels, themis, tidyverse, gt, fs, furrr, future,
#   progressr, lightgbm, bonsai, vip
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/resultados_LGBM/*
# ================================================================

install.packages(c("rlang","tidymodels","themis","tidyverse","gt",
                   "fs","furrr","future","progressr","lightgbm",
                   "bonsai","vip"))

# -------------------------------------
# LIBRERÍAS NECESARIAS
# -------------------------------------
library(rlang)         # Utilidades de programación
library(tidymodels)    # Framework de ML
library(themis)        # Métodos de balanceo de clases
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
library(rsample)       # Particiones de datos y resampling
library(tibble)        # Tablas modernas
library(tidyverse)     # Colección de paquetes tidy
library(purrr)         # Programación funcional
library(tidyr)         # Transformación de datos
library(bonsai)        # Modelos adicionales (LightGBM, XGBoost, CatBoost)
library(vip)           # Visualización de importancia de variables

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
carpeta_resultados_LGBM <- file.path(carpeta_base, "resultados_LGBM")
dir_create(carpeta_resultados_LGBM)


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
receta_lgbm <- recipe(Classification ~ ., data = datos_train) %>%
  step_zv(all_predictors()) %>%                # Elimina predictores constantes
  step_impute_median(all_numeric_predictors()) # Imputa NA con mediana


# -------------------------------------
# MODELO Y WORKFLOW
# -------------------------------------
# Definimos LightGBM con hiperparámetros a tunear:
#   - tree_depth: profundidad máxima de cada árbol
#   - learn_rate: tasa de aprendizaje (shrinkage)
#   - loss_reduction: min. ganancia en reducción de pérdida para dividir
#   - mtry: nº de predictores muestreados por split
#   - trees: nº de árboles (fijo en 300)
modelo_lgbm <- boost_tree(
  trees = 300,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  mtry = tune()
) %>%
  set_engine("lightgbm", num_threads = 1) %>%
  set_mode("classification")

workflow_lgbm <- workflow() %>%
  add_recipe(receta_lgbm) %>%
  add_model(modelo_lgbm)


# -------------------------------------
# GRILLA DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
grid_lgbm <- grid_random(
  tree_depth(range = c(3L, 12L)),   # profundidad
  learn_rate(range = c(-3.5, -1.5)),# tasa de aprendizaje (10^x)
  loss_reduction(range = c(-3, 0)), # min. reducción pérdida (10^x)
  mtry(range = c(2L, 7L)),          # nº predictores por split
  size = 40
)


# -------------------------------------
# AJUSTE DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
rs_lgbm <- bootstraps(datos_train, times = 5, strata = Classification)

set.seed(123)
handlers(global = TRUE)
with_progress({
  tuning_lgbm <- tune_grid(
    workflow_lgbm,
    resamples = rs_lgbm,
    grid = grid_lgbm,
    metrics = metric_set(accuracy, bal_accuracy, kap, precision, recall, f_meas),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
})

saveRDS(tuning_lgbm, file.path(carpeta_resultados_LGBM, "tuning_lgbm.rds"))


# -------------------------------------
# ENTRENAMIENTO FINAL
# -------------------------------------
mejores_parametros_lgbm <- select_best(tuning_lgbm, metric = "bal_accuracy")
workflow_lgbm_final     <- finalize_workflow(workflow_lgbm, mejores_parametros_lgbm)

modelo_final_lgbm <- workflow_lgbm_final %>% fit(data = datos_train)


# -------------------------------------
# PREDICCIONES SOBRE TEST
# -------------------------------------
predicciones_lgbm <- predict(modelo_final_lgbm, new_data = datos_test) %>%
  bind_cols(datos_test %>% select(Classification))


# -------------------------------------
# MÉTRICAS DE EVALUACIÓN
# -------------------------------------
metricas_lgbm <- predicciones_lgbm %>%
  metrics(truth = Classification, estimate = .pred_class) %>%
  bind_rows(
    predicciones_lgbm %>%
      conf_mat(truth = Classification, estimate = .pred_class) %>%
      summary()
  )


# -------------------------------------
# GUARDAR RESULTADOS
# -------------------------------------
write_csv(predicciones_lgbm, file.path(carpeta_resultados_LGBM, "predicciones_LGBM.csv"))
write_csv(metricas_lgbm,    file.path(carpeta_resultados_LGBM, "metricas_LGBM.csv"))

tabla_metricas_lgbm <- metricas_lgbm %>%
  select(.metric, .estimate) %>%
  mutate(.estimate = round(.estimate, 3)) %>%
  gt() %>%
  tab_header(title = "Métricas de desempeño - LightGBM")

gtsave(tabla_metricas_lgbm, file.path(carpeta_resultados_LGBM, "tabla_metricas_LGBM.html"))


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

predicciones_lgbm <- predicciones_lgbm %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class), levels = names(nombres_clases))
  )

matriz_conf_lgbm <- predicciones_lgbm %>%
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

grafico_conf_lgbm <- ggplot(matriz_conf_lgbm, aes(x = Prediction, y = Truth, fill = prop)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% Relativo") +
  scale_y_discrete(limits = rev(levels(matriz_conf_lgbm$Truth))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Confusión Normalizada - LightGBM",
    caption = "Valores relativos por clase real (por fila)",
    x = "Clase predicha", y = "Clase real"
  )

ggsave(file.path(carpeta_resultados_LGBM, "matriz_confusion_LGBM_normalizada.jpg"),
       grafico_conf_lgbm, width = 10, height = 7, dpi = 1200)


# -------------------------------------
# IMPORTANCIA DE VARIABLES
# -------------------------------------
importancia_df_lgbm <- modelo_final_lgbm %>%
  extract_fit_parsnip() %>%
  vip::vi()

grafico_importancia_lgbm <- ggplot(importancia_df_lgbm, aes(x = reorder(Variable, Importance),
                                                            y = Importance, fill = Importance)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Importancia de las Variables - LightGBM",
    x = "Variable", y = "Importancia",
    caption = "El número ( ) indica el radio usado en la métrica"
  )

ggsave(file.path(carpeta_resultados_LGBM, "importancia_variables_LGBM.jpg"),
       grafico_importancia_lgbm, width = 10, height = 6, dpi = 1200)

saveRDS(modelo_final_lgbm, file.path(carpeta_resultados_LGBM, "modelo_final_lgbm.rds"))
write_csv(importancia_df_lgbm, file.path(carpeta_resultados_LGBM, "importancia_variables_LGBM.csv"))


# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ LightGBM entrenado, evaluado y resultados exportados:\n")
cat("📁 modelo_final_lgbm.rds\n📁 predicciones_LGBM.csv\n📁 metricas_LGBM.csv\n📁 tabla_metricas_LGBM.html\n📁 matriz_confusion_LGBM_normalizada.jpg\n📁 importancia_variables_LGBM.jpg\n📁 importancia_variables_LGBM.csv\n\n")

print(metricas_lgbm)
