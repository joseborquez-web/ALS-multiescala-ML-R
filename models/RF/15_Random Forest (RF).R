
# ================================================================
# SCRIPT: Entrenamiento y evaluación de Random Forest (ranger)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script entrena y evalúa un modelo de clasificación basado
#   en Random Forest, usando el motor "ranger" dentro de tidymodels.
#
# Flujo:
#   1. Lectura de datos de entrenamiento y evaluación (.rds).
#   2. Selección de métricas definitivas.
#   3. Preprocesamiento: eliminación de var. constantes + imputación.
#   4. Definición del modelo Random Forest.
#   5. Tuning de hiperparámetros con bootstraps estratificados.
#   6. Entrenamiento final con mejores parámetros.
#   7. Evaluación en test: métricas, matriz de confusión, importancia.
#   8. Exportación de resultados (csv, html, gráficos, modelo rds).
#
# Paquetes requeridos:
#   rlang, tidymodels, themis, tidyverse, gt, fs, furrr,
#   future, progressr, ranger
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/resultados_RF/*
# ================================================================

install.packages(c("rlang","tidymodels","themis","tidyverse","gt",
                   "fs","furrr","future","progressr","ranger"))

# -------------------------------------
# LIBRERÍAS NECESARIAS
# -------------------------------------
library(rlang)         # Utilidades de programación
library(tidymodels)    # Framework de ML (modelado completo)
library(themis)        # Métodos de balanceo de clases
library(readr)         # Lectura/escritura de datos
library(dplyr)         # Manipulación de datos
library(ggplot2)       # Gráficos
library(gt)            # Tablas HTML para métricas
library(yardstick)     # Métricas de evaluación
library(fs)            # Manejo de rutas y directorios
library(future)        # Backend para paralelismo
library(furrr)         # Paralelización de procesos
library(progressr)     # Barra de progreso
library(recipes)       # Preprocesamiento de variables
library(parsnip)       # Definición de modelos
library(workflows)     # Combina recetas y modelos
library(tune)          # Ajuste de hiperparámetros
library(dials)         # Definición de rangos de hiperparámetros
library(rsample)       # Particiones de datos y resampling
library(tibble)        # Tablas modernas
library(tidyverse)     # Colección de paquetes tidy
library(purrr)         # Programación funcional
library(tidyr)         # Transformación de datos

options(future.globals.maxSize = 30 * 1024^3)  # 30 GiB memoria global


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
carpeta_resultados_RF <- file.path(carpeta_base, "resultados_RF")
dir_create(carpeta_resultados_RF)


# -------------------------------------
# CARGAR DATOS
# -------------------------------------
datos_rf_train <- readRDS(archivo_train)
datos_rf_test  <- readRDS(archivo_test)

# Submuestreo opcional para pruebas rápidas
if (modo_muestra) {
  set.seed(123)
  datos_rf_train <- sample_frac(datos_rf_train, porcentaje_muestra)
}

# Convertir la variable objetivo a factor
datos_rf_train$Classification <- as.factor(datos_rf_train$Classification)
datos_rf_test$Classification  <- as.factor(datos_rf_test$Classification)


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

datos_rf_train <- datos_rf_train %>% select(all_of(variables_seleccionadas))
datos_rf_test  <- datos_rf_test  %>% select(all_of(variables_seleccionadas))

gc()  # Liberar memoria


# -------------------------------------
# PREPROCESAMIENTO
# -------------------------------------
receta_rf <- recipe(Classification ~ ., data = datos_rf_train) %>%
  step_zv(all_predictors()) %>%                # Elimina predictores constantes
  step_impute_median(all_numeric_predictors()) # Imputa NA con la mediana


# -------------------------------------
# MODELO Y WORKFLOW
# -------------------------------------
# Definimos Random Forest con hiperparámetros a tunear:
#   - mtry: nº de predictores considerados en cada split
#   - min_n: nº mínimo de observaciones por nodo
#   - trees: nº de árboles (fijo en 300 para estabilidad)
modelo_rf <- rand_forest(
  mtry = tune(),
  trees = 300,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity", num.threads = 1) %>%
  set_mode("classification")

workflow_rf <- workflow() %>%
  add_recipe(receta_rf) %>%
  add_model(modelo_rf)


# -------------------------------------
# GRILLA DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
grid_rf <- grid_random(
  mtry(range = c(1L, 7L)),   # nº de predictores a muestrear por split
  min_n(range = c(1L, 10L)), # tamaño mínimo de nodo
  size = 40                  # nº de combinaciones aleatorias
)


# -------------------------------------
# AJUSTE DE HIPERPARÁMETROS
# -------------------------------------
# bootstraps: remuestreo con reemplazo, estratificado por clase
# tune_grid: evalúa el modelo en cada combinación de parámetros
plan(sequential)  # evitar problemas de paralelismo en tuning

set.seed(123)
rs_rf <- bootstraps(datos_rf_train, times = 5, strata = Classification)

set.seed(123)
handlers(global = TRUE)
with_progress({
  tuning_rf <- tune_grid(
    workflow_rf,
    resamples = rs_rf,
    grid = grid_rf,
    metrics = metric_set(accuracy, bal_accuracy, kap, precision, recall, f_meas),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
})

plan(multisession, workers = 3)  # restaurar paralelismo

saveRDS(tuning_rf, file.path(carpeta_resultados_RF, "tuning_rf.rds"))


# -------------------------------------
# ENTRENAMIENTO FINAL
# -------------------------------------
mejores_parametros_rf <- select_best(tuning_rf, metric = "bal_accuracy")
workflow_rf_final     <- finalize_workflow(workflow_rf, mejores_parametros_rf)

modelo_final_rf <- fit(workflow_rf_final, data = datos_rf_train)


# -------------------------------------
# PREDICCIONES SOBRE TEST
# -------------------------------------
predicciones_rf <- predict(modelo_final_rf, new_data = datos_rf_test) %>%
  bind_cols(datos_rf_test %>% select(Classification))


# -------------------------------------
# MÉTRICAS DE EVALUACIÓN
# -------------------------------------
metricas_rf <- predicciones_rf %>%
  metrics(truth = Classification, estimate = .pred_class) %>%
  bind_rows(
    predicciones_rf %>%
      conf_mat(truth = Classification, estimate = .pred_class) %>%
      summary()
  )


# -------------------------------------
# GUARDAR RESULTADOS
# -------------------------------------
write_csv(predicciones_rf, file.path(carpeta_resultados_RF, "predicciones_RF.csv"))
write_csv(metricas_rf,    file.path(carpeta_resultados_RF, "metricas_RF.csv"))

tabla_metricas_rf <- metricas_rf %>%
  select(.metric, .estimate) %>%
  mutate(.estimate = round(.estimate, 3)) %>%
  gt() %>%
  tab_header(title = "Métricas de desempeño - Random Forest")

gtsave(tabla_metricas_rf, file.path(carpeta_resultados_RF, "tabla_metricas_RF.html"))


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

predicciones_rf <- predicciones_rf %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class),    levels = names(nombres_clases))
  )

matriz_conf_rf <- predicciones_rf %>%
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

grafico_conf_rf <- ggplot(matriz_conf_rf, aes(x = Prediction, y = Truth, fill = prop)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% Relativo") +
  scale_y_discrete(limits = rev(levels(matriz_conf_rf$Truth))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Confusión Normalizada - Random Forest",
    caption = "Valores relativos por clase real (por fila)",
    x = "Clase predicha", y = "Clase real"
  )

ggsave(file.path(carpeta_resultados_RF, "matriz_confusion_RF_normalizada.jpg"),
       grafico_conf_rf, width = 10, height = 7, dpi = 1200)


# -------------------------------------
# IMPORTANCIA DE VARIABLES
# -------------------------------------
modelo_ranger <- extract_fit_engine(modelo_final_rf)

importancia_df_rf <- modelo_ranger$variable.importance %>%
  enframe(name = "Variable", value = "Importancia")

grafico_importancia_rf <- ggplot(importancia_df_rf, aes(x = reorder(Variable, Importancia),
                                                        y = Importancia, fill = Importancia)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Importancia de las Variables - Random Forest",
    x = "Variable", y = "Importancia",
    caption = "El número ( ) indica el radio usado en la métrica"
  )

ggsave(file.path(carpeta_resultados_RF, "importancia_variables_RF.jpg"),
       grafico_importancia_rf, width = 10, height = 6, dpi = 1200)

saveRDS(modelo_final_rf, file.path(carpeta_resultados_RF, "modelo_final_rf.rds"))
write_csv(importancia_df_rf, file.path(carpeta_resultados_RF, "importancia_variables_RF.csv"))


# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ Random Forest entrenado, evaluado y resultados exportados:\n")
cat("📁 modelo_final_rf.rds\n📁 predicciones_RF.csv\n📁 metricas_RF.csv\n📁 tabla_metricas_RF.html\n📁 matriz_confusion_RF_normalizada.jpg\n📁 importancia_variables_RF.jpg\n📁 importancia_variables_RF.csv\n\n")

print(metricas_rf)
