# ================================================================
# SCRIPT: Entrenamiento y evaluación de Árbol de Decisión (rpart)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script entrena y evalúa un modelo de clasificación basado
#   en Árboles de Decisión (rpart), usando el framework tidymodels.
#
# Flujo:
#   1. Lectura de datos de entrenamiento y evaluación (.rds).
#   2. Selección de métricas definitivas.
#   3. Preprocesamiento: eliminación de var. constantes + imputación.
#   4. Definición del modelo de Árbol de Decisión.
#   5. Tuning de hiperparámetros con bootstraps estratificados.
#   6. Entrenamiento final con mejores parámetros.
#   7. Evaluación en test: métricas, matriz de confusión, importancia.
#   8. Exportación de resultados (csv, html, gráficos, modelo rds).
#
# Paquetes requeridos:
#   rlang, tidymodels, themis, tidyverse, gt, fs, furrr,
#   future, progressr, rpart, rpart.plot
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/resultados_DT/*
# ================================================================

install.packages(c("rlang","tidymodels","themis","tidyverse","gt","fs","furrr","future","progressr","rpart","rpart.plot"))

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
library(rpart)         # Implementación de árboles CART
library(tibble)        # Tablas modernas
library(tidyverse)     # Colección de paquetes tidy
library(rpart.plot)    # Visualización de árboles
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
modo_muestra <- TRUE   # TRUE = usar muestra (ensayo rápido), FALSE = usar todo
porcentaje_muestra <- 1 # % de datos de entrenamiento a usar si modo_muestra=TRUE


# -------------------------------------
# RUTAS
# -------------------------------------
carpeta_base <- "./data/salida/algoritmos/"
archivo_train <- file.path(carpeta_base, "entrenamiento.rds")
archivo_test  <- file.path(carpeta_base, "evaluacion.rds")
carpeta_resultados_DT <- file.path(carpeta_base, "resultados_DT")
dir_create(carpeta_resultados_DT)


# -------------------------------------
# CARGAR DATOS
# -------------------------------------
datos_train <- readRDS(archivo_train)
datos_test  <- readRDS(archivo_test)

# Submuestreo opcional para pruebas rápidas
if (modo_muestra) {
  set.seed(123)
  datos_train <- sample_frac(datos_train, porcentaje_muestra)
}

# Convertir la variable objetivo a factor
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

gc()  # Liberar memoria


# -------------------------------------
# PREPROCESAMIENTO
# -------------------------------------
# step_zv: elimina variables con varianza cero (constantes)
# step_impute_median: imputa valores NA con la mediana
receta_dt <- recipe(Classification ~ ., data = datos_train) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())


# -------------------------------------
# MODELO Y WORKFLOW
# -------------------------------------
# Definimos un árbol de decisión con hiperparámetros a tunear:
#   - tree_depth: profundidad máxima del árbol
#   - min_n: número mínimo de observaciones en un nodo
#   - cost_complexity: penalización por complejidad (poda)
modelo_dt <- decision_tree(
  tree_depth = tune(),
  min_n = tune(),
  cost_complexity = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Workflow = Receta (preprocesamiento) + Modelo
workflow_dt <- workflow() %>%
  add_recipe(receta_dt) %>%
  add_model(modelo_dt)


# -------------------------------------
# GRILLA DE HIPERPARÁMETROS
# -------------------------------------
set.seed(123)
grid_dt <- grid_random(
  tree_depth(range = c(2L, 15L)),   # Profundidad de árbol
  min_n(range = c(1L, 30L)),        # Mínimo de obs. por nodo
  cost_complexity(range = c(-4, 0)),# Poda: log10(lambda)
  size = 40                         # Nº de combinaciones aleatorias
)


# -------------------------------------
# AJUSTE DE HIPERPARÁMETROS (TUNING)
# -------------------------------------
# bootstraps: re-muestreo con reemplazo, estratificado por clase
# tune_grid: evalúa el modelo en cada combinación de parámetros
set.seed(123)
rs_dt <- bootstraps(datos_train, times = 5, strata = Classification)

set.seed(123)
handlers(global = TRUE)
with_progress({
  tuning_dt <- tune_grid(
    workflow_dt,
    resamples = rs_dt,
    grid = grid_dt,
    metrics = metric_set(accuracy, bal_accuracy, kap, precision, recall, f_meas), # métricas balanceadas
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
})

# Guardamos resultados del tuning
saveRDS(tuning_dt, file.path(carpeta_resultados_DT, "tuning_dt.rds"))


# -------------------------------------
# ENTRENAMIENTO FINAL
# -------------------------------------
# Seleccionamos los mejores hiperparámetros según balanced accuracy
mejores_parametros <- select_best(tuning_dt, metric = "bal_accuracy")
workflow_dt_final  <- finalize_workflow(workflow_dt, mejores_parametros)

# Entrenamos modelo definitivo con todos los datos de entrenamiento
modelo_final_dt <- workflow_dt_final %>% fit(data = datos_train)


# -------------------------------------
# PREDICCIONES SOBRE TEST
# -------------------------------------
predicciones_dt <- predict(modelo_final_dt, new_data = datos_test) %>%
  bind_cols(datos_test %>% select(Classification))


# -------------------------------------
# MÉTRICAS DE EVALUACIÓN
# -------------------------------------
metricas_dt <- predicciones_dt %>%
  metrics(truth = Classification, estimate = .pred_class) %>%
  bind_rows(
    predicciones_dt %>%
      conf_mat(truth = Classification, estimate = .pred_class) %>%
      summary()
  )


# -------------------------------------
# GUARDAR RESULTADOS
# -------------------------------------
write_csv(predicciones_dt, file.path(carpeta_resultados_DT, "predicciones_DT.csv"))
write_csv(metricas_dt,    file.path(carpeta_resultados_DT, "metricas_DT.csv"))

# Tabla HTML con métricas
tabla_metricas <- metricas_dt %>%
  select(.metric, .estimate) %>%
  mutate(.estimate = round(.estimate, 3)) %>%
  gt() %>%
  tab_header(title = "Métricas de desempeño - Árbol de Decisión")

gtsave(tabla_metricas, file.path(carpeta_resultados_DT, "tabla_metricas_DT.html"))


# -------------------------------------
# MATRIZ DE CONFUSIÓN NORMALIZADA
# -------------------------------------
# Se calculan proporciones relativas por clase real (fila)
# Esto permite analizar sensibilidad clase a clase
nombres_clases <- c(
  "2"  = "Suelo",
  "6"  = "Edificio",
  "7"  = "Ruido",
  "9"  = "Agua",
  "21" = "Aerogenerador",
  "22" = "Torre eléctrica",
  "23" = "Cable eléctrico",
  "24" = "Vías",
  "25" = "Puente",
  "26" = "Placas solares",
  "27" = "Ferrocarril",
  "28" = "Invernadero",
  "29" = "Vegetación"
)

predicciones_dt <- predicciones_dt %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class), levels = names(nombres_clases))
  )

matriz_conf <- predicciones_dt %>%
  conf_mat(truth = Classification, estimate = .pred_class) %>%
  .$table %>%
  as_tibble() %>%
  group_by(Truth) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Truth      = factor(as.character(Truth), levels = names(nombres_clases), labels = nombres_clases),
    Prediction = factor(as.character(Prediction), levels = names(nombres_clases), labels = nombres_clases)
  ) %>%
  complete(Truth, Prediction, fill = list(n = 0, prop = 0))

# Gráfico
grafico_conf_relativa <- ggplot(matriz_conf, aes(x = Prediction, y = Truth, fill = prop)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% Relativo") +
  scale_y_discrete(limits = rev(levels(matriz_conf$Truth))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Confusión Normalizada - Árbol de Decisión",
    caption = "Valores relativos por clase real (por fila)",
    x = "Clase predicha", y = "Clase real"
  )

ggsave(file.path(carpeta_resultados_DT, "matriz_confusion_DT_normalizada.jpg"),
       grafico_conf_relativa, width = 10, height = 7, dpi = 1200)


# -------------------------------------
# IMPORTANCIA DE VARIABLES
# -------------------------------------
# Se obtiene desde el motor rpart: mide qué variables reducen más la impureza
modelo_rpart <- extract_fit_engine(modelo_final_dt)

importancia_df <- modelo_rpart$variable.importance %>%
  enframe(name = "Variable", value = "Importancia")

grafico_importancia <- ggplot(importancia_df, aes(x = reorder(Variable, Importancia), y = Importancia, fill = Importancia)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Importancia de las Variables - Árbol de Decisión",
    x = "Variable", y = "Importancia",
    caption = "El número ( ) indica el radio usado en la métrica"
  )

ggsave(file.path(carpeta_resultados_DT, "importancia_variables_DT.jpg"),
       grafico_importancia, width = 10, height = 6, dpi = 1200)

# Guardamos también tabla y modelo
saveRDS(modelo_final_dt, file.path(carpeta_resultados_DT, "modelo_final_dt.rds"))
write_csv(importancia_df, file.path(carpeta_resultados_DT, "importancia_variables_DT.csv"))


# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ Árbol de Decisión entrenado, evaluado y resultados exportados:\n")
cat("📁 modelo_final_dt.rds\n📁 predicciones_DT.csv\n📁 metricas_DT.csv\n📁 tabla_metricas_DT.html\n📁 matriz_confusion_DT_normalizada.jpg\n📁 importancia_variables_DT.jpg\n📁 importancia_variables_DT.csv\n\n")

print(metricas_dt)
