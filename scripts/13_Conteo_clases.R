# ================================================================
# SCRIPT: Análisis y visualización de distribución de clases
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script calcula y grafica la distribución de clases en los
#   conjuntos de entrenamiento y evaluación, usando los datasets .rds.
#
# Flujo:
#   1. Leer datasets de entrenamiento y evaluación.
#   2. Contar puntos por clase y calcular porcentajes.
#   3. Exportar distribución como CSV.
#   4. Graficar y guardar comparación lado a lado.
#
# Paquetes requeridos:
#   - dplyr, readr, fs, future, furrr, ggplot2, patchwork
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
#   - Salida:
#       ./data/salida/algoritmos/distribucion_clases/*.csv
#       ./data/salida/algoritmos/distribucion_clases/grafico_distribucion_lado_a_lado.jpg
# ================================================================


# -----------------------------
# LIBRERÍAS NECESARIAS
# -----------------------------
library(dplyr)
library(readr)
library(fs)
library(future)
library(furrr)
library(ggplot2)
library(patchwork)


# -----------------------------
# CONFIGURACIÓN DE PARALELISMO
# -----------------------------
plan(multisession, workers = 3)


# -----------------------------
# RUTAS
# -----------------------------
carpeta_base   <- "./data/salida/algoritmos/"
archivo_train  <- file.path(carpeta_base, "entrenamiento.rds")
archivo_test   <- file.path(carpeta_base, "evaluacion.rds")
carpeta_clases <- file.path(carpeta_base, "distribucion_clases")
dir_create(carpeta_clases)


# -----------------------------
# DICCIONARIO DE NOMBRES DE CLASES
# -----------------------------
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


# -----------------------------
# CARGAR DATOS
# -----------------------------
datos_train <- readRDS(archivo_train)
datos_test  <- readRDS(archivo_test)

datos_train$Classification <- as.factor(datos_train$Classification)
datos_test$Classification  <- as.factor(datos_test$Classification)


# -----------------------------
# FUNCIÓN PARA CONTAR CLASES
# -----------------------------
contar_clases <- function(df, conjunto) {
  df %>%
    count(Classification, sort = TRUE) %>%
    mutate(
      porcentaje = round(100 * n / sum(n), 2),
      Conjunto = conjunto
    )
}


# -----------------------------
# CALCULAR DISTRIBUCIÓN
# -----------------------------
distrib_train <- contar_clases(datos_train, "Entrenamiento")
distrib_test  <- contar_clases(datos_test, "Evaluación")


# -----------------------------
# EXPORTAR CSVs
# -----------------------------
write_csv(distrib_train, file.path(carpeta_clases, "distribucion_clases_train.csv"))
write_csv(distrib_test,  file.path(carpeta_clases, "distribucion_clases_test.csv"))


# -----------------------------
# FUNCIÓN DE GRAFICADO
# -----------------------------
graficar_distribucion <- function(df, titulo) {
  df %>%
    mutate(Clase = factor(nombres_clases[as.character(Classification)],
                          levels = nombres_clases)) %>%
    ggplot(aes(x = Clase, y = n, fill = Clase)) +
    geom_col(width = 0.7, color = "black", show.legend = FALSE) + # ❌ sin leyenda
    scale_fill_manual(values = rep("steelblue", length(nombres_clases))) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title      = element_text(face = "bold", size = 18),
      axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
      axis.text.y     = element_text(face = "bold", size = 12),
      axis.title.x    = element_text(face = "bold", size = 14),
      axis.title.y    = element_text(face = "bold", size = 14),
      panel.grid.major.x = element_blank(),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.background    = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = titulo,
      x = "Clase",
      y = "Cantidad de puntos"
    )
}


# -----------------------------
# GENERAR Y GUARDAR GRÁFICOS
# -----------------------------
g_train <- graficar_distribucion(distrib_train, "Distribución de Clases - Entrenamiento")
g_test  <- graficar_distribucion(distrib_test,  "Distribución de Clases - Evaluación")

# Comparar lado a lado
g_combinado <- g_train + g_test + plot_layout(ncol = 2)

ggsave(file.path(carpeta_clases, "grafico_distribucion_lado_a_lado.jpg"),
       g_combinado, width = 12, height = 6, dpi = 2400, bg = "white")


# -----------------------------
# MOSTRAR POR CONSOLA
# -----------------------------
cat("\n📊 Distribución de clases - ENTRENAMIENTO:\n")
print(distrib_train)

cat("\n📊 Distribución de clases - EVALUACIÓN:\n")
print(distrib_test)


# -----------------------------
# MENSAJE FINAL
# -----------------------------
cat("\n✅ Distribución de clases analizada, exportada y graficada exitosamente.\n")
