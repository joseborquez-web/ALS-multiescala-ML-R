# ================================================================
# SCRIPT: Conversión de teselas CSV a formato RDS
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Convierte las teselas de Entrenamiento y Evaluación (en CSV)
#   a formato RDS comprimido (.rds con compresión xz) para optimizar
#   su uso en flujos de Machine Learning.
#
#   Flujo:
#     1. Lee cada CSV (entrenamiento y evaluación).
#     2. Agrega columna con nombre de la tesela.
#     3. Exporta el resultado como archivo RDS.
#     4. Separa archivos en subcarpetas de salida.
#
# Paquetes requeridos:
#   - readr, dplyr, furrr, future, fs, progressr
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/entrenamiento/*.csv
#       ./data/salida/algoritmos/evaluacion/*.csv
#   - Salida:
#       ./data/salida/algoritmos/rds/entrenamiento/*.rds
#       ./data/salida/algoritmos/rds/evaluacion/*.rds
# ================================================================


# -----------------------------
# LIBRERÍAS
# -----------------------------
library(readr)
library(dplyr)
library(furrr)      # Paralelización
library(fs)         # Manejo de carpetas
library(progressr)  # Barra de progreso


# -----------------------------
# CONFIGURAR PARALELISMO
# -----------------------------
plan(multisession, workers = 20)  # Se recomienda ajustar según recursos


# -----------------------------
# RUTAS
# -----------------------------
carpeta_entrenamiento <- "./data/salida/algoritmos/entrenamiento/"
carpeta_evaluacion    <- "./data/salida/algoritmos/evaluacion/"
carpeta_salida        <- "./data/salida/algoritmos/"

# Subcarpetas de salida
carpeta_rds_entrenamiento <- file.path(carpeta_salida, "rds", "entrenamiento")
carpeta_rds_evaluacion    <- file.path(carpeta_salida, "rds", "evaluacion")

# Crear carpetas si no existen
dir_create(carpeta_rds_entrenamiento)
dir_create(carpeta_rds_evaluacion)


# -----------------------------
# FUNCIÓN DE CONVERSIÓN A RDS
# -----------------------------
convertir_a_rds <- function(path, tipo) {
  tryCatch({
    # Leer archivo CSV y agregar columna con nombre de tesela
    datos <- read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
      mutate(Tesela = basename(path))
    
    # Nombre de salida en RDS
    archivo_rds <- gsub(".csv", ".rds", basename(path))
    
    # Ruta de salida según tipo
    if (tipo == "entrenamiento") {
      ruta_rds <- file.path(carpeta_rds_entrenamiento, archivo_rds)
    } else {
      ruta_rds <- file.path(carpeta_rds_evaluacion, archivo_rds)
    }
    
    # Guardar en formato RDS con compresión
    saveRDS(datos, ruta_rds, compress = "xz")
    message(paste("✔️ Tesela guardada como RDS:", ruta_rds))
    
  }, error = function(e) {
    message(paste("❌ Error procesando:", path, "-", e$message))
  })
}


# -----------------------------
# LISTAR ARCHIVOS CSV
# -----------------------------
archivos_train_csv <- dir_ls(carpeta_entrenamiento, regexp = "\\.csv$")
archivos_test_csv  <- dir_ls(carpeta_evaluacion, regexp = "\\.csv$")


# -----------------------------
# PROCESAMIENTO CON PROGRESO
# -----------------------------
if (length(archivos_train_csv) == 0) {
  message("❌ No se encontraron archivos CSV en la carpeta de entrenamiento.")
} else if (length(archivos_test_csv) == 0) {
  message("❌ No se encontraron archivos CSV en la carpeta de evaluación.")
} else {
  with_progress({
    # Barra de progreso: suma total de archivos
    p <- progressor(steps = length(archivos_train_csv) + length(archivos_test_csv))
    
    # Procesar entrenamiento
    future_map(archivos_train_csv, function(path) {
      convertir_a_rds(path, tipo = "entrenamiento")
      p()
    })
    
    # Procesar evaluación
    future_map(archivos_test_csv, function(path) {
      convertir_a_rds(path, tipo = "evaluacion")
      p()
    })
  })
}
