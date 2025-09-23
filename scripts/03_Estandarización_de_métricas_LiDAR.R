# ================================================================
# SCRIPT: Estandarización de métricas en archivos CSV LiDAR
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script procesa archivos CSV de nubes de puntos LiDAR y:
#     1. Convierte todas las columnas a numéricas (si es posible).
#     2. Identifica qué métricas deben ser estandarizadas.
#     3. Excluye columnas geométricas y de atributos no métricos.
#     4. Aplica estandarización (media = 0, sd = 1) a las métricas seleccionadas.
#     5. Exporta los resultados a una carpeta de salida.
#
# Paquetes requeridos:
#   - dplyr
#   - readr
#   - future
#   - furrr
#
# Configuración de entrada/salida:
#   - Carpeta de entrada: ./data/entrada/CSV_con_Z_normalizada/
#   - Carpeta de salida: ./data/salida/CSV_estandarizado/
#
# Nota:
#   - Los archivos estandarizados se guardan con el mismo nombre
#     en la carpeta de salida.
# ================================================================


# -----------------------------
# CARGA DE LIBRERÍAS
# -----------------------------
library(dplyr)   # Manipulación de datos
library(readr)   # Lectura y escritura de CSV
library(future)  # Paralelismo
library(furrr)   # Map paralelo


# -----------------------------
# CONFIGURACIÓN DE NÚCLEOS
# -----------------------------
plan(multisession, workers = 10)  # Núcleos a utilizar


# -----------------------------
# CARPETAS DE ENTRADA Y SALIDA
# -----------------------------
# ⚠️ Ajustar según la estructura del repositorio
carpeta_raiz   <- "./data/entrada/CSV_con_Z_normalizada/"
carpeta_salida <- "./data/salida/CSV_estandarizado/"

# Crear carpeta de salida si no existe
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# FUNCIÓN PARA ESTANDARIZAR CSV
# -----------------------------
estandarizar_csv <- function(archivo) {
  # Leer CSV con soporte de valores faltantes
  df <- read_csv(
    archivo,
    na = c("NaN", "NA", "", " "),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )
  
  # Convertir todas las columnas posibles a numéricas
  df <- df %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  # Columnas que NO deben ser estandarizadas (geométricas/atributos)
  no_estandarizar <- c(
    "X", "Y", "Z", "gpstime",
    "ReturnNumber", "NumberOfReturns",
    "ScanDirectionFlag", "EdgeOfFlightline",
    "Classification", "UserData", "PointSourceID",
    "ScanAngleRank"
  )
  
  # Seleccionar métricas candidatas a estandarizar
  metricas_estandarizar <- df %>%
    select(where(is.numeric)) %>%
    select(-any_of(no_estandarizar[no_estandarizar %in% names(df)])) %>%
    colnames()
  
  # Aplicar estandarización si existen métricas válidas
  if (length(metricas_estandarizar) > 0) {
    df[metricas_estandarizar] <- scale(df[metricas_estandarizar])
  }
  
  # Guardar resultado en carpeta de salida
  write_csv(df, file.path(carpeta_salida, basename(archivo)), na = "NaN")
  cat("✅", basename(archivo), "-", length(metricas_estandarizar), "métricas estandarizadas\n")
}


# -----------------------------
# EJECUCIÓN EN PARALELO
# -----------------------------
archivos <- list.files(path = carpeta_raiz, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

cat("🚀 Iniciando estandarización en paralelo con 10 núcleos...\n")
future_walk(archivos, estandarizar_csv)
cat("\n📂 Archivos estandarizados guardados en:", carpeta_salida, "\n")
