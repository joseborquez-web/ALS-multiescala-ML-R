# ================================================================
# SCRIPT: Limpieza de valores problemáticos en archivos CSV
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script procesa múltiples archivos CSV y reemplaza valores
#   problemáticos (como "--", "null", "N/A", "Error", etc.) por NA.
#   Posteriormente, convierte las columnas a numéricas y guarda los
#   resultados en los mismos archivos con "NaN" como valor nulo.
#
# Paquetes requeridos:
#   - dplyr
#   - readr
#   - furrr
#   - future
#
# Configuración de entrada/salida:
#   - Los CSV deben estar en la carpeta definida en `carpeta_raiz`.
#   - El script sobrescribe los mismos archivos con los valores limpios.
#
# Uso en un repositorio compartido:
#   - Colocar CSV en: ./data/entrada/CSV_con_Z_normalizada
#   - Ejecutar el script para limpiar todos los archivos de la carpeta.
# ================================================================


# -----------------------------
# CARGA DE LIBRERÍAS
# -----------------------------
library(dplyr)   # Manipulación de datos
library(readr)   # Lectura y escritura de CSV
library(furrr)   # Map paralelo
library(future)  # Paralelismo


# -----------------------------
# CONFIGURACIÓN DE PARALELISMO
# -----------------------------
plan(multisession, workers = 10)  # Número de núcleos a usar


# -----------------------------
# RUTA Y ARCHIVOS
# -----------------------------
# Carpeta raíz donde están los CSV
# ⚠️ Ajustar según la estructura del repositorio
carpeta_raiz <- "./data/entrada/CSV_con_Z_normalizada"

# Listar todos los archivos CSV (incluye subcarpetas si existen)
archivos <- list.files(path = carpeta_raiz, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)


# -----------------------------
# VALORES PROBLEMÁTICOS A IMPUTAR COMO NA
# -----------------------------
valores_problema <- c("--", "null", "NULL", "N/A", "#VALUE!", "Error", "ERROR", 
                      "T", "F", "TRUE", "FALSE", "true", "false", "")


# -----------------------------
# FUNCIÓN DE LIMPIEZA POR ARCHIVO
# -----------------------------
procesar_csv <- function(archivo) {
  # Leer CSV como texto completo para evitar errores de tipos
  df <- tryCatch(
    read_csv(archivo, col_types = cols(.default = "c"), show_col_types = FALSE),
    error = function(e) return(NULL)
  )
  
  if (is.null(df)) {
    cat("❌ Error al leer:", basename(archivo), "\n")
    return(NULL)
  }
  
  # Reemplazar valores problemáticos por NA y convertir a numérico
  df <- df %>%
    mutate(across(everything(), ~ {
      col <- trimws(.)                             # Eliminar espacios
      col[col %in% valores_problema] <- NA         # Reemplazar valores definidos
      suppressWarnings(as.numeric(col))            # Intentar convertir a numérico
    }))
  
  # Sobrescribir archivo CSV con NaN en celdas vacías
  write_csv(df, archivo, na = "NaN")
  cat("✅ Limpio y guardado con NaN:", basename(archivo), "\n")
  return(NULL)
}


# -----------------------------
# EJECUCIÓN EN PARALELO
# -----------------------------
cat("🚀 Procesando en paralelo con 10 núcleos...\n")
invisible(future_map(archivos, procesar_csv))
cat("✅ Limpieza completa.\n")
