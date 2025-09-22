# ================================================================
# SCRIPT: Combinación de archivos RDS en un único dataset
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script combina múltiples archivos RDS (teselas) en un único
#   archivo de entrenamiento y otro de evaluación. Los resultados
#   se guardan comprimidos (.rds con compresión xz).
#
# Flujo:
#   1. Leer todos los RDS en la carpeta correspondiente.
#   2. Combinar en un único objeto con data.table::rbindlist().
#   3. Guardar el dataset final comprimido en carpeta de resultados.
#
# Paquetes requeridos:
#   - data.table, fs, future, furrr
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/rds/entrenamiento/*.rds
#       ./data/salida/algoritmos/rds/evaluacion/*.rds
#   - Salida:
#       ./data/salida/algoritmos/entrenamiento.rds
#       ./data/salida/algoritmos/evaluacion.rds
# ================================================================


# -----------------------------
# CARGAR LIBRERÍAS
# -----------------------------
library(data.table)  # Lectura/combinar rápido
library(fs)          # Manejo de rutas/carpetas
library(future)      # Paralelismo
library(furrr)       # Map paralelo


# -----------------------------
# CONFIGURACIÓN DE PARALELISMO
# -----------------------------
plan(multisession, workers = 1)  # Ajustar según recursos disponibles


# -----------------------------
# DEFINIR RUTAS
# -----------------------------
carpeta_entrenamiento <- "./data/salida/algoritmos/rds/entrenamiento/"
carpeta_evaluacion    <- "./data/salida/algoritmos/rds/evaluacion/"
carpeta_resultados    <- "./data/salida/algoritmos"
dir_create(carpeta_resultados)


# -----------------------------
# FUNCIÓN PARA COMBINAR RDS
# -----------------------------
combinar_rds <- function(carpeta, nombre_salida) {
  archivos <- dir_ls(carpeta, regexp = "\\.rds$")
  
  if (length(archivos) == 0) stop("⚠️ No se encontraron archivos RDS en: ", carpeta)
  
  # Leer y combinar todos los RDS
  datos_list <- future_map(archivos, readRDS)
  datos_combinados <- rbindlist(datos_list, fill = TRUE)
  
  # Guardar archivo RDS final
  ruta_salida <- path(carpeta_resultados, nombre_salida)
  saveRDS(datos_combinados, ruta_salida, compress = "xz")
  
  message("✅ Archivos combinados guardados en: ", ruta_salida)
  message("- Total filas: ", nrow(datos_combinados))
  message("- Columnas: ", paste(names(datos_combinados), collapse = ", "))
  
  return(ruta_salida)
}


# -----------------------------
# COMBINAR ENTRENAMIENTO Y EVALUACIÓN
# -----------------------------
ruta_entrenamiento <- combinar_rds(
  carpeta = carpeta_entrenamiento,
  nombre_salida = "entrenamiento.rds"
)

ruta_evaluacion <- combinar_rds(
  carpeta = carpeta_evaluacion,
  nombre_salida = "evaluacion.rds"
)


# -----------------------------
# MENSAJE FINAL
# -----------------------------
message("✅ Proceso completado:")
message("- Entrenamiento: ", ruta_entrenamiento)
message("- Evaluación: ", ruta_evaluacion)
