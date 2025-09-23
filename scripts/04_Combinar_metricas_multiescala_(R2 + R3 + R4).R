# ================================================================
# SCRIPT: Combinación multiescala de métricas LiDAR (R2, R3, R4)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script combina archivos CSV correspondientes a diferentes
#   radios de cálculo (R2, R3, R4) en un único dataset multiescala.
#
#   Flujo de trabajo:
#     1. Lee los CSV de las carpetas R2, R3 y R4.
#     2. Verifica que existan los archivos correspondientes.
#     3. Extrae las métricas exclusivas de R3 y R4 (sin duplicar columnas comunes).
#     4. Une R2 + métricas de R3 + métricas de R4.
#     5. Verifica duplicados y genera un log detallado.
#     6. Exporta los CSV combinados a una carpeta de salida.
#
# Paquetes requeridos:
#   - dplyr
#   - readr
#   - future
#   - future.apply
#
# Configuración de entrada/salida:
#   - Carpeta de entrada R2: ./data/entrada/R2/CSV_estandarizado
#   - Carpeta de entrada R3: ./data/entrada/R3/CSV_estandarizado
#   - Carpeta de entrada R4: ./data/entrada/R4/CSV_estandarizado
#   - Carpeta de salida:     ./data/salida/multiescala_combinado
#
# ================================================================


# -----------------------------
# LIBRERÍAS NECESARIAS
# -----------------------------
library(dplyr)
library(readr)
library(future)
library(future.apply)


# -----------------------------
# CONFIGURAR PARALELISMO (10 NÚCLEOS)
# -----------------------------
plan(multisession, workers = 10)


# -----------------------------
# RUTAS DE ENTRADA Y SALIDA
# -----------------------------
carpeta_r2      <- "./data/entrada/R2/CSV_estandarizado"
carpeta_r3      <- "./data/entrada/R3/CSV_estandarizado"
carpeta_r4      <- "./data/entrada/R4/CSV_estandarizado"
carpeta_salida  <- "./data/salida/multiescala_combinado"

# Crear carpeta de salida si no existe
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

# Archivo de log
log_resultados <- file.path(carpeta_salida, "log_combinacion.csv")


# -----------------------------
# LISTA DE ARCHIVOS A PROCESAR
# -----------------------------
# Se asume que los nombres de los archivos coinciden entre R2, R3 y R4
teselas <- list.files(carpeta_r2, pattern = "\\.csv$", full.names = FALSE)


# -----------------------------
# FUNCIÓN DE COMBINACIÓN MULTIESCALA
# -----------------------------
combinar_multiescala <- function(nombre_archivo) {
  archivo_r2 <- file.path(carpeta_r2, nombre_archivo)
  archivo_r3 <- file.path(carpeta_r3, nombre_archivo)
  archivo_r4 <- file.path(carpeta_r4, nombre_archivo)
  
  # Validar existencia de R3 y R4
  if (!file.exists(archivo_r3) || !file.exists(archivo_r4)) {
    mensaje <- "R3 o R4 no existe"
    cat("⚠️ Faltante:", nombre_archivo, "\n")
    return(data.frame(archivo = nombre_archivo, estado = "faltante", mensaje = mensaje, columnas_combinadas = NA))
  }
  
  tryCatch({
    # Leer datasets y convertir a data.frame para evitar conflictos
    r2 <- as.data.frame(read_csv(archivo_r2, na = "NaN", show_col_types = FALSE))
    r3 <- as.data.frame(read_csv(archivo_r3, na = "NaN", show_col_types = FALSE))
    r4 <- as.data.frame(read_csv(archivo_r4, na = "NaN", show_col_types = FALSE))
    
    # Columnas comunes que no deben duplicarse
    comunes <- c("X", "Y", "Z", "gpstime", "Intensity", "ReturnNumber", "NumberOfReturns",
                 "ScanDirectionFlag", "EdgeOfFlightline", "Classification",
                 "UserData", "PointSourceID", "ScanAngleRank", "R", "G", "B", "Z_normalizada")
    
    # Extraer solo métricas de R3 y R4
    r3_metr <- dplyr::select(r3, -any_of(comunes))
    r4_metr <- dplyr::select(r4, -any_of(comunes))
    
    # Unir datasets: R2 + métricas de R3 + métricas de R4
    combinado <- dplyr::bind_cols(r2, r3_metr, r4_metr)
    
    # Verificar columnas duplicadas
    col_duplicadas <- names(combinado)[duplicated(names(combinado))]
    if (length(col_duplicadas) > 0) {
      mensaje <- paste("Columnas duplicadas:", paste(col_duplicadas, collapse = ", "))
      cat("❌ Error:", nombre_archivo, "-", mensaje, "\n")
      return(data.frame(archivo = nombre_archivo, estado = "error", mensaje = mensaje, columnas_combinadas = NA))
    }
    
    # Guardar archivo combinado
    write_csv(combinado, file.path(carpeta_salida, nombre_archivo), na = "NaN")
    cat("✅ Combinado:", nombre_archivo, "\n")
    
    return(data.frame(
      archivo = nombre_archivo,
      estado = "combinado",
      mensaje = "OK",
      columnas_combinadas = ncol(combinado)
    ))
    
  }, error = function(e) {
    cat("❌ Error:", nombre_archivo, "-", e$message, "\n")
    return(data.frame(archivo = nombre_archivo, estado = "error", mensaje = e$message, columnas_combinadas = NA))
  })
}


# -----------------------------
# EJECUTAR EN PARALELO Y GUARDAR LOG
# -----------------------------
cat("🚀 Iniciando combinación en paralelo con 10 núcleos...\n")
log <- future_lapply(teselas, combinar_multiescala, future.globals = TRUE)

if (length(log) > 0) {
  log_validos <- log[which(sapply(log, function(x) !is.null(x) && is.data.frame(x)))]
  
  if (length(log_validos) > 0) {
    log_df <- do.call(rbind, log_validos)
    write_csv(log_df, log_resultados)
    cat("\n📦 Proceso completado. Log guardado en:", log_resultados, "\n")
    print(table(log_df$estado))
    cat("\n📝 Primeros registros del log:\n")
    print(head(log_df, 5))
  } else {
    cat("\n⚠️ No se generaron registros válidos en el log.\n")
  }
} else {
  cat("\n⚠️ El objeto 'log' está vacío. Verifica si hay archivos en R2.\n")
}
