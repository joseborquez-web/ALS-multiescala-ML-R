# ================================================================
# SCRIPT: Recalculo de Z_normalizada en nubes de puntos LiDAR
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script procesa archivos CSV con información LiDAR para:
#     1. Revisar si contienen suficientes puntos de terreno.
#     2. Filtrar clases de terreno específicas.
#     3. Calcular un MDT (Modelo Digital del Terreno).
#     4. Recalcular la variable Z_normalizada (altura sobre el terreno).
#     5. Sobrescribir el archivo CSV con la nueva columna.
#     6. Generar un log con el estado de cada archivo procesado.
#
# Paquetes requeridos:
#   - data.table
#   - lidR
#   - raster
#   - future
#   - furrr
#   - future.apply
#
# Configuración de entrada/salida:
#   - Los archivos CSV deben estar en la carpeta definida en `carpeta_csv`.
#   - El script guarda un archivo de log llamado "log_recalculo_Z.csv"
#     dentro de esa misma carpeta.
#
# Uso en un repositorio compartido:
#   - Colocar los CSV en: ./data/entrada/
#   - El log se generará en la misma carpeta.
# ================================================================


# -----------------------------
# CARGA DE LIBRERÍAS
# -----------------------------
library(data.table)   # Lectura y escritura rápida de CSV
library(lidR)         # Manejo de nubes de puntos LiDAR
library(raster)       # Soporte de operaciones raster
library(future)       # Paralelismo
library(furrr)        # Map paralelo
library(future.apply) # Paralelismo aplicado a listas


# -----------------------------
# CONFIGURAR PARALELISMO
# -----------------------------
plan(multisession, workers = 10)  # Número de núcleos a utilizar


# -----------------------------
# CONFIGURACIÓN GENERAL
# -----------------------------
clases_terreno <- c(2, 24, 27)       # Clases consideradas como "terreno"
umbral_minimo_puntos <- 100          # Número mínimo de puntos de terreno

# Carpeta de entrada: donde se almacenan los CSV de nubes de puntos
# ⚠️ Ajustar la ruta según la estructura del repositorio
carpeta_csv <- "./data/entrada/CSV_con_Z_normalizada"

# Listar todos los archivos CSV en la carpeta
archivos_csv <- list.files(carpeta_csv, pattern = "\\.csv$", full.names = TRUE)


# -----------------------------
# FUNCIÓN DE REVISIÓN Y PROCESAMIENTO
# -----------------------------
procesar_o_omitir <- function(file) {
  nombre <- basename(file)
  message("📌 Revisando: ", nombre)
  
  # Leer CSV
  datos <- tryCatch(fread(file, sep = ","), error = function(e) return(NULL))
  if (is.null(datos)) {
    return(data.frame(archivo = nombre, estado = "error", motivo = "error_lectura", puntos_terreno = NA))
  }
  
  # Verificar existencia de columna "Classification"
  if (!"Classification" %in% names(datos)) {
    return(data.frame(archivo = nombre, estado = "omitido", motivo = "sin_columna_class", puntos_terreno = NA))
  }
  
  # Contar puntos de terreno
  puntos_terreno <- sum(datos$Classification %in% clases_terreno, na.rm = TRUE)
  if (puntos_terreno < umbral_minimo_puntos) {
    return(data.frame(archivo = nombre, estado = "omitido", motivo = "pocos_puntos_terreno", puntos_terreno = puntos_terreno))
  }
  
  # Convertir columnas a enteros (cuando aplica)
  cols_integer <- intersect(c(
    "Intensity", "Return_Number", "Number_Of_Returns",
    "Classification", "Scan_Angle_Rank", "User_Data", "Point_Source_ID"
  ), names(datos))
  for (col in cols_integer) {
    datos[[col]] <- as.integer(round(as.numeric(datos[[col]])))
  }
  
  # Crear objeto LAS
  nube <- LAS(data = datos)
  
  # Filtrar solo puntos de terreno
  terreno <- filter_poi(nube, Classification %in% clases_terreno)
  if (is.empty(terreno)) {
    return(data.frame(archivo = nombre, estado = "omitido", motivo = "sin_puntos_utiles", puntos_terreno = 0))
  }
  
  # Generar MDT usando interpolación IDW
  mdt <- tryCatch({
    grid_terrain(terreno, res = 1, algorithm = knnidw(k = 10))
  }, error = function(e) {
    message("❌ No se pudo generar DTM para: ", nombre)
    return(NULL)
  })
  
  if (is.null(mdt)) {
    return(data.frame(archivo = nombre, estado = "omitido", motivo = "fallo_grid_terrain", puntos_terreno = npoints(terreno)))
  }
  
  # Calcular Z_normalizada (altura relativa al terreno)
  alturas <- raster::extract(mdt, datos[, .(X, Y)])
  alturas[is.na(alturas)] <- 0
  datos[, Z_normalizada := Z - alturas]
  
  # Sobrescribir archivo CSV con nueva columna
  fwrite(datos, file, sep = ",", quote = FALSE, col.names = TRUE)
  message("✅ Recalculado: ", nombre)
  
  return(data.frame(archivo = nombre, estado = "recalculado", motivo = "ok", puntos_terreno = puntos_terreno))
}


# -----------------------------
# EJECUTAR Y GUARDAR LOG
# -----------------------------
cat("🚀 Iniciando revisión de archivos con 10 núcleos...\n")
log <- future_lapply(archivos_csv, procesar_o_omitir)
log_df <- rbindlist(log, fill = TRUE)

# Guardar log en la misma carpeta
fwrite(log_df, file.path(carpeta_csv, "log_recalculo_Z.csv"))
cat("📋 Proceso finalizado. Log guardado en: log_recalculo_Z.csv ✅\n")
