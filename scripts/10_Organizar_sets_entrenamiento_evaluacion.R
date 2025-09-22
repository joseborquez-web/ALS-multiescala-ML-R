# ================================================================
# SCRIPT: Separación de teselas en Entrenamiento / Evaluación / Otros
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script organiza automáticamente los archivos CSV de teselas
#   en tres carpetas:
#     - Entrenamiento
#     - Evaluación (prueba)
#     - Otros
#
#   Usa un CSV maestro de división de sets para decidir dónde enviar
#   cada tesela. También genera un log con el detalle de movimientos
#   y un resumen final con conteo de archivos.
#
# Paquetes requeridos:
#   - readr, fs, dplyr, furrr, future
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/multiescala_combinado/ (teselas en CSV)
#       ./data/division_sets_entrenamiento_validacion_test_final.csv
#   - Salida:
#       ./data/salida/algoritmos/entrenamiento/
#       ./data/salida/algoritmos/evaluacion/
#       ./data/salida/algoritmos/otros/
#       ./data/salida/algoritmos/logs/
# ================================================================


# -----------------------------
# LIBRERÍAS
# -----------------------------
library(readr)
library(fs)
library(dplyr)
library(furrr)
library(future)


# -----------------------------
# CONFIGURAR PARALELISMO
# -----------------------------
plan(multisession, workers = 10)


# -----------------------------
# RUTAS DE CARPETAS Y ARCHIVOS
# -----------------------------
carpeta_origen        <- "./data/salida/multiescala_combinado/"
carpeta_entrenamiento <- "./data/salida/algoritmos/entrenamiento/"
carpeta_prueba        <- "./data/salida/algoritmos/evaluacion/"
carpeta_otros         <- "./data/salida/algoritmos/otros/"
archivo_csv           <- "./data/division_sets_entrenamiento_validacion_test_final.csv"
carpeta_logs          <- "./data/salida/algoritmos/logs/"

# Crear carpetas si no existen
dir_create(carpeta_entrenamiento)
dir_create(carpeta_prueba)
dir_create(carpeta_otros)
dir_create(carpeta_logs)


# -----------------------------
# LEER CSV DE DIVISIÓN
# -----------------------------
datos_csv <- read_csv(archivo_csv, show_col_types = FALSE)

if (!"Filename" %in% names(datos_csv)) {
  stop("❌ No se encontró la columna 'Filename' en el CSV.")
}

teselas_validas <- datos_csv$Filename


# -----------------------------
# LISTAR ARCHIVOS DE ORIGEN
# -----------------------------
archivos_csv <- dir_ls(carpeta_origen, regexp = "\\.csv$", recurse = FALSE)


# -----------------------------
# INICIALIZAR LOG
# -----------------------------
log_archivos <- tibble(Archivo = character(), Accion = character(), Detalle = character())


# -----------------------------
# FUNCIÓN PARA MOVER ARCHIVOS
# -----------------------------
mover_archivo <- function(nombre_archivo, dir_origen, dir_destino) {
  archivo_origen  <- file.path(dir_origen, nombre_archivo)
  archivo_destino <- file.path(dir_destino, nombre_archivo)
  
  if (!file.exists(archivo_origen)) {
    log_archivos <<- bind_rows(log_archivos,
                               tibble(Archivo = nombre_archivo,
                                      Accion = "No encontrado",
                                      Detalle = "❌ No existe en origen"))
    return(NULL)
  }
  
  if (file.exists(archivo_destino)) {
    log_archivos <<- bind_rows(log_archivos,
                               tibble(Archivo = nombre_archivo,
                                      Accion = "Ya existía",
                                      Detalle = "⚠️ Archivo ya estaba en destino"))
    return(NULL)
  }
  
  exito_copia <- file.copy(archivo_origen, archivo_destino, overwrite = TRUE)
  
  if (exito_copia) {
    file.remove(archivo_origen)
    log_archivos <<- bind_rows(log_archivos,
                               tibble(Archivo = nombre_archivo,
                                      Accion = "Movido",
                                      Detalle = "✅ Movido correctamente"))
  } else {
    log_archivos <<- bind_rows(log_archivos,
                               tibble(Archivo = nombre_archivo,
                                      Accion = "Error",
                                      Detalle = "❌ No se pudo copiar"))
  }
  
  return(NULL)
}


# -----------------------------
# SEPARAR TESELAS SEGÚN GRUPO
# -----------------------------
teselas_entrenamiento <- datos_csv %>%
  filter(Group %in% c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,25,26,27,28,29)) %>%
  pull(Filename)

teselas_prueba <- datos_csv %>%
  filter(Group %in% c(1,24)) %>%
  pull(Filename)

teselas_otros <- datos_csv %>%
  filter(!(Group %in% c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27,28,29))) %>%
  pull(Filename)


# -----------------------------
# MOVER ARCHIVOS
# -----------------------------
future_walk(teselas_entrenamiento, ~mover_archivo(gsub("\\.laz$", ".csv", .x), carpeta_origen, carpeta_entrenamiento))
future_walk(teselas_prueba,        ~mover_archivo(gsub("\\.laz$", ".csv", .x), carpeta_origen, carpeta_prueba))
future_walk(teselas_otros,         ~mover_archivo(gsub("\\.laz$", ".csv", .x), carpeta_origen, carpeta_otros))


# -----------------------------
# FINALIZAR PARALELISMO
# -----------------------------
plan(sequential)


# -----------------------------
# GUARDAR LOG Y RESUMEN
# -----------------------------
write_csv(log_archivos, file.path(carpeta_logs, "log_movimiento_archivos.csv"))

n_entrenamiento <- length(list.files(carpeta_entrenamiento, pattern = "\\.csv$"))
n_prueba        <- length(list.files(carpeta_prueba, pattern = "\\.csv$"))
n_otros         <- length(list.files(carpeta_otros, pattern = "\\.csv$"))
n_total         <- n_entrenamiento + n_prueba + n_otros

resumen_final <- tibble(
  Tipo       = c("Entrenamiento", "Evaluación", "Otros", "Total"),
  Cantidad   = c(n_entrenamiento, n_prueba, n_otros, n_total),
  Porcentaje = round(100 * c(n_entrenamiento, n_prueba, n_otros, n_total) / n_total, 2)
)
write_csv(resumen_final, file.path(carpeta_logs, "resumen_teselas.csv"))


# -----------------------------
# MENSAJE FINAL
# -----------------------------
cat("\n✅ Carpetas separadas correctamente: Entrenamiento, Evaluación y Otros.\n")
cat("✅ Log guardado en carpeta 'logs'.\n")
