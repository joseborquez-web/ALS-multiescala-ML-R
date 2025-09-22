# ================================================================
# SCRIPT: Análisis de correlación multisemilla en métricas LiDAR
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script toma subconjuntos aleatorios de teselas (archivos CSV)
#   y realiza un análisis de correlación multisemilla:
#     1. Selección de n subconjuntos distintos (semillas aleatorias).
#     2. Cálculo de correlaciones entre métricas numéricas.
#     3. Identificación de pares altamente correlacionados.
#     4. Generación de gráficos: heatmap, corrplot, densidades y boxplots.
#     5. Exportación de matrices de correlación y resúmenes estadísticos.
#
# Paquetes requeridos:
#   - readr, dplyr, tidyr
#   - future, furrr
#   - ggplot2, corrplot, pheatmap, factoextra
#   - purrr
#
# Configuración de entrada/salida:
#   - Carpeta de entrada: ./data/salida/multiescala_combinado
#   - Carpeta de salida:  ./data/salida/correlacion_semillas
# ================================================================


# -----------------------------
# LIBRERÍAS NECESARIAS
# -----------------------------
library(readr)
library(dplyr)
library(tidyr)
library(future)
library(furrr)
library(ggplot2)
library(corrplot)
library(pheatmap)
library(factoextra)
library(purrr)


# -----------------------------
# CONFIGURACIÓN GENERAL
# -----------------------------
plan(multisession, workers = 5)  # Núcleos a utilizar

n_semillas <- 10                     # Número de repeticiones (semillas)
n_teselas_por_subconjunto <- 100     # Archivos por subconjunto
umbral_correlacion <- 0.85           # Umbral para considerar correlación alta

# Carpeta de entrada y salida (ajustar según repositorio)
carpeta_base   <- "./data/salida/multiescala_combinado"
carpeta_salida <- "./data/salida/correlacion_semillas"
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# CREACIÓN DE SUBCONJUNTOS
# -----------------------------
teselas <- list.files(carpeta_base, pattern = "\\.csv$", full.names = TRUE)

set.seed(777)
semillas <- sample(1000:9999, n_semillas)

subconjuntos <- lapply(semillas, function(s) {
  set.seed(s)
  sample(teselas, n_teselas_por_subconjunto)
})
names(subconjuntos) <- paste0("semilla_", semillas)


# -----------------------------
# FUNCIÓN DE ANÁLISIS
# -----------------------------
analizar_subconjunto <- function(archivos, nombre_grupo) {
  cat("🔁 Procesando:", nombre_grupo, "\n")
  salida_grupo <- file.path(carpeta_salida, nombre_grupo)
  dir.create(salida_grupo, showWarnings = FALSE)
  
  # Combinar archivos seleccionados en un único data.frame
  df <- bind_rows(future_map(archivos, read_csv, show_col_types = FALSE, na = c("NaN", "NA", "", " ")))
  
  # Columnas que no se consideran métricas
  excluir <- c(
    "X", "Y", "Z", "gpstime", "ReturnNumber", "NumberOfReturns",
    "ScanDirectionFlag", "EdgeOfFlightline", "Classification",
    "UserData", "PointSourceID", "ScanAngleRank", "R", "G", "B",
    "Number of neighbors (r=2)", "Surface density (r=2)", "Volume density (r=2)",
    "Number of neighbors (r=3)", "Surface density (r=3)", "Volume density (r=3)",
    "Number of neighbors (r=4)", "Surface density (r=4)", "Volume density (r=4)"
  )
  
  # Seleccionar solo métricas válidas
  metricas <- df %>%
    select(where(is.numeric)) %>%
    select(-any_of(excluir[excluir %in% names(.)])) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  if (ncol(metricas) < 2) {
    cat("⚠️ Demasiadas columnas filtradas. Saltando:", nombre_grupo, "\n")
    return(NULL)
  }
  
  # Calcular matriz de correlación
  cor_matriz <- cor(metricas, use = "pairwise.complete.obs", method = "pearson")
  write_csv(as.data.frame(cor_matriz), file.path(salida_grupo, "correlacion_matriz.csv"))
  
  # Guardar métricas altamente correlacionadas
  cor_altas <- which(abs(cor_matriz) > umbral_correlacion & abs(cor_matriz) < 1, arr.ind = TRUE)
  if (nrow(cor_altas) > 0) {
    pares <- data.frame(
      Metrica_1   = rownames(cor_matriz)[cor_altas[, 1]],
      Metrica_2   = colnames(cor_matriz)[cor_altas[, 2]],
      Correlacion = cor_matriz[cor_altas]
    )
    write_csv(pares, file.path(salida_grupo, "metricas_altamente_correlacionadas.csv"))
  }
  
  # Correlograma circular tipo corrplot (JPG)
  jpeg(file.path(salida_grupo, "correlacion_corrplot.jpg"), width = 12000, height = 12000, res = 1200, bg = "white")
  corrplot(cor_matriz,
           method = "circle",
           type   = "full",
           order  = "hclust",
           tl.cex = 0.6,
           tl.col = "black",
           cl.cex = 0.8,
           addCoef.col = NULL,
           col = colorRampPalette(c("red", "white", "blue"))(200))
  dev.off()
  
  # Distribuciones de densidad (JPG)
  metricas_long <- metricas %>% pivot_longer(everything(), names_to = "Metrica", values_to = "Valor")
  g1 <- ggplot(metricas_long, aes(x = Valor)) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    facet_wrap(~ Metrica, scales = "free", ncol = 8) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Distribuciones -", nombre_grupo))
  ggsave(file.path(salida_grupo, "distribuciones_metricas.jpg"), g1, width = 20, height = 12, dpi = 1200, bg = "white")
  
  # Boxplot de métricas (JPG)
  g2 <- ggplot(metricas_long, aes(x = Metrica, y = Valor)) +
    geom_boxplot(fill = "lightblue", outlier.size = 0.5) +
    theme_minimal(base_size = 10) +
    coord_flip() +
    labs(title = paste("Boxplot de métricas -", nombre_grupo))
  ggsave(file.path(salida_grupo, "boxplot_metricas.jpg"), g2, width = 20, height = 12, dpi = 600, bg = "white")
  
  # Resumen estadístico de cada métrica
  resumen <- data.frame(
    Metrica   = colnames(metricas),
    Media     = sapply(metricas, function(x) mean(x, na.rm = TRUE)),
    SD        = sapply(metricas, function(x) sd(x, na.rm = TRUE)),
    Min       = sapply(metricas, function(x) min(x, na.rm = TRUE)),
    Max       = sapply(metricas, function(x) max(x, na.rm = TRUE)),
    NA_count  = sapply(metricas, function(x) sum(is.na(x)))
  )
  write_csv(resumen, file.path(salida_grupo, "resumen_metricas.csv"))
  
  cat("✅ Terminado:", nombre_grupo, "\n")
}


# -----------------------------
# EJECUTAR ANÁLISIS
# -----------------------------
walk2(subconjuntos, names(subconjuntos), analizar_subconjunto)

cat("\n🎉 ¡Todo listo! Resultados guardados en:\n", carpeta_salida, "\n")
