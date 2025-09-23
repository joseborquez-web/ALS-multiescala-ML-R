# ================================================================
# SCRIPT: Análisis espacial (Índice de Moran) en métricas LiDAR
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script evalúa la autocorrelación espacial de métricas
#   multiescalares LiDAR usando el índice de Moran (I).
#
#   Flujo de trabajo:
#     1. Calcular centroides de cada tesela.
#     2. Agrupar teselas espacialmente con DBSCAN.
#     3. Seleccionar los 10 grupos espaciales más grandes.
#     4. Generar subconjuntos replicables de teselas.
#     5. Calcular Moran I para cada métrica válida.
#     6. Exportar resultados: CSV, barplots y mapas.
#
# Paquetes requeridos:
#   - readr, dplyr, tibble, purrr, furrr, progressr
#   - dbscan, sf, spdep
#   - ggplot2, patchwork
#
# Configuración de entrada/salida:
#   - Carpeta de entrada: ./data/salida/multiescala_combinado
#   - Carpeta de salida:  ./data/salida/moran_semillas
# ================================================================


# -----------------------------
# LIBRERÍAS NECESARIAS
# -----------------------------
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(furrr)
library(progressr)
library(dbscan)
library(sf)
library(spdep)
library(ggplot2)
library(patchwork)


# -----------------------------
# CONFIGURACIÓN GENERAL
# -----------------------------
plan(multisession, workers = 15)  # Núcleos de ejecución
handlers(global = TRUE)
handlers("progress")

n_teselas_por_subconjunto <- 100   # Nº de teselas por subconjunto
umbral_moran <- 0.4                # Umbral de Moran I para considerar relevancia

# Carpeta de entrada y salida
carpeta        <- "./data/salida/multiescala_combinado"
carpeta_salida <- "./data/salida/moran_semillas"
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# CENTROIDES DE TESELAS
# -----------------------------
teselas <- sort(list.files(carpeta, pattern = "\\.csv$", full.names = TRUE))
cat("🔍 Total de teselas encontradas:", length(teselas), "\n")

extraer_centroide <- function(archivo) {
  tryCatch({
    df <- read_csv(archivo, col_select = c("X", "Y"), show_col_types = FALSE, na = c("NaN", "NA", " "))
    df <- filter(df, !is.na(X), !is.na(Y))
    if (nrow(df) == 0) return(NULL)
    tibble(archivo_csv = archivo, X = median(df$X, na.rm = TRUE), Y = median(df$Y, na.rm = TRUE))
  }, error = function(e) {
    message("⚠️  Error leyendo archivo: ", archivo)
    return(NULL)
  })
}

cat("⏳ Calculando centroides...\n")
with_progress({
  p <- progressor(along = teselas)
  centroides_df <- future_map_dfr(teselas, function(archivo) {
    res <- extraer_centroide(archivo); p(); res
  })
})
cat("✅ Centroides calculados para", nrow(centroides_df), "teselas.\n")


# -----------------------------
# CLUSTERING ESPACIAL (DBSCAN)
# -----------------------------
coords <- centroides_df %>% select(X, Y)
clustering <- dbscan(coords, eps = 7000, minPts = 5)
centroides_df$grupo_espacial <- clustering$cluster


# -----------------------------
# SELECCIÓN DE 10 GRUPOS PRINCIPALES
# -----------------------------
grupos_validos <- centroides_df %>%
  filter(grupo_espacial != 0) %>%
  count(grupo_espacial, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(grupo_espacial)

set.seed(42)  # Garantiza replicabilidad
subconjuntos <- lapply(grupos_validos, function(g) {
  centroides_df %>%
    filter(grupo_espacial == g) %>%
    slice_sample(n = n_teselas_por_subconjunto) %>%
    pull(archivo_csv)
})
names(subconjuntos) <- paste0("semilla_espacial_", seq_along(subconjuntos))
cat("🎯 Subconjuntos generados con semilla fija.\n\n")


# -----------------------------
# FUNCIÓN DE ANÁLISIS DE MORAN
# -----------------------------
analizar_moran <- function(archivos, nombre_grupo) {
  cat("🔁 Procesando grupo:", nombre_grupo, "\n")
  salida_grupo <- file.path(carpeta_salida, nombre_grupo)
  dir.create(salida_grupo, showWarnings = FALSE)
  
  # Combinar teselas en un único dataset
  df_total <- bind_rows(future_map(archivos, read_csv, na = c("NaN", "NA", "", " "), show_col_types = FALSE)) %>%
    distinct(X, Y, .keep_all = TRUE)
  
  # Excluir columnas no métricas
  excluir <- c("X","Y","Z","gpstime","ReturnNumber","NumberOfReturns","ScanDirectionFlag",
               "EdgeOfFlightline","Classification","UserData","PointSourceID","ScanAngleRank",
               "R","G","B",
               "Number of neighbors (r=2)","Surface density (r=2)","Volume density (r=2)",
               "Number of neighbors (r=3)","Surface density (r=3)","Volume density (r=3)",
               "Number of neighbors (r=4)","Surface density (r=4)","Volume density (r=4)")
  names(df_total) <- trimws(names(df_total))
  
  metricas <- df_total %>%
    select(where(is.numeric)) %>%
    select(-any_of(excluir[excluir %in% names(.)])) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  if (ncol(metricas) == 0) {
    cat("⚠️  No hay métricas válidas para analizar en", nombre_grupo, "\n"); return(NULL)
  }
  
  # Objeto espacial + pesos
  sf_obj <- st_as_sf(df_total, coords = c("X", "Y"), crs = 25830)
  coords <- st_coordinates(sf_obj)
  vecinos <- knearneigh(coords, k = 8)
  lista_vecinos <- knn2nb(vecinos)
  pesos <- nb2listw(lista_vecinos, style = "W", zero.policy = TRUE)
  
  # Cálculo Moran I
  resultados_moran <- map_dfr(colnames(metricas), function(met) {
    valores <- metricas[[met]]
    if (any(!is.na(valores))) {
      test <- moran.test(valores, pesos, na.action = na.exclude, zero.policy = TRUE)
      tibble(Metrica = met, Moran_I = as.numeric(test$estimate["Moran I statistic"]))
    } else NULL
  }) %>% arrange(desc(Moran_I))
  
  write_csv(resultados_moran, file.path(salida_grupo, "moran_resultados_metricas.csv"))
  
  # Top métricas
  top_moran <- resultados_moran %>% filter(Moran_I > umbral_moran) %>% slice_head(n = 18)
  if (nrow(top_moran) == 0) {
    cat("⚠️  Ninguna métrica superó el umbral en", nombre_grupo, "\n"); return(NULL)
  }
  
  # Barplot
  g_moran <- ggplot(top_moran, aes(x = reorder(Metrica, -Moran_I), y = Moran_I, fill = Moran_I)) +
    geom_col(width = 0.7, color = "black") +
    geom_text(aes(label = round(Moran_I, 2)), vjust = 1.3, size = 4, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    theme_minimal(base_size = 13) +
    labs(title = paste("Top métricas según índice de Moran –", nombre_grupo),
         x = "Métrica", y = "Índice de Moran (I)",
         caption = "El número ( ) indica el radio utilizado para calcular la variable.") +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
          axis.text.y = element_text(face = "bold", size = 11),
          axis.title = element_text(face = "bold", size = 14),
          panel.grid.major.x = element_blank(),
          legend.position = "none",
          plot.caption = element_text(face = "italic", size = 13))
  
  ggsave(file.path(salida_grupo, "moran_top_barplot.jpg"),
         g_moran, width = 10, height = 6, dpi = 1200, bg = "white")
  
  # Mapas de las 3 mejores métricas
  top_3 <- top_moran %>% slice_head(n = 3)
  plots <- lapply(top_3$Metrica, function(met) {
    ggplot(df_total, aes(x = X, y = Y, color = .data[[met]])) +
      geom_point(size = 0.6) +
      scale_color_viridis_c(option = "D", limits = c(-3, 3),
                            guide = guide_colorbar(title = NULL, barwidth = 6, barheight = 0.4)) +
      coord_equal() +
      theme_minimal(base_size = 8) +
      labs(title = met) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  })
  
  grid <- wrap_plots(plots, ncol = 3) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  ggsave(file.path(salida_grupo, "distribucion_top3_moran.jpg"),
         grid, width = 9, height = 3.5, dpi = 600, bg = "white")
  
  cat("✅ Terminado:", nombre_grupo, "\n")
}


# -----------------------------
# APLICAR A TODOS LOS SUBCONJUNTOS
# -----------------------------
walk2(subconjuntos, names(subconjuntos), analizar_moran)
cat("\n🎉 ¡Análisis de Moran completado para todos los subconjuntos espaciales!\n")

