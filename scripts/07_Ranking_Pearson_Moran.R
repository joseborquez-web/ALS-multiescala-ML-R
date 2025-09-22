# ================================================================
# SCRIPT: Ranking de métricas (Correlación + Moran’s I)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script combina los resultados de selección de métricas
#   generados a partir de 10 semillas distintas, aplicando dos filtros:
#     1) Exclusión de métricas altamente correlacionadas.
#     2) Selección de métricas con alto índice de Moran (I > umbral).
#
#   Con esta información se construye un ranking global ponderado,
#   que combina la frecuencia de aparición y el valor promedio de Moran’s I.
#   Finalmente, se exporta:
#     - CSV con el ranking completo.
#     - Gráfico del Top 14 de métricas mejor puntuadas.
#
# Paquetes requeridos:
#   readr, dplyr, tidyr, ggplot2, patchwork, purrr, viridis, fs
#
# Entradas esperadas:
#   ./ML/correlacion_semillas/*/metricas_altamente_correlacionadas.csv
#   ./ML/moran_semillas/*/moran_resultados_metricas.csv
#
# Salidas:
#   ./ML/ranking_metricas/ranking_metricas_seleccion_ponderado.csv
#   ./ML/ranking_metricas/top14_metricas_plot.jpg
# ================================================================

# -------------------------------------
# LIBRERÍAS
# -------------------------------------
library(readr)    # Lectura de CSV
library(dplyr)    # Manipulación de datos
library(ggplot2)  # Gráficos
library(purrr)    # Programación funcional (map)
library(viridis)  # Escalas de color
library(fs)       # Manejo de rutas
library(tidyr)    # Transformación de datos

# -------------------------------------
# RUTAS DE ENTRADA Y SALIDA
# -------------------------------------
carpeta_corr   <- "C:/Users/jborq/Escritorio/Proyecto_Grado/Geomatica/Pruebas_JLB/ML/correlacion_semillas"
carpeta_moran  <- "C:/Users/jborq/Escritorio/Proyecto_Grado/Geomatica/Pruebas_JLB/ML/moran_semillas"
carpeta_salida <- file.path(dirname(carpeta_corr), "ranking_metricas")
dir_create(carpeta_salida)

# Umbral de Moran’s I para considerar una métrica válida
umbral_moran <- 0.4

# -------------------------------------
# FUNCIONES AUXILIARES
# -------------------------------------

# Lee métricas altamente correlacionadas desde cada semilla
leer_correlaciones <- function(path) {
  archivo <- file.path(path, "metricas_altamente_correlacionadas.csv")
  if (!file.exists(archivo)) return(NULL)
  read_csv(archivo, show_col_types = FALSE) %>%
    select(Metrica_1, Metrica_2) %>%
    pivot_longer(cols = everything(), values_to = "Metrica") %>%
    distinct(Metrica)
}

# Lee los resultados de Moran’s I por métrica
leer_moran <- function(path) {
  archivo <- file.path(path, "moran_resultados_metricas.csv")
  if (!file.exists(archivo)) return(NULL)
  read_csv(archivo, show_col_types = FALSE)
}

# -------------------------------------
# LISTAR SEMILLAS DISPONIBLES
# -------------------------------------
semillas_corr  <- list.dirs(carpeta_corr, recursive = FALSE)
semillas_moran <- list.dirs(carpeta_moran, recursive = FALSE)

# -------------------------------------
# PROCESAR TODAS LAS SEMILLAS
# -------------------------------------
resultados <- map2_dfr(semillas_corr, semillas_moran, function(corr_path, moran_path) {
  correladas <- leer_correlaciones(corr_path)
  moran      <- leer_moran(moran_path)
  
  if (is.null(correladas) || is.null(moran)) return(NULL)
  
  metricas_correladas <- unique(correladas$Metrica)
  
  moran %>%
    filter(!(Metrica %in% metricas_correladas)) %>%   # excluir correlacionadas
    filter(Moran_I > umbral_moran) %>%                # aplicar filtro Moran
    mutate(Semilla = basename(moran_path))            # identificar semilla
})

# -------------------------------------
# CREAR RANKING GLOBAL
# -------------------------------------
ranking <- resultados %>%
  group_by(Metrica) %>%
  summarise(
    Frecuencia       = n(),                        # veces que aparece
    Moran_I_promedio = mean(Moran_I),              # valor promedio
    Moran_I_max      = max(Moran_I),               # valor máximo
    .groups = "drop"
  ) %>%
  mutate(
    Frecuencia_norm = Frecuencia / max(Frecuencia),          # normalización [0-1]
    Moran_I_norm    = Moran_I_promedio / max(Moran_I_promedio),
    Puntaje_Combinado = round((Frecuencia_norm + Moran_I_norm) / 2, 3)
  ) %>%
  arrange(desc(Puntaje_Combinado))

# Guardar ranking completo
write_csv(ranking, file.path(carpeta_salida, "ranking_metricas_seleccion_ponderado.csv"))

# -------------------------------------
# GRÁFICO TOP 14 MÉTRICAS
# -------------------------------------
ranking_top14 <- ranking %>% slice_max(order_by = Puntaje_Combinado, n = 14)

g14 <- ggplot(ranking_top14, aes(x = reorder(Metrica, Puntaje_Combinado),
                                 y = Puntaje_Combinado, fill = Frecuencia)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = round(Puntaje_Combinado, 3)), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Top 14 métricas seleccionadas (frecuencia + Moran’s I)",
    x = "Métrica",
    y = "Puntaje combinado",
    fill = "Frecuencia"
  )

ggsave(file.path(carpeta_salida, "top14_metricas_plot.jpg"),
       g14, width = 12, height = 6, dpi = 600, bg = "white")

# -------------------------------------
# MENSAJE FINAL
# -------------------------------------
cat("\n✅ ¡Ranking completo generado correctamente usando las 10 semillas!\n")
