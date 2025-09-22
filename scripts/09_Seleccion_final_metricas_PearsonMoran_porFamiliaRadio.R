# ================================================================
# SCRIPT: Selección y visualización de métricas top (ranking final)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script combina los resultados del ranking de métricas con
#   la tabla de familias, filtra por radios relevantes (3 y 4),
#   selecciona la mejor métrica por familia, y exporta:
#     - CSV con las métricas seleccionadas
#     - Gráfico de barras estilizado con las métricas top
#
# Paquetes requeridos:
#   - readr, dplyr, stringr, ggplot2
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/ranking_metricas/ranking_metricas_seleccion_ponderado.csv
#       ./data/salida/familias_metricas.csv
#   - Salida:
#       ./data/salida/ranking_metricas/top_metricas_filtradas.csv
#       ./data/salida/ranking_metricas/grafico_top_metricas_filtradas.jpg
# ================================================================


# -----------------------------
# LIBRERÍAS
# -----------------------------
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)


# -----------------------------
# RUTAS DE ENTRADA Y SALIDA
# -----------------------------
archivo_ranking  <- "./data/salida/ranking_metricas/ranking_metricas_seleccion_ponderado.csv"
archivo_familias <- "./data/salida/familias_metricas.csv"
carpeta_salida   <- "./data/salida/ranking_metricas"


# -----------------------------
# CARGAR DATOS
# -----------------------------
ranking   <- read_csv(archivo_ranking, show_col_types = FALSE)
familias  <- read_csv(archivo_familias, show_col_types = FALSE)


# -----------------------------
# UNIR DATOS Y EXTRAER RADIO
# -----------------------------
ranking_final <- ranking %>%
  left_join(familias, by = "Metrica") %>%
  mutate(
    # Extraer radio desde el nombre de la métrica
    Radio = str_extract(Metrica, "\\((\\d)\\)") %>% str_remove_all("[()]"),
    
    # Unificar etiquetas de familia
    Grupo_Familia = case_when(
      Familia %in% c("Normales", "Normales (Z - menos robusto)") ~ "Normales",
      TRUE ~ Familia
    )
  )


# -----------------------------
# FILTRAR Y SELECCIONAR MÉTRICAS
# -----------------------------
# Mantener solo radios 3 y 4 (y métricas sin radio, como Intensity)
ranking_filtrado <- ranking_final %>%
  filter(is.na(Radio) | Radio %in% c("3", "4"))

# Selección de una métrica por combinación Familia + Radio
ranking_top <- ranking_filtrado %>%
  group_by(Grupo_Familia, Radio) %>%
  slice_max(order_by = Puntaje_Combinado, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(Puntaje_Combinado)) %>%
  mutate(Etiqueta = paste0(Metrica, "\n(", Familia, ")"))


# -----------------------------
# EXPORTAR CSV FINAL
# -----------------------------
write_csv(ranking_top, file.path(carpeta_salida, "top_metricas_filtradas.csv"))


# -----------------------------
# GRÁFICO DE BARRAS
# -----------------------------
g_top <- ggplot(ranking_top, aes(x = reorder(Etiqueta, Puntaje_Combinado),
                                 y = Puntaje_Combinado,
                                 fill = Frecuencia)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = round(Puntaje_Combinado, 2)),
            vjust = 1.3, size = 4, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "lightblue", high = "steelblue", name = "Frecuencia") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 8),
    axis.text.y = element_text(face = "bold", size = 9),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    plot.caption = element_text(face = "italic", size = 13)
  ) +
  labs(
    title = "Top Métricas Filtradas",
    x = "Métrica",
    y = "Puntaje Combinado",
    caption = "El número ( ) indica el radio utilizado para calcular la variable."
  )

# Exportar gráfico
ggsave(file.path(carpeta_salida, "grafico_top_metricas_filtradas.jpg"),
       g_top, width = 10, height = 6, dpi = 1200, bg = "white")


cat("\n✅ ¡Top métricas corregidas! Número total seleccionado:", nrow(ranking_top), "\n")
