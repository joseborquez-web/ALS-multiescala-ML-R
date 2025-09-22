# ================================================================
# SCRIPT: Cálculo y visualización de fiabilidad (Usuario/Productor)
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script calcula la fiabilidad del Usuario y del Productor
#   (también conocidas como precisión y recall por clase) a partir
#   de los resultados de predicción de un modelo de clasificación.
#   Genera gráficos comparativos y exporta tablas en CSV.
#
# Flujo:
#   1. Lectura de predicciones guardadas (.csv).
#   2. Construcción de matriz de confusión cruda.
#   3. Cálculo de fiabilidad del Usuario (columnas → predicciones).
#   4. Cálculo de fiabilidad del Productor (filas → observaciones).
#   5. Visualización con ggplot2 + patchwork.
#   6. Exportación de gráficos (.jpg) y tablas (.csv).
#
# Paquetes requeridos:
#   readr, dplyr, tidyr, ggplot2, patchwork
#
# Configuración de entrada/salida:
#   - Entrada:
#       ./data/salida/algoritmos/resultados_XXX/predicciones_XXX.csv
#   - Salida:
#       ./data/salida/algoritmos/resultados_XXX/fiabilidad_*.csv
#       ./data/salida/algoritmos/resultados_XXX/fiabilidad_usuario_productor_XXX.jpg
#   (Reemplazar XXX por el algoritmo correspondiente: RF, DT, LGBM, XGB, CatBoost)
# ================================================================

library(readr)      # Lectura/escritura de CSV
library(dplyr)      # Manipulación de datos
library(tidyr)      # Transformaciones tidy
library(ggplot2)    # Visualización
library(patchwork)  # Composición de gráficos

# -------------------------------------
# DICCIONARIO DE CLASES
# -------------------------------------
nombres_clases <- c(
  "2" = "Suelo", "6" = "Edificio", "7" = "Ruido", "9" = "Agua",
  "21" = "Aerogenerador", "22" = "Torre eléctrica", "23" = "Cable eléctrico",
  "24" = "Vías", "25" = "Puente", "26" = "Placas solares",
  "27" = "Ferrocarril", "28" = "Invernadero", "29" = "Vegetación"
)

# -------------------------------------
# RUTA DE RESULTADOS (CAMBIAR SEGÚN ALGORITMO)
# -------------------------------------
carpeta_resultados <- "./data/salida/algoritmos/resultados_CatBoost"

# -------------------------------------
# LECTURA DE PREDICCIONES
# -------------------------------------
predicciones <- read_csv(file.path(carpeta_resultados, "predicciones_CatBoost.csv"), 
                         show_col_types = FALSE) %>%
  mutate(
    Classification = factor(as.character(Classification), levels = names(nombres_clases)),
    .pred_class    = factor(as.character(.pred_class),    levels = names(nombres_clases))
  )

# -------------------------------------
# MATRIZ CRUDA (Truth vs Prediction)
# -------------------------------------
matriz_tibble <- table(predicciones$Classification, predicciones$.pred_class) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename(Truth = Var1, Prediction = Var2, n = Freq) %>%
  mutate(
    Truth      = factor(as.character(Truth),      levels = names(nombres_clases), labels = nombres_clases, ordered = TRUE),
    Prediction = factor(as.character(Prediction), levels = names(nombres_clases), labels = nombres_clases, ordered = TRUE)
  )

# -------------------------------------
# FIABILIDAD DEL USUARIO (Precisión por clase)
# -------------------------------------
fiabilidad_usuario <- matriz_tibble %>%
  filter(Truth == Prediction) %>%
  group_by(Prediction) %>%
  summarise(aciertos = sum(n), .groups = "drop") %>%
  left_join(
    matriz_tibble %>% group_by(Prediction) %>% summarise(total_predicho = sum(n), .groups = "drop"),
    by = "Prediction"
  ) %>%
  mutate(Fiabilidad = ifelse(total_predicho > 0, aciertos / total_predicho, 0))

# -------------------------------------
# FIABILIDAD DEL PRODUCTOR (Recall por clase)
# -------------------------------------
fiabilidad_productor <- matriz_tibble %>%
  filter(Truth == Prediction) %>%
  group_by(Truth) %>%
  summarise(aciertos = sum(n), .groups = "drop") %>%
  left_join(
    matriz_tibble %>% group_by(Truth) %>% summarise(total_real = sum(n), .groups = "drop"),
    by = "Truth"
  ) %>%
  mutate(Fiabilidad = ifelse(total_real > 0, aciertos / total_real, 0))

# -------------------------------------
# ESTILO GRÁFICO ARMONIZADO
# -------------------------------------
tema_fiabilidad <- theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", size = 20),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.text.y  = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position    = "none",
    plot.caption       = element_text(face = "italic", size = 13)
  )

# -------------------------------------
# GRÁFICOS
# -------------------------------------
grafico_usuario <- ggplot(fiabilidad_usuario, aes(x = Prediction, y = Fiabilidad, fill = Fiabilidad)) +
  geom_col(width = 0.7, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Fiabilidad del Usuario", x = "Clase", y = "Fiabilidad (%)") +
  tema_fiabilidad

grafico_productor <- ggplot(fiabilidad_productor, aes(x = Truth, y = Fiabilidad, fill = Fiabilidad)) +
  geom_col(width = 0.7, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Fiabilidad del Productor", x = "Clase", y = "Fiabilidad (%)") +
  tema_fiabilidad

# Combinar ambos gráficos en una sola figura
grafico_combinado <- grafico_usuario + grafico_productor + plot_layout(ncol = 2)

# -------------------------------------
# EXPORTAR RESULTADOS
# -------------------------------------
# Gráfico
ggsave(filename = file.path(carpeta_resultados, "fiabilidad_usuario_productor_CatBoost.jpg"),
       plot = grafico_combinado, width = 15, height = 7, dpi = 1200)

# Tablas CSV
write_csv(fiabilidad_usuario,  file.path(carpeta_resultados, "fiabilidad_usuario_CatBoost.csv"))
write_csv(fiabilidad_productor, file.path(carpeta_resultados, "fiabilidad_productor_CatBoost.csv"))
