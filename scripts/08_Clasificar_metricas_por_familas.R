# ================================================================
# SCRIPT: Construcción manual de tabla de familias de métricas LiDAR
# ================================================================
# Autor: José Luis Bórquez Ávila
# Descripción:
#   Este script crea una tabla que clasifica métricas derivadas de
#   nubes de puntos LiDAR en familias conceptuales. La tabla es
#   construida manualmente y se exporta como archivo CSV.
#
# Flujo de trabajo:
#   1. Definir manualmente las métricas y su familia asociada.
#   2. Guardar la tabla resultante como CSV para su uso posterior.
#
# Paquetes requeridos:
#   - tibble
#   - readr
#
# Configuración de salida:
#   - Carpeta de salida: ./data/salida/familias_metricas.csv
# ================================================================


# -----------------------------
# LIBRERÍAS
# -----------------------------
library(tibble)
library(readr)


# -----------------------------
# CONSTRUCCIÓN MANUAL DE LA TABLA
# -----------------------------
familias_metricas <- tribble(
  ~Metrica,                ~Base,                   ~Familia,
  
  # Intensity y Z_normalizada
  "Intensity",              "Intensity",              "Intensidad",
  "Z_normalizada",          "Z_normalizada",          "Altura relativa",
  
  # Rugosidad
  "Roughness (2)",          "Roughness",              "Rugosidad",
  "Roughness (3)",          "Roughness",              "Rugosidad",
  "Roughness (4)",          "Roughness",              "Rugosidad",
  
  # Normales
  "Nx_(2)",                 "Nx_",                    "Normales XY",
  "Nx_(3)",                 "Nx_",                    "Normales XY",
  "Nx_(4)",                 "Nx_",                    "Normales XY",
  "Ny_(2)",                 "Ny_",                    "Normales XY",
  "Ny_(3)",                 "Ny_",                    "Normales XY",
  "Ny_(4)",                 "Ny_",                    "Normales XY",
  "Nz_(2)",                 "Nz_",                    "Normales Z",
  "Nz_(3)",                 "Nz_",                    "Normales Z",
  "Nz_(4)",                 "Nz_",                    "Normales Z",
  
  # Verticality
  "Verticality_(2)",        "Verticality",            "Verticalidad",
  "Verticality_(3)",        "Verticality",            "Verticalidad",
  "Verticality_(4)",        "Verticality",            "Verticalidad",
  
  # Normal change rate y Surface variation
  "Normal change rate (2)", "Normal change rate",     "Normal change rate",
  "Normal change rate (3)", "Normal change rate",     "Normal change rate",
  "Normal change rate (4)", "Normal change rate",     "Normal change rate",
  "Surface_variation_(2)",  "Surface variation",      "Normal change rate",
  "Surface_variation_(3)",  "Surface variation",      "Normal change rate",
  "Surface_variation_(4)",  "Surface variation",      "Normal change rate",
  
  # Omnivariance
  "Omnivariance_(2)",       "Omnivariance",           "Omnivariance",
  "Omnivariance_(3)",       "Omnivariance",           "Omnivariance",
  "Omnivariance_(4)",       "Omnivariance",           "Omnivariance",
  
  # Formas Eigenvalores
  "Planarity_(2)",          "Planarity",              "Formas Eigenvalores",
  "Planarity_(3)",          "Planarity",              "Formas Eigenvalores",
  "Planarity_(4)",          "Planarity",              "Formas Eigenvalores",
  "Linearity_(2)",          "Linearity",              "Formas Eigenvalores",
  "Linearity_(3)",          "Linearity",              "Formas Eigenvalores",
  "Linearity_(4)",          "Linearity",              "Formas Eigenvalores",
  "Anisotropy_(2)",         "Anisotropy",             "Formas Eigenvalores",
  "Anisotropy_(3)",         "Anisotropy",             "Formas Eigenvalores",
  "Anisotropy_(4)",         "Anisotropy",             "Formas Eigenvalores",
  "Sphericity_(2)",         "Sphericity",             "Formas Eigenvalores",
  "Sphericity_(3)",         "Sphericity",             "Formas Eigenvalores",
  "Sphericity_(4)",         "Sphericity",             "Formas Eigenvalores",
  
  # PCA Metrics
  "PCA1_(2)",               "PCA1",                   "PCA Metrics",
  "PCA1_(3)",               "PCA1",                   "PCA Metrics",
  "PCA1_(4)",               "PCA1",                   "PCA Metrics",
  "PCA2_(2)",               "PCA2",                   "PCA Metrics",
  "PCA2_(3)",               "PCA2",                   "PCA Metrics",
  "PCA2_(4)",               "PCA2",                   "PCA Metrics",
  
  # Dispersión / Entropía
  "1st order moment (2)",   "1st order moment",       "Dispersión / Entropía",
  "1st order moment (3)",   "1st order moment",       "Dispersión / Entropía",
  "1st order moment (4)",   "1st order moment",       "Dispersión / Entropía",
  "Eigenentropy_(2)",       "Eigenentropy",           "Dispersión / Entropía",
  "Eigenentropy_(3)",       "Eigenentropy",           "Dispersión / Entropía",
  "Eigenentropy_(4)",       "Eigenentropy",           "Dispersión / Entropía",
  
  # Curvatura media
  "Mean curvature (2)",     "Mean curvature",         "Curvatura media",
  "Mean curvature (3)",     "Mean curvature",         "Curvatura media",
  "Mean curvature (4)",     "Mean curvature",         "Curvatura media",
  
  # Curvatura gaussiana
  "Gaussian curvature (2)", "Gaussian curvature",     "Curvatura gaussiana",
  "Gaussian curvature (3)", "Gaussian curvature",     "Curvatura gaussiana",
  "Gaussian curvature (4)", "Gaussian curvature",     "Curvatura gaussiana",
  
  # Sumas Eigenvalues
  "Eigenvalues sum (2)",    "Eigenvalues sum",        "Sumas Eigenvalues",
  "Eigenvalues sum (3)",    "Eigenvalues sum",        "Sumas Eigenvalues",
  "Eigenvalues sum (4)",    "Eigenvalues sum",        "Sumas Eigenvalues",
  
  # Primer eigenvalor
  "1st_eigenvalue_(2)",     "1st eigenvalue",         "Primer eigenvalor",
  "1st_eigenvalue_(3)",     "1st eigenvalue",         "Primer eigenvalor",
  "1st_eigenvalue_(4)",     "1st eigenvalue",         "Primer eigenvalor",
  
  # Segundo eigenvalor
  "2nd_eigenvalue_(2)",     "2nd eigenvalue",         "Segundo eigenvalor",
  "2nd_eigenvalue_(3)",     "2nd eigenvalue",         "Segundo eigenvalor",
  "2nd_eigenvalue_(4)",     "2nd eigenvalue",         "Segundo eigenvalor",
  
  # Tercer eigenvalor
  "3rd_eigenvalue_(2)",     "3rd eigenvalue",         "Tercer eigenvalor",
  "3rd_eigenvalue_(3)",     "3rd eigenvalue",         "Tercer eigenvalor",
  "3rd_eigenvalue_(4)",     "3rd eigenvalue",         "Tercer eigenvalor"
)


# -----------------------------
# EXPORTAR CSV
# -----------------------------
ruta_salida <- "./data/salida/familias_metricas.csv"
write_csv(familias_metricas, ruta_salida)

cat("\n✅ ¡familias_metricas.csv creado correctamente en:", ruta_salida, "!\n")
