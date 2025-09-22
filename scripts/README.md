# Carpeta `scripts/`

Esta carpeta contiene los scripts R utilizados para procesar y preparar las métricas multiescalares derivadas de datos LiDAR ALS, antes y después del entrenamiento de los modelos.

## Scripts incluidos

### 1. Cálculo y preparación de métricas
- `01_Calculo_Z_normalizada.R`: Alturas normalizadas (`Z_norm`).
- `02_Celdas_vacias_a_NaN.R`: Conversión explícita de vacíos a `NaN`.
- `03_Estandarización_de_métricas_LiDAR.R`: Estandarización tipo Z-score.
- `04_Combinar_metricas_multiescala_(R2 + R3 + R4).R`: Unión de métricas por escala.

> 🔹 *Nota:* Las métricas estructurales, geométricas y radiométricas.... de las nubes de puntos fueron generadas en procesos por lotes utilizando **CloudCompare**, para radios de vecindad 2, 3 y 4.

### 2. Selección de variables
- `05_Correlacion_espacial.R`: Correlación de Pearson entre métricas.
- `06_Análisis_de_Moran_con_selección_por_semillas_por_vecindad.R`: Moran multisemilla y multivecindad.
- `07_Ranking_Pearson_Moran.R`: Ranking combinado de correlación y autocorrelación.
- `08_Clasificar_metricas_por_familias.R`: Agrupación por tipo geométrico/radiométrico.
- `09_Seleccion_final_metricas_PearsonMoran_porFamiliaRadio.R`: Selección final por familia y radio.

### 3. Preparación de datos para modelado
- `10_Organizar_sets_entrenamiento_evaluacion.R`: Crear partición `Entrenamiento` y `Evaluacion`.
- `11_Transformar_a_rds.R`: Conversión de datos a formato `.rds`.
- `12_Unir_entrenamiento_y_evaluacion.R`: Fusión final de datos de entrada.
- `13_Conteo_clases.R`: Análisis de desbalance de clases.

### 4. Evaluación posterior al modelado
- `19_Fiabilidad_usuario_y_productor.R`: Cálculo de métricas de precisión, recall y exactitud por clase.

---

*Todos los scripts están escritos en R y forman parte del flujo metodológico del trabajo de título.*

---

Autor: José Luis Bórquez Ávila  
Magíster en Teledetección – Universidad Mayor  
Año: 2025
