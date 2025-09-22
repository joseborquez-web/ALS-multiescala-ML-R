Este repositorio contiene los scripts R desarrollados como parte del trabajo de título del Magíster en Teledetección de la Universidad Mayor (Chile), titulado:

**“Clasificación de nubes de puntos ALS mediante algoritmos de aprendizaje automático”**  
(*“Classification of ALS point clouds using machine learning algorithms”*)  
**José Luis Bórquez Ávila**, Universidad Mayor, 2025

## 📁 Estructura del repositorio

- **data/**: Archivos auxiliares generados para el flujo (división de conjuntos, familias de métricas).  
  > *Nota: Los datos originales no están disponibles por restricciones de uso.*
  
- **models/**: Scripts R utilizados para entrenar y evaluar cada modelo de machine learning.

- **outputs/**: Figuras, tablas y resultados generados a partir de los scripts.

- **scripts/**: Scripts R utilizados para la preparación de datos, cálculo de métricas y análisis exploratorio.

## 📜 Descripción

Se implementaron flujos de clasificación multiescalares para nubes de puntos LiDAR aéreo (ALS), empleando algoritmos de aprendizaje automático como `Random Forest`, `CatBoost`, `XGBoost` y `LightGBM`, con énfasis en clases minoritarias y de alta complejidad estructural (aerogeneradores, paneles solares, cables, etc.).

El dataset corresponde a la segunda cobertura LiDAR del Plan Nacional de Ortofotografía Aérea (PNOA, España). Se calcularon métricas como Nx, verticalidad, intensidad, omnivarianza, momentos de orden 1 y 2, entre otras, usando `lidR`, `CloudCompare` y flujos en R (`tidymodels`, `bonsai`, `furrr`, etc.).

## 👨‍🎓 Autor

**José Luis Bórquez Ávila**  
Magíster en Teledetección, Universidad Mayor  
✉️ jose.borquez@mayor.cl  
📬 jborquez.ingeos@gmail.com  
🔗 [LinkedIn](https://www.linkedin.com/in/jos%C3%A9-luis-alberto-borquez-avila-18672b1ba/)

## 📄 Licencia

Este código se publica bajo la licencia MIT. Ver el archivo [LICENSE](LICENSE) para más detalles.

---

Autor: José Luis Bórquez Ávila  
Magíster en Teledetección – Universidad Mayor  
Año: 2025
