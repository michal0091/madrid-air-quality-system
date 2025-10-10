# Sistema de Predicción de Calidad del Aire de Madrid

![R Version](https://img.shields.io/badge/R-4.5.1+-blue.svg) ![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue.svg) ![Machine Learning](https://img.shields.io/badge/ML-RANGER%20RF-orange.svg)

## Descripción

Sistema automatizado de análisis y predicción de calidad del aire para Madrid mediante técnicas de machine learning espacial. Integra datos históricos (2015-2025) y en tiempo real de estaciones de monitoreo con variables meteorológicas para generar predicciones a 40 horas con modelos Random Forest optimizados.

### Características principales

- **Modelos predictivos**: RANGER Random Forest con R² > 0.92 para 5 contaminantes ICA
- **Predicción espacial**: Forecasts horarios para 16 estaciones de monitoreo
- **Automatización**: Pipeline completo mediante GitHub Actions
- **Visualización**: Dashboard interactivo Shiny con mapas animados
- **Datos**: Integración APIs Madrid Open Data + AEMET (meteorología)

---

## Arquitectura del Sistema

### 1. Ingesta de datos 📥

**Fuentes de datos**:
- Históricos: Portal Open Data Madrid (2015-2025) + AEMET
- Tiempo real: API XML Madrid (19 estaciones, 10 contaminantes, actualización cada 20 min)
- Meteorología: AEMET OpenData (predicciones horarias)

**Almacenamiento**: PostgreSQL 15 con extensión PostGIS, modelo de datos estrella con tablas dimensionales y de hechos.

### 2. Modelado predictivo 🤖

**Algoritmo**: RANGER Random Forest implementado con UTM projection para preservar relaciones espaciales.

**Configuración**:
- 5 modelos independientes (NO₂, PM10, PM2.5, O₃, SO₂)
- ~100,000 observaciones por contaminante
- Variables predictoras: coordenadas UTM, meteorología horaria, temporales
- Rendimiento: R² = 0.92-0.95, RMSE = 3-6 µg/m³

### 3. Predicciones operacionales ⚡

**Horizonte temporal**: 40 horas con resolución horaria

**Output**: 3,200 predicciones (16 estaciones × 5 contaminantes × 40 horas)

**Automatización**: GitHub Actions ejecuta pipeline diariamente, actualiza dashboard en shinyapps.io

### 4. Dashboard interactivo 📊

**Framework**: Shiny + Leaflet + Plotly

**Funcionalidades**:
- Mapas interactivos con clasificación OMS 2021
- Animaciones temporales (50 gráficos de barras + 50 mapas)
- Análisis de evolución temporal y meteorológica
- Exportación datos (CSV/JSON)

---

## Stack Tecnológico

**Lenguaje**: R 4.5.1+

**Paquetes principales**:
- ML: `caret`, `randomForest`, `ranger`
- Espacial: `sf`, `mapSpain`, `tidyterra`
- Datos: `data.table`, `dplyr`, `lubridate`
- Visualización: `shiny`, `leaflet`, `plotly`, `ggplot2`
- BD: `DBI`, `RPostgres`

**Infraestructura**:
- Base de datos: PostgreSQL 15 + PostGIS
- CI/CD: GitHub Actions
- Deployment: shinyapps.io
- Gestión dependencias: `renv`

---

## Estructura del Repositorio

```
madrid-air-quality-system/
├── R/                                    # Scripts de análisis
│   ├── 00_setup_dimension_tables.R      # Configuración inicial BD
│   ├── 01b_collect_historical_data.R    # Ingesta datos históricos
│   ├── 01d_collect_meteo_data.R         # Datos meteorológicos AEMET
│   ├── 02_modelo_ranger_ica.R           # Entrenamiento modelos RANGER
│   ├── 05_predicciones_horarias.R       # Generación predicciones 40h
│   ├── api_madrid_real.R                # Cliente API Madrid
│   ├── datos_realtime_fallback.R        # Sistema fallback robusto
│   ├── meteo_forecast.R                 # Predicciones AEMET
│   └── utils_meteo_horario.R            # Expansión datos horarios
├── app/                                 # Dashboard Shiny
│   ├── global.R                         # Configuración global
│   ├── ui.R                             # Interfaz usuario
│   ├── server.R                         # Lógica servidor
│   └── data/                            # Datos para dashboard
├── models/                              # Modelos entrenados
│   └── ranger_ica_*.rds                 # 5 modelos RANGER ICA
├── output/                              # Predicciones generadas
│   ├── predicciones_40h_latest.rds
│   └── meteo_40h_latest.rds
├── generar_imagenes_por_hora.R          # Generador gráficos animados
├── generar_mapas_por_hora.R             # Generador mapas animados
├── .github/workflows/                   # Automatización
│   └── daily-predictions.yml
└── renv.lock                            # Dependencias exactas
```

---

## Instalación y Uso

### 1. Configuración inicial

```bash
git clone https://github.com/michal0091/madrid-air-quality-system.git
cd madrid-air-quality-system
```

### 2. Variables de entorno

Crear `.Renviron` en raíz del proyecto:

```env
# PostgreSQL
DB_HOST="tu_host"
DB_PORT="5432"
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
DB_PASSWORD="tu_password"

# AEMET API (opcional)
AEMET_API_KEY="tu_api_key"
```

### 3. Instalación de dependencias

```r
# Restaurar entorno exacto
renv::restore()

# Verificar paquetes críticos
library(caret)
library(randomForest)
library(sf)
library(shiny)
```

### 4. Entrenamiento de modelos

```r
# Entrenar modelos RANGER para 5 contaminantes ICA
source("R/02_modelo_ranger_ica.R")

# Modelos se guardan en models/ranger_ica_*.rds
# Tiempo estimado: ~30-60 min con 100K observaciones
```

### 5. Generación de predicciones

```r
# Predicciones 40 horas
source("R/05_predicciones_horarias.R")

# Output: output/predicciones_40h_latest.rds
```

### 6. Generación de visualizaciones

```r
# Gráficos de barras (50 imágenes)
Rscript generar_imagenes_por_hora.R

# Mapas animados (50 imágenes)
Rscript generar_mapas_por_hora.R
```

### 7. Dashboard interactivo

```r
# Lanzar dashboard local
source("launch_dashboard.R")

# Acceso: http://localhost:3838
```

---

## Rendimiento de Modelos

| Contaminante | R² | RMSE (µg/m³) | Observaciones |
|--------------|-----|--------------|---------------|
| NO₂ | 0.945 | 4.2 | ~120K |
| PM10 | 0.932 | 5.8 | ~95K |
| PM2.5 | 0.928 | 3.4 | ~88K |
| O₃ | 0.951 | 6.1 | ~110K |
| SO₂ | 0.921 | 2.9 | ~75K |

**Metodología de validación**: Spatial cross-validation con bloques geográficos para evitar autocorrelación espacial.

---

## Automatización GitHub Actions

El workflow `daily-predictions.yml` ejecuta diariamente:

1. Descarga modelos RANGER ICA desde releases
2. Recolecta datos Madrid API + AEMET
3. Genera 3,200 predicciones horarias
4. Crea 100 gráficos animados (barras + mapas)
5. Sincroniza datos con dashboard
6. Despliega en shinyapps.io

```yaml
schedule:
  - cron: '0 6 * * *'  # Diario 6:00 UTC (7:00 Madrid)
```

---

## Roadmap: Implementación ICA Oficial

**Objetivo**: Integrar Índice de Calidad del Aire oficial español (BOE-A-2019-4494).

### Categorías ICA

- 🟢 Buena
- 🟡 Razonablemente buena
- 🟠 Regular
- 🔴 Desfavorable
- 🟣 Muy desfavorable
- ⚫ Extremadamente desfavorable

### Tareas pendientes

1. Extraer tabla oficial de límites desde [MITECO PDF](https://www.miteco.gob.es/content/dam/miteco/es/calidad-y-evaluacion-ambiental/temas/atmosfera-y-calidad-del-aire/resolucion_02092020_modificacion_ica_tcm30-511596.pdf)
2. Implementar función `calcular_ica_oficial()` con promedios móviles (1h, 8h, 24h según contaminante)
3. Actualizar gráficos con límites ICA oficiales (sustituir OMS 2021)
4. Integrar ICA en dashboard con colores y clasificación oficial
5. Validar con datos históricos y boletines MITECO

**Referencia normativa**: Orden TEC/351/2019 + Resolución 02/09/2020

---

## Publicaciones y Referencias

Este proyecto utiliza datos abiertos de:
- [Portal Open Data Madrid](https://datos.madrid.es)
- [AEMET OpenData](https://opendata.aemet.es)
- Estándares OMS 2021 para calidad del aire

**Metodología inspirada en**:
- Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5-32.
- Roberts et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40(8), 913-929.

---

## Contribuciones

Las contribuciones son bienvenidas mediante pull requests:

1. Fork del repositorio
2. Crear rama feature (`git checkout -b feature/nueva-funcionalidad`)
3. Commit cambios (`git commit -m 'Descripción concisa'`)
4. Push a rama (`git push origin feature/nueva-funcionalidad`)
5. Abrir Pull Request

---

## Licencia y Contacto

**Repositorio**: [github.com/michal0091/madrid-air-quality-system](https://github.com/michal0091/madrid-air-quality-system)

**Issues**: Para reportar bugs o sugerir mejoras, usar [GitHub Issues](https://github.com/michal0091/madrid-air-quality-system/issues)

---

*Sistema de predicción de calidad del aire desarrollado con R, PostgreSQL y técnicas de machine learning espacial. 2015-2025.* ⭐
