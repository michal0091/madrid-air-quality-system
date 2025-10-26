# Sistema de Predicción de Calidad del Aire de Madrid (v2.0)

[![R Version](https://img.shields.io/badge/R-4.5.1+-blue.svg)](https://www.r-project.org/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue.svg)](https://www.postgresql.org/)
[![Machine Learning](https://img.shields.io/badge/ML-XGBoost_(Native)-brightgreen.svg)](https://xgboost.readthedocs.io/)
[![GitHub Actions](https://img.shields.io/github/actions/workflow/status/michal0091/madrid-air-quality-system/daily-predictions.yml?branch=main&label=Daily%20Predictions)](https://github.com/michal0091/madrid-air-quality-system/actions)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Release](https://img.shields.io/github/v/release/michal0091/madrid-air-quality-system?include_prereleases)](https://github.com/michal0091/madrid-air-quality-system/releases)

## Descripción

Sistema automatizado de análisis y predicción de calidad del aire para Madrid. Usa **XGBoost nativo** con **data.table** para eficiencia máxima. Integra datos históricos (2015-2025) y tiempo real con variables meteorológicas para predicciones horarias (40 horas).

### Características principales (v2.0)

- **Modelos XGBoost**: 5 contaminantes ICA (NO₂, PM10, PM2.5, O₃, SO₂) con GPU support
- **Eficiencia**: Modelos 16 MB total (99% reducción vs versión anterior)
- **Predicción espacial**: 16 estaciones de Madrid, resolución horaria
- **Automatización**: GitHub Actions (ingesta, predicción, despliegue diario)
- **Dashboard Shiny**: Mapas interactivos, animaciones temporales, análisis evolutivo

---

## Arquitectura del Sistema

### 1. Ingesta de Datos 📥

**Fuentes**:
- Históricos: Portal Open Data Madrid (2015-2025), AEMET
- Tiempo real: API XML Madrid (19 estaciones, 10 contaminantes, actualización cada 20 min)
- Meteorología: AEMET OpenData (predicciones horarias)

**Almacenamiento**: PostgreSQL 15 + PostGIS (esquema estrella: `dim_estaciones`, `dim_magnitudes`, `fact_mediciones`, `fact_meteo_diaria`)

**Procesamiento**: Scripts R con `data.table` (operaciones zero-copy, modificación por referencia)

### 2. Modelado Predictivo (XGBoost Nativo) 🚀

**Algoritmo**: `xgboost` con `xgb.cv` (calibración) + `xgb.train` (modelo final)

**Configuración**:
- 5 modelos independientes (NO₂, PM10, PM2.5, O₃, SO₂)
- Tree method: `hist` (CPU) o `gpu_hist` (GPU con CUDA)
- Feature engineering: coordenadas UTM, meteorología (VPD, ratios temp/hum), temporales, baseline estacional
- Entrenamiento: 100% `data.table` syntax

**Hardware recomendado para training**:
- CPU: 16+ cores, 32+ GB RAM (~5 min total)
- GPU: CUDA-capable (RTX 4070 Ti tested), 12+ GB VRAM (~2 min total)

**Tamaño modelos**: 16 MB total (vs 1.5 GB versión Ranger anterior)

### 3. Predicciones Operacionales ⚡

**Horizonte**: 40 horas, resolución horaria

**Output**: ~3,200 predicciones (16 estaciones × 5 contaminantes × 40 horas)

**Archivos generados**:
- `output/predicciones_xgb_nativo_40h_latest.rds` - Predicciones con geometría SF
- `output/meteo_40h_latest.rds` - Variables meteorológicas usadas

**Automatización**: GitHub Actions diario (4:00 AM UTC)

### 4. Dashboard Interactivo 📊

**Framework**: Shiny + Leaflet + Plotly + ggplot2

**Funcionalidades**:
- Mapas interactivos con burbujas de concentración
- Animaciones temporales (10 timesteps estratégicos)
- Gráficos evolutivos por estación
- Exportación datos (CSV, JSON)

**Deployment**: shinyapps.io (actualización automática diaria)

---

## Stack Tecnológico

- **Lenguaje**: R 4.5.1+
- **Paquetes clave**:
  - ML: `xgboost` (native API)
  - Data: `data.table`, `lubridate`
  - Spatial: `sf`, `lwgeom`
  - Viz: `shiny`, `leaflet`, `plotly`, `ggplot2`, `ggrepel`, `mapSpain`
  - API/DB: `httr2`, `rvest`, `DBI`, `RPostgres`, `xml2`
- **Infraestructura**:
  - BD: PostgreSQL 15 + PostGIS
  - CI/CD: GitHub Actions
  - Deploy: shinyapps.io
  - Deps: `renv`

---

## Estructura del Repositorio

```
madrid-air-quality-system/
├── R/                      # Scripts R (data.table style)
│   ├── 01_setup_dimension_tables.R
│   ├── 02_collect_historical_data.R
│   ├── 03_create_predictors.R
│   ├── 04_collect_meteo_data.R
│   ├── 05_crear_baseline_estacional.R
│   ├── 06_modelo_xgboost_ica.R        # XGBoost training
│   ├── 07_predicciones_horarias.R     # XGBoost predictions
│   ├── 08_actualizar_baseline.R
│   ├── api_madrid_real.R
│   ├── meteo_forecast.R
│   └── utils.R
├── app/                    # Shiny dashboard
│   ├── global.R, ui.R, server.R
│   ├── R/                  # Helper functions
│   └── data/               # Prediction data for app
├── models/                 # Trained models
│   ├── xgboost_nativo_ica_*.model     # XGBoost native format (16 MB total)
│   └── xgboost_nativo_ica_metricas.rds
├── output/                 # Generated predictions
│   ├── predicciones_xgb_nativo_40h_latest.rds
│   └── meteo_40h_latest.rds
├── .github/workflows/
│   ├── daily-predictions.yml          # Daily pipeline (4:00 AM UTC)
│   └── build-docker.yml               # Docker image build
├── Dockerfile              # R + XGBoost + dependencies
├── install_packages.R      # Package installation script
├── .Renviron.example       # Environment variables template
└── renv.lock               # Exact dependencies
```

---

## Instalación y Uso

### 1. Clonar Repositorio

```bash
git clone https://github.com/michal0091/madrid-air-quality-system.git
cd madrid-air-quality-system
```

### 2. Variables de Entorno

Copiar `.Renviron.example` a `.Renviron` y configurar:

```env
# .Renviron
DB_HOST="tu_host"
DB_PORT="5432"
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
DB_PASSWORD="tu_password"
AEMET_API_KEY="tu_api_key"  # Opcional para R/04
```

### 3. Dependencias R

```r
# Restaurar entorno exacto
renv::restore()

# Verificar instalación
library(data.table)
library(xgboost)
library(sf)
library(shiny)
```

### 4. Pipeline Local

```r
# --- Setup inicial (una vez) ---
source("R/01_setup_dimension_tables.R")    # Crea dim_estaciones, dim_magnitudes
source("R/03_create_predictors.R")          # Añade predictores espaciales

# --- Carga de datos (si faltan) ---
source("R/02_collect_historical_data.R")    # Calidad aire 2015-2025
source("R/04_collect_meteo_data.R")         # Meteorología AEMET
source("R/05_crear_baseline_estacional.R")  # Baseline estacional

# --- Entrenamiento XGBoost ---
# Recomendado: máquina con GPU o 16+ cores CPU
source("R/06_modelo_xgboost_ica.R")
# Output: models/xgboost_nativo_ica_*.model (16 MB total)
# Tiempo: ~2 min (GPU RTX 4070 Ti), ~5 min (CPU 16 cores)

# --- Generación de predicciones ---
source("R/07_predicciones_horarias.R")
# Output: output/predicciones_xgb_nativo_40h_latest.rds
# Tiempo: ~5 segundos
```

### 5. Dashboard

```r
# Lanzar localmente
shiny::runApp("app")
# Acceso: http://127.0.0.1:<PORT>
```

---

## Rendimiento de Modelos (XGBoost Nativo)

Métricas de validación cruzada (5-folds) con `xgb.cv`:

| Contaminante | RMSE CV (µg/m³) | Obs | nrounds | max_depth | eta | Tiempo CV (min) | Tiempo Train (seg) |
|---|---|---|---|---|---|---|---|
| **NO₂** | 15.49 | 4.36M | 200 | 8 | 0.05 | 2.08 | 4.6 |
| **PM10** | 10.84 | 2.28M | 200 | 8 | 0.05 | 1.21 | 2.9 |
| **PM2.5** | 5.63 | 1.26M | 200 | 8 | 0.05 | 0.80 | 2.0 |
| **O₃** | 14.86 | 2.47M | 200 | 8 | 0.05 | 1.35 | 3.1 |
| **SO₂** | 3.59 | 1.31M | 100 | 8 | 0.10 | 0.82 | 1.1 |

**Nota**: R² y MAE no reportados (xgb.cv optimiza RMSE por defecto). RMSE CV es métrica principal de selección de hiperparámetros.

**Tiempos en GPU** (RTX 4070 Ti, CUDA 12.0, `tree_method='hist'`, `device='cuda'`)

---

## Automatización GitHub Actions

**Workflow**: `.github/workflows/daily-predictions.yml`

**Schedule**: Diario a las 4:00 AM UTC (6:00 AM Madrid)

**Steps**:
1. **Pull Docker image** con XGBoost + dependencies
2. **Download models** desde GitHub Release (`.model` files)
3. **Collect real-time data** (API Madrid + AEMET)
4. **Generate predictions** (R/07_predicciones_horarias.R)
5. **Update dashboard** (deploy a shinyapps.io)

**Status**: Workflow reactivado tras migración XGBoost (anteriormente pausado por OOM con modelos Ranger)

---

## Migración v1.0 → v2.0 (Ranger → XGBoost)

### Mejoras principales

| Métrica | v1.0 (Ranger) | v2.0 (XGBoost) | Mejora |
|---|---|---|---|
| **Tamaño modelos** | 1.5 GB | 16 MB | **99% ↓** |
| **Memoria RAM** | 4-9 GB/modelo | 200 MB/modelo | **95% ↓** |
| **Training time** | ~15 min (CPU) | ~5 min (CPU), ~2 min (GPU) | **65-85% ↓** |
| **GitHub Actions** | ❌ OOM Error | ✅ Funcional | ✅ |
| **RMSE** | ~12-16 µg/m³ | ~4-15 µg/m³ | Comparable/mejor |

### Breaking changes

- Archivos modelos: `ranger_ica_*.rds` → `xgboost_nativo_ica_*.model`
- Output predictions: `predicciones_40h_latest.rds` → `predicciones_xgb_nativo_40h_latest.rds`
- Dependencias: Eliminado `caret`, `ranger` → Añadido `xgboost`

---

## Roadmap

### Fase Actual (v2.0)
- [x] Migración XGBoost nativo
- [x] Optimización data.table
- [x] Pipeline GitHub Actions funcional
- [x] Dashboard Shiny actualizado

### Próximas fases
- [ ] **ICA Oficial**: Implementar Índice Calidad Aire (BOE-A-2019-4494)
- [ ] **Alertas**: Notificaciones automáticas niveles desfavorables
- [ ] **Historial**: Base datos predicciones vs observaciones
- [ ] **API REST**: Endpoint público para consumo externo

---

## Contribuciones

Las contribuciones son bienvenidas. Por favor:
1. Fork el repositorio
2. Crea una rama para tu feature (`git checkout -b feature/nueva-funcionalidad`)
3. Commit cambios (`git commit -m 'Add nueva funcionalidad'`)
4. Push a la rama (`git push origin feature/nueva-funcionalidad`)
5. Abre un Pull Request

---

## Licencia

MIT License - Ver [LICENSE](LICENSE) para detalles.

---

## Contacto

- **GitHub**: [@michal0091](https://github.com/michal0091)
- **Proyecto**: [madrid-air-quality-system](https://github.com/michal0091/madrid-air-quality-system)
- **Dashboard**: [shinyapps.io](https://shinyapps.io) (URL específica en Actions)

---

*Sistema de predicción de calidad del aire v2.0 (XGBoost + data.table) - Madrid 2015-2025* ⭐
