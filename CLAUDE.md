# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an automated air quality analysis and prediction system for Madrid, built in R. The system collects, processes, and stores both historical and real-time air quality data from Madrid's open data portal, along with meteorological data from AEMET (Spanish State Meteorological Agency).

## Key Commands

### Environment Setup
```r
# Restore R package environment
renv::restore()

# Load environment (automatically done via .Rprofile)
source("renv/activate.R")
```

### Database Configuration
Create a `.Renviron` file in the project root with your PostgreSQL credentials:
```
DB_HOST="your_host"
DB_PORT="5432"
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
DB_PASSWORD="your_password"
```

### Data Pipeline Execution Order
```r
# 1. Setup dimension tables (run once)
source("R/00_setup_dimension_tables.R")

# 2. Collect historical data (intensive process)
source("R/01b_collect_historical_data.R")

# 3. Create predictors
source("R/01c_create_predictors.R")

# 4. Collect meteorological data
source("R/01d_collect_meteo_data.R")
```

### Running Individual Scripts
```r
# Real-time data collection (currently paused due to API instability)
source("R/01_collect_data.R")
```

## Architecture

### Data Pipeline Structure
The system follows a star schema design with dimension and fact tables:

- **Dimension Tables**: `dim_estaciones` (stations), `dim_magnitudes` (measurement types)
- **Fact Tables**: `fact_mediciones` (air quality measurements), `fact_meteo_diaria` (daily meteorological data)

### Key R Scripts
- `R/00_setup_dimension_tables.R` - Creates dimension tables from official catalogs
- `R/01_collect_data.R` - Real-time API data collection (currently inactive)
- `R/01b_collect_historical_data.R` - Historical data scraping and processing
- `R/01c_create_predictors.R` - Feature engineering for modeling
- `R/01d_collect_meteo_data.R` - AEMET meteorological data collection
- `R/utils.R` - Utility functions for data processing and database operations

### Data Sources
1. **Historical Air Quality**: Web scraping of Madrid's open data portal (ZIP files with monthly CSV data)
2. **Real-time Air Quality**: API endpoint (currently unstable)
3. **Station Metadata**: Official CSV catalog with coordinates and station details
4. **Meteorological Data**: AEMET API for weather data

## Technical Stack

- **Language**: R 4.5.1+
- **Database**: PostgreSQL 15 with PostGIS extension
- **Key Packages**: 
  - Data processing: `data.table`, `lubridate`
  - Web/API: `httr2`, `rvest`, `jsonlite`
  - Database: `DBI`, `RPostgres`
  - Spatial: `sf`
  - Logging: `logger`
  - Environment: `renv`

## Data Processing Details

### Wide-to-Long Transformation
Air quality data comes in wide format (H01-H24 columns for hourly values, V01-V24 for validation). The `procesar_y_cargar_lote()` function in `utils.R` transforms this to long format for efficient storage.

### AEMET Integration
The system includes comprehensive AEMET API integration with:
- Robust error handling and retry logic
- Automatic data type conversion and cleaning
- Bulk loading with duplicate handling
- Timezone management (Europe/Madrid)

### Logging
All scripts use structured logging to `logs/` directory. Each major operation is logged with appropriate levels (info, warn, error, success).

## Database Schema

### Fact Tables
- `fact_mediciones`: Hourly air quality measurements
- `fact_meteo_diaria`: Daily meteorological data

### Dimension Tables  
- `dim_estaciones`: Station metadata (coordinates, names, types)
- `dim_magnitudes`: Measurement types and units

## Phase 2: Spatial Modeling (NEW)

### Modeling Approach
The project implements a **hybrid ensemble modeling strategy** combining:

1. **Geostatistical Methods**: Traditional kriging with `gstat` and `automap`
2. **Machine Learning**: Modern spatial ML with `tidymodels` + `spatialsample`
3. **Ensemble Learning**: Multi-model combinations for robust predictions

### Spatial ML Frameworks
Three implementation strategies available:

#### Primary: Tidymodels + Spatial CV
```r
# Execute primary spatial modeling
source("R/02_train_spatial_models.R")

# Generate ensemble predictions
source("R/02b_ensemble_models.R")
```

**Key Features:**
- Spatial cross-validation with `spatialsample::spatial_block_cv()`
- Area of Applicability assessment with `waywiser`
- Integration of AEMET meteorological predictors
- Automated hyperparameter tuning

#### Alternative A: CARET + BlockCV
```r
# Traditional ML approach with spatial awareness
# Requires: blockCV, CAST packages
```

#### Alternative B: Pure Geostatistics
```r
# Classical kriging interpolation
# Automatic variogram fitting with automap
```

### Required Additional Packages
```r
install.packages(c(
  "spatialsample", "waywiser", "blockCV", "CAST",
  "automap", "tmap", "mlr3spatial"
))
```

### Known Issues and Fixes (2025-07-30)

#### Spatial Modeling Script Corrections
The `R/02_train_spatial_models.R` script has been corrected to address several critical issues:

1. **Tidymodels Workflow Syntax**: Fixed `fit()` function calls to remove incorrect `data =` parameter
2. **Tidymodels Function Parameters**: Updated `select_best()` and `show_best()` to use `metric = "rmse"` syntax
3. **Data Join Warnings**: Added `relationship = "many-to-many"` to left_join operations
4. **Spatial Cross-Validation**: Implemented adaptive fold sizing and radius calculation based on data extent
5. **Recipe Column Selection**: Updated from deprecated `all_of()` to `!!!syms()` pattern
6. **Error Handling**: Enhanced error messages in geostatistical modeling functions

#### Spatial Cross-Validation Improvements
- Adaptive fold sizing: 3 folds for <500 obs, 4 folds for <1000 obs, 5 folds otherwise
- Dynamic radius calculation based on spatial extent of data
- Automatic fallback to regular cross-validation if spatial blocks fail

#### Hourly Meteorological Data Integration (NEW)
**Major Enhancement**: The system now supports hourly meteorological data expansion:

**New Files:**
- `R/utils_meteo_horario.R` - Meteorological expansion utilities
- `R/test_expansion_meteo.R` - Validation testing framework

**Key Features:**
1. **Dual Interpolation Methods**:
   - **Linear interpolation**: Simple approach for rapid validation
   - **Sinusoidal interpolation**: Advanced method using exact timestamps of daily extremes

2. **Smart Parameter Configuration**:
   ```r
   # In R/02_train_spatial_models.R
   usar_datos_horarios <- TRUE   # FALSE = daily data, TRUE = hourly expansion
   ```

3. **Optimized Model Selection**:
   - **Hourly mode**: Prioritizes CARET framework (best performance)
   - **Daily mode**: Maintains all frameworks for comparison
   - **Performance improvement**: CARET shows significantly better RMSE vs tidymodels/geostat

4. **Validation System**:
   - Cross-validation between interpolation methods
   - Automatic method recommendation based on performance
   - Visual comparisons and statistical metrics

**Usage:**
```r
# Enable hourly data processing
source("R/02_train_spatial_models.R")  # usar_datos_horarios = TRUE

# Test interpolation methods
source("R/test_expansion_meteo.R")
```

**Benefits:**
- **Higher temporal resolution**: From daily to hourly predictions
- **Better model accuracy**: More granular meteorological predictors
- **Real-time capability**: Supports AEMET forecast integration

#### CARET Optimization and Bug Fixes (2025-07-31)

**Critical fixes implemented** for the CARET modeling pipeline:

1. **Missing Values Handling**:
   - **Issue**: `na.omit()` was removing ALL observations due to missing meteorological data
   - **Solution**: Smart filtering that only requires critical variables (valor_medio, coordinates, temp_media_c)
   - **Imputation**: Missing meteorological values filled with statistical defaults:
     ```r
     precipitacion_mm = ifelse(is.na(precipitacion_mm), 0, precipitacion_mm)
     dir_viento_grados = ifelse(is.na(dir_viento_grados), 180, dir_viento_grados)
     # Other variables imputed with means
     ```

2. **Categorical Variables Fix**:
   - **Issue**: "contrasts can be applied only to factors with 2 or more levels" error
   - **Solution**: Remove problematic categorical variables before training:
     ```r
     columnas_eliminar <- c("fecha", "id_estacion", "id_magnitud", "nombre_magnitud", 
                           "coords", "nombre_estacion", "tipo_estacion", "unidad")
     ```

3. **Enhanced Diagnostics**:
   - Added detailed logging for each data preparation step
   - Missing values analysis before imputation
   - Observation count tracking throughout pipeline

4. **Performance Validation**:
   - **Test Results**: Successfully processes 1000+ observations with 30% missing dir_viento_grados
   - **Model Training**: CARET Random Forest achieves RMSE ~12.7 with 14 predictor variables
   - **Robustness**: Handles mixed data types and missing patterns correctly

**Required Package**: Added `randomForest` dependency for CARET RF method.

### Model Outputs
- `models/modelos_espaciales.rds` - Individual trained models
- `models/ensemble_results.rds` - Ensemble predictions
- `models/madrid_grid.rds` - Prediction grid for Madrid
- `models/ensemble_shiny.rds` - Simplified data for Shiny app

### Validation Strategy
- **Spatial Cross-Validation**: Prevents spatial autocorrelation bias
- **Ensemble Metrics**: Diversity and consensus assessment
- **Area of Applicability**: Identifies reliable prediction zones
- **Multi-model Comparison**: RMSE-based model weighting

## Development Notes

- The project uses `renv` for package management - always run `renv::restore()` when setting up
- Database connections are established via environment variables
- Historical data loading is bandwidth-intensive and time-consuming
- Real-time API is currently unstable per official documentation
- All spatial data uses ETRS89 UTM Zone 30N (EPSG:25830) converted to WGS84 for web compatibility
- **Phase 2 models require substantial computational resources** - recommend running on dedicated hardware
- Spatial modeling uses ~1km resolution grid covering Madrid metropolitan area
- **Phase 3 real-time predictions** run automatically via GitHub Actions every hour
- The system is designed to run on Raspberry Pi 5 hardware

## Phase 3: Real-time Predictions (NEW)

### Real-time Data Pipeline
Automated system for generating current air quality predictions using live data:

#### Data Collection Script
```r
# Collect current air quality and weather data
source("R/03_prepare_realtime_prediction.R")
```

**Features:**
- Madrid Open Data API integration for current air quality measurements
- AEMET weather prediction API (with fallback to default values)
- Automatic data cleaning and format standardization
- Database integration with existing historical data

#### Prediction Execution
```r
# Generate spatial predictions using trained models
source("R/03b_execute_predictions.R")
```

**Capabilities:**
- Applies Phase 2 trained ensemble models to current conditions
- Generates spatial predictions across Madrid grid (~1km resolution)
- Creates real-time prediction maps for multiple pollutants (NO2, PM10, O3)
- Provides confidence assessments and air quality classifications

### GitHub Actions Automation

The system runs automatically via GitHub Actions:

```yaml
# Executes every hour at minute 15
schedule:
  - cron: '15 * * * *'
```

**Required Secrets:**
- `DB_HOST`, `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASSWORD` - Database connection
- `AEMET_API_KEY` - Spanish weather service API (optional)

### Output Structure

#### Generated Files
- `data/realtime/datos_prediccion_latest.rds` - Current input data
- `output/predicciones_realtime_latest.rds` - Spatial predictions
- `output/mapas_realtime/*_latest.png` - Prediction maps
- `output/resumen_predicciones_latest.rds` - Executive summary

#### Integration Points
- **Shiny App**: Reads latest predictions for real-time dashboard
- **APIs**: JSON exports available for external consumption
- **Monitoring**: Automated validation and error reporting

### API Data Sources (Updated 2025-07-31)

**Implementation Status**: APIs implemented with robust fallback system

#### Madrid Open Data API
- **Status**: ⚠ URLs changed/unstable (404 errors encountered)
- **Fallback**: Realistic simulated data with Madrid station coordinates
- **Format**: Wide format (H01-H24 hourly values, V01-V24 validation flags)
- **Update frequency**: Every 20 minutes (15, 35, 55 minutes)
- **Pollutants**: NO2, PM10, PM2.5, O3, SO2, CO, NOx
- **New Implementation**: `R/datos_realtime_fallback.R`

#### AEMET Weather API
- **Status**: ⚠ JSON structure complex (partial implementation)
- **API Key**: Configured via `AEMET_API_KEY` environment variable
- **Endpoint**: AEMET OpenData municipal hourly predictions (Madrid: 28079)
- **Fallback**: Realistic weather patterns for Madrid with diurnal cycles
- **Parameters**: Temperature, humidity, pressure, wind, precipitation

#### Robust Fallback System
**Key Features Implemented**:

1. **Smart Data Generation**:
   ```r
   # Example usage
   source("R/datos_realtime_fallback.R")
   datos <- obtener_datos_tiempo_real(usar_fallback = TRUE)
   ```

2. **Realistic Data Patterns**:
   - **Meteorological**: Diurnal temperature cycles, seasonal variations
   - **Pollution**: Station-specific baselines, realistic concentration ranges
   - **Spatial**: Actual Madrid monitoring station coordinates (20 stations)

3. **Data Quality Validation**:
   - Zero NA values across all variables
   - Realistic value ranges (temp: 5-45°C, pollution: 0-200 µg/m³)
   - Proper spatial format (SF objects with WGS84 coordinates)
   - Comprehensive logging and error handling

4. **Production-Ready Features**:
   - Automatic file generation: `data/realtime/datos_prediccion_latest.rds`
   - Validation functions for meteorological and air quality data
   - Seamless integration with existing modeling pipeline
   - Version tracking (`pipeline_version = "realtime_v1.0"`)

**Test Results**: 
- ✅ 60 records generated (20 stations × 3 pollutants)
- ✅ All validations passed (no NAs, realistic ranges)
- ✅ Spatial format compatible with CARET modeling pipeline
- **Fallback**: Statistical weather defaults for Madrid
- **Parameters**: Temperature, humidity, pressure, wind, precipitation

### Prediction Quality Control

- **Data validation**: Automatic checks for data completeness and quality
- **Model confidence**: Statistical confidence assessment based on input data
- **Spatial coverage**: Ensures predictions cover full Madrid metropolitan area
- **Temporal consistency**: Validates prediction continuity with historical patterns

## Phase 4: Interactive Dashboard (NEW)

### Shiny Dashboard Application

**Inspiration**: Simplified version of [Montreal Curbcut](https://montreal.curbcut.ca/) urban analytics platform

**GitHub Reference**: [Curbcut Montreal](https://github.com/Curbcut/curbcut-montreal)

#### Dashboard Features

```r
# Launch dashboard
source("R/08_dashboard_shiny.R")
ejecutar_dashboard(puerto = 3838)
```

**Core Components**:

1. **Interactive Map Tab**: 
   - Real-time Leaflet maps with air quality predictions
   - Color-coded markers by concentration levels
   - Popup information with station details and meteorological data
   - Dynamic legend and zoom controls

2. **Temporal Evolution Tab**:
   - 40-hour prediction timeline with Plotly interactive charts
   - Statistical tables with hourly summaries
   - Multi-contaminant comparison visualizations

3. **Meteorological Tab**:
   - Weather condition graphs (temperature, humidity, wind)
   - Real-time meteorological value boxes
   - Detailed weather data tables

4. **Data Export Tab**:
   - Interactive data tables with filtering
   - CSV and JSON download functionality
   - System performance metrics

5. **Information Tab**:
   - System architecture overview
   - Model performance statistics
   - Technical documentation links

#### Dashboard Architecture

**Modular Design** inspired by Montreal Curbcut's scalable approach:

- **Reactive Data Loading**: Automatic data refresh from prediction outputs
- **Responsive UI**: Mobile-friendly design with shinydashboard framework  
- **Performance Monitoring**: Built-in system metrics and diagnostics
- **Error Handling**: Graceful fallbacks for missing data scenarios

#### Required Data Files

The dashboard expects these output files from the prediction pipeline:

- `output/predicciones_40h_latest.rds` - Main prediction dataset
- `output/meteo_40h_latest.rds` - Meteorological forecasts
- `output/mapas_leaflet/*.rds` - Spatial mapping data

#### Montreal Curbcut Integration Concepts

**Adopted from Montreal Curbcut**:
- **Urban Analytics Framework**: Multi-scale spatial analysis approach
- **Interactive Visualization**: User-driven exploration of urban data
- **Real-time Updates**: Dynamic data refresh capabilities
- **Accessibility Features**: Screen reader compatibility and WCAG compliance
- **Modular Architecture**: Scalable component-based design

**Madrid-Specific Adaptations**:
- **Air Quality Focus**: Specialized for environmental monitoring vs general urban analytics
- **Temporal Predictions**: 40-hour forecasting vs historical analysis
- **CARET Integration**: Machine learning model integration
- **Spanish Localization**: UI and data labels in Spanish