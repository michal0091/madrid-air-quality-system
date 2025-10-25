# 📋 ORDEN CORRECTO DE EJECUCIÓN

**IMPORTANTE**: Los números de prefijo NO reflejan el orden real

## ✅ ORDEN REAL

### 🏗️ FASE 1: SETUP (Una sola vez)
```bash
# 1. Crear tablas dimensión
Rscript R/00_setup_dimension_tables.R

# 2. Recolectar históricos 10 años (⏱️ 2-3h)
Rscript R/01b_collect_historical_data.R

# 3. Agregar predictores espaciales (altitud, etc.)
Rscript R/01c_create_predictors.R

# 4. Recolectar meteorología AEMET
Rscript R/01d_collect_meteo_data.R

# 5. Crear baseline estacional (necesita paso 2,3,4)
Rscript R/00b_crear_baseline_estacional.R
```

### 🤖 FASE 2: ENTRENAMIENTO
```bash
# 6. Entrenar modelos xgboost
Rscript R/02_modelo_xgboost_ica.R
```

### 🔄 FASE 3: PREDICCIONES (Diario)
```bash
# 7. Pronóstico meteorológico
Rscript R/meteo_forecast.R

# 8. Datos actuales Madrid
Rscript -e "source('R/datos_realtime_fallback.R'); saveRDS(obtener_datos_tiempo_real(), 'data/realtime/datos_prediccion_latest.rds')"

# 9. Predicciones 40h
Rscript R/05_predicciones_horarias.R
```

### 🔧 FASE 4: MANTENIMIENTO (Mensual)
```bash
# 10. Actualizar baseline
Rscript R/06_actualizar_baseline.R
```

## 🚨 PROBLEMA: Prefijo 00b está MAL

```
00b_crear_baseline_estacional.R
```
Tiene prefijo `00b` PERO necesita datos de `01b`, `01c`, `01d`
Debería ser `01e` o `04`

## 📊 Ver documentación completa

```bash
# Documento detallado (si docs/ no está en .gitignore)
cat docs/ORDEN_EJECUCION.md
```
