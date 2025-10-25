# 📋 ORDEN CORRECTO DE EJECUCIÓN

**✅ CORREGIDO**: Los números ahora reflejan el orden real (2025-10-25)

## ✅ ORDEN CORRECTO

### 🏗️ FASE 1: SETUP (Una sola vez)
```bash
# 1. Crear tablas dimensión
Rscript R/01_setup_dimension_tables.R

# 2. Recolectar históricos 10 años (⏱️ 2-3h)
Rscript R/02_collect_historical_data.R

# 3. Agregar predictores espaciales (altitud, etc.)
Rscript R/03_create_predictors.R

# 4. Recolectar meteorología AEMET
Rscript R/04_collect_meteo_data.R

# 5. Crear baseline estacional (necesita paso 2,3,4)
Rscript R/05_crear_baseline_estacional.R
```

### 🤖 FASE 2: ENTRENAMIENTO
```bash
# 6. Entrenar modelos xgboost
Rscript R/06_modelo_xgboost_ica.R
```

### 🔄 FASE 3: PREDICCIONES (Diario)
```bash
# 7. Pronóstico meteorológico
Rscript R/meteo_forecast.R

# 8. Datos actuales Madrid
Rscript -e "source('R/datos_realtime_fallback.R'); saveRDS(obtener_datos_tiempo_real(), 'data/realtime/datos_prediccion_latest.rds')"

# 9. Predicciones 40h
Rscript R/07_predicciones_horarias.R
```

### 🔧 FASE 4: MANTENIMIENTO (Mensual)
```bash
# 10. Actualizar baseline
Rscript R/08_actualizar_baseline.R
```

## ✅ PROBLEMA RESUELTO

Los archivos fueron renombrados para que los prefijos reflejen el orden real:
- `00_` → `01_`
- `01b_` → `02_`
- `01c_` → `03_`
- `01d_` → `04_`
- `00b_` → `05_` ✅ (ahora refleja que va al final del setup)
- `02_` → `06_`
- `05_` → `07_`
- `06_` → `08_`

## 📊 Ver documentación completa

```bash
# Documento detallado (si docs/ no está en .gitignore)
cat docs/ORDEN_EJECUCION.md
```
