# 📋 Orden Correcto de Ejecución de Scripts

**Última actualización**: 2025-10-25
**Importante**: Los números de prefijo NO reflejan el orden real de ejecución

---

## 🚨 PROBLEMAS IDENTIFICADOS

### 1. Prefijo 00b Incorrecto
- `00b_crear_baseline_estacional.R` tiene prefijo `00b`
- ❌ Sugiere que va después de `00`
- ✅ **PERO** necesita datos de `01b` (históricos)

### 2. Salto de Numeración (02 → 05)
- Scripts `03` y `04` fueron archivados/borrados
- Causa confusión sobre el orden

### 3. Script 01c Obsoleto
- `01c_create_predictors.R` mencionado en docs
- Ya NO existe (feature engineering ahora es inline en entrenamiento)

---

## ✅ ORDEN REAL DE EJECUCIÓN

### 🏗️ FASE 1: SETUP INICIAL (Una sola vez)

```bash
# 1. Crear tablas dimensión (estaciones, magnitudes)
Rscript R/00_setup_dimension_tables.R

# 2. Recolectar datos históricos 10 años (⏱️ ~2-3 horas)
Rscript R/01b_collect_historical_data.R

# 3. Recolectar datos meteorológicos AEMET
Rscript R/01d_collect_meteo_data.R

# 4. Crear baseline estacional (requiere paso 2 y 3)
Rscript R/00b_crear_baseline_estacional.R
```

**Orden correcto**: `00 → 01b → 01d → 00b`

**Dependencias**:
- `00`: Ninguna (primer script)
- `01b`: Necesita tablas de `00`
- `01d`: Independiente (API AEMET)
- `00b`: Necesita datos de `01b` y `01d`

---

### 🤖 FASE 2: ENTRENAMIENTO (Cuando cambien modelos)

```bash
# 5. Entrenar modelos xgboost con GPU
Rscript R/02_modelo_xgboost_ica.R
```

**Dependencias**: Necesita FASE 1 completa
- Requiere: `fact_mediciones`, `fact_meteo_diaria`, `dim_baseline_estacional`
- Genera: `models/xgboost_ica_*.model` (5 modelos ICA)
- Tiempo estimado: ~15-20 minutos (con GPU)

---

### 🔄 FASE 3: PREDICCIONES (Diario/Cada hora)

```bash
# 6. Obtener pronóstico meteorológico AEMET
Rscript R/meteo_forecast.R

# 7. Obtener datos actuales Madrid (API real)
Rscript -e "
source('R/datos_realtime_fallback.R')
datos <- obtener_datos_tiempo_real(usar_fallback = FALSE)
saveRDS(datos, 'data/realtime/datos_prediccion_latest.rds')
"

# 8. Generar predicciones 40h
Rscript R/05_predicciones_horarias.R
```

**Dependencias**:
- Necesita modelos de FASE 2
- Necesita `dim_baseline_estacional`
- Necesita datos meteorológicos actuales

**Genera**: `output/predicciones_40h_latest.rds`

---

### 🔧 FASE 4: MANTENIMIENTO (Mensual)

```bash
# 9. Actualizar baseline con nuevos datos
Rscript R/06_actualizar_baseline.R
```

**Dependencias**: `fact_mediciones` con datos nuevos
**Actualiza**: `dim_baseline_estacional`

---

## 📂 Scripts Sin Prefijo Numérico

Estos son **auxiliares** y no tienen orden de ejecución:

| Script | Tipo | Cuándo se usa |
|--------|------|---------------|
| `api_madrid_real.R` | Función | Llamada por `datos_realtime_fallback.R` |
| `datos_realtime_fallback.R` | Función | FASE 3 (predicciones) |
| `meteo_forecast.R` | Ejecutable | FASE 3 (antes de predicciones) |
| `utils.R` | Funciones | Cualquier momento |
| `utils_meteo_horario.R` | Funciones | FASE 2 (entrenamiento) |

---

## 🎯 Flujo Completo Visual

```
┌─────────────────────────────────────────────┐
│  FASE 1: SETUP (Una sola vez)               │
├─────────────────────────────────────────────┤
│  00_setup_dimension_tables.R                │
│           ↓                                 │
│  01b_collect_historical_data.R (2-3h)       │
│           ↓                                 │
│  01d_collect_meteo_data.R                   │
│           ↓                                 │
│  00b_crear_baseline_estacional.R            │
└─────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────┐
│  FASE 2: ENTRENAMIENTO (Ocasional)          │
├─────────────────────────────────────────────┤
│  02_modelo_xgboost_ica.R (15-20 min)        │
└─────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────┐
│  FASE 3: PREDICCIONES (Diario)              │
├─────────────────────────────────────────────┤
│  meteo_forecast.R                           │
│           ↓                                 │
│  datos_realtime_fallback.R                  │
│           ↓                                 │
│  05_predicciones_horarias.R                 │
└─────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────┐
│  FASE 4: MANTENIMIENTO (Mensual)            │
├─────────────────────────────────────────────┤
│  06_actualizar_baseline.R                   │
└─────────────────────────────────────────────┘
```

---

## 🔄 Propuesta de Renombrado (Opcional)

Si quisieras que los números reflejen el orden real:

```bash
# FASE 1: SETUP
01_setup_dimension_tables.R          # era 00
02_collect_historical_data.R         # era 01b
03_collect_meteo_data.R              # era 01d
04_crear_baseline_estacional.R       # era 00b

# FASE 2: ENTRENAMIENTO
05_modelo_xgboost_ica.R              # era 02

# FASE 3: PREDICCIONES
06_predicciones_horarias.R           # era 05

# FASE 4: MANTENIMIENTO
07_actualizar_baseline.R             # era 06
```

**⚠️ Advertencia**: Renombrar rompería referencias en:
- GitHub Actions workflows
- Documentación existente (CLAUDE.md, README.md)
- Scripts que hacen `source("R/00...")`

---

## 💡 Recomendaciones

### OPCIÓN A: Dejar como está + Documentar
- ✅ No rompe nada
- ✅ Esta documentación aclara el orden
- ❌ Confusión permanente para nuevos colaboradores

### OPCIÓN B: Renombrar todo
- ✅ Claridad total
- ❌ Rompe referencias en workflows y docs
- ❌ Requiere actualizar múltiples archivos

### OPCIÓN C: Script maestro (RECOMENDADA)
Crear scripts que ejecuten las fases completas:

```bash
# run_setup_complete.R
source("R/00_setup_dimension_tables.R")
source("R/01b_collect_historical_data.R")
source("R/01d_collect_meteo_data.R")
source("R/00b_crear_baseline_estacional.R")

# run_train_models.R
source("R/02_modelo_xgboost_ica.R")

# run_daily_predictions.R
source("R/meteo_forecast.R")
source("R/datos_realtime_fallback.R")
source("R/05_predicciones_horarias.R")
```

---

## 📝 Notas Adicionales

### Script 01c_create_predictors.R
- **Estado**: Mencionado en docs pero YA NO EXISTE
- **Razón**: Feature engineering ahora es inline en `02_modelo_xgboost_ica.R`
- **Acción**: Actualizar referencias en documentación

### Scripts Archivados
Los siguientes scripts fueron movidos a `.archive/backups_2025-10-25/`:
- `02_modelo_ranger_ica.R` (reemplazado por xgboost)
- `02_modelo_caret_avanzado.R` (obsoleto)
- `03_prediccion_espacial.R` (obsoleto)
- Y otros (ver `docs/LIMPIEZA_2025-10-25.md`)

---

## 🚀 Comando Rápido para Setup Completo

```bash
# Ejecutar setup completo desde cero
Rscript R/00_setup_dimension_tables.R && \
Rscript R/01b_collect_historical_data.R && \
Rscript R/01d_collect_meteo_data.R && \
Rscript R/00b_crear_baseline_estacional.R && \
echo "✅ Setup completo"
```

**Tiempo total estimado**: ~2-4 horas (principalmente por 01b)
