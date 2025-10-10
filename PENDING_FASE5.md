# TAREAS PENDIENTES - FASE 5: ICA OFICIAL

## Estado Actual (2025-10-10)

✅ **Completado**:
- Dashboard actualizado para mostrar 5 contaminantes ICA
- Modelos RANGER entrenados con UTM projection para todos los contaminantes
- Predicciones 40h generando datos para NO₂, PM10, PM2.5, O₃, SO₂
- Estándares OMS 2021 implementados en clasificación

⚠️ **Pendiente - Gráficos de Animación**:
- Faltan gráficos animados para PM2.5 y SO₂ en:
  - `app/www/horas/pm25_hora_*.png`
  - `app/www/horas/so2_hora_*.png`
  - `app/www/mapas_horas/mapa_pm25_hora_*.png`
  - `app/www/mapas_horas/mapa_so2_hora_*.png`

**Impacto**: El dashboard funciona, pero las animaciones para PM2.5 y SO₂ mostrarán imagen "no encontrada"

## FASE 5: IMPLEMENTAR ICA OFICIAL (BOE-A-2019-4494)

### Objetivo
Implementar el **Índice Nacional de Calidad del Aire** oficial español según normativa BOE-A-2019-4494 y Resolución 02/09/2020.

### Tareas Principales

#### 1. Obtener Tabla Oficial ICA
- [ ] Descargar PDF oficial de MITECO:
  - URL: https://www.miteco.gob.es/content/dam/miteco/es/calidad-y-evaluacion-ambiental/temas/atmosfera-y-calidad-del-aire/resolucion_02092020_modificacion_ica_tcm30-511596.pdf
- [ ] Extraer tabla con valores µg/m³ para cada categoría ICA
- [ ] Documentar categorías oficiales:
  - 🟢 Buena
  - 🟡 Razonablemente buena
  - 🟠 Regular
  - 🔴 Desfavorable
  - 🟣 Muy desfavorable
  - ⚫ Extremadamente desfavorable

#### 2. Crear Función Cálculo ICA
Archivo: `R/06_calcular_ica_oficial.R`

```r
# Calcular ICA por estación tomando el peor contaminante
calcular_ica_estacion <- function(datos_estacion) {
  # Calcular promedios según contaminante (1h, 8h, 24h)
  # Clasificar cada contaminante según tabla oficial
  # Retornar la peor categoría
}
```

**Requisitos**:
- Promedios móviles según contaminante:
  - NO₂: 1 hora
  - SO₂: 1 hora
  - O₃: 8 horas
  - PM10: 24 horas
  - PM2.5: 24 horas
- Tomar categoría más desfavorable como ICA de la estación

#### 3. Modificar Modelos Predictivos
- [ ] Verificar que modelo SO₂ existe (✅ Ya existe: `models/ranger_ica_Di_xido_de_Azufre.rds`)
- [ ] Integrar cálculo ICA en `R/05_predicciones_horarias.R`
- [ ] Agregar columna `ica_categoria` a predicciones

#### 4. Actualizar Dashboard con ICA
Archivo: `app/global.R`

```r
# Definir categorías ICA oficiales
CATEGORIAS_ICA <- list(
  "Buena" = list(color = "#00e400", icono = "🟢"),
  "Razonablemente buena" = list(color = "#ffff00", icono = "🟡"),
  "Regular" = list(color = "#ff7e00", icono = "🟠"),
  "Desfavorable" = list(color = "#ff0000", icono = "🔴"),
  "Muy desfavorable" = list(color = "#8f3f97", icono = "🟣"),
  "Extremadamente desfavorable" = list(color = "#7e0023", icono = "⚫")
)
```

Archivo: `app/ui.R`
- [ ] Agregar tab "Índice ICA" con:
  - Mapa de Madrid coloreado por ICA
  - Tabla de estaciones con categoría ICA
  - Alertas automáticas para niveles desfavorables

Archivo: `app/server.R`
- [ ] Renderizar visualizaciones ICA
- [ ] Implementar sistema de alertas

#### 5. Generar Gráficos con Límites ICA Oficiales

Script: `R/07_generar_graficos_ica.R`

**Para cada contaminante (NO₂, PM10, PM2.5, O₃, SO₂)**:
- [ ] Generar gráficos de estaciones con líneas de referencia ICA
- [ ] Generar mapas de Madrid con colores ICA
- [ ] Usar límites oficiales (no OMS 2021)
- [ ] Guardar en:
  - `app/www/horas/{contaminante}_hora_*.png`
  - `app/www/mapas_horas/mapa_{contaminante}_hora_*.png`

**Características gráficos**:
- 6 líneas horizontales con categorías ICA oficiales
- Colores según MITECO (no OMS)
- Títulos indicando "Límites ICA Oficial España"

#### 6. Validar con Datos Reales Madrid
- [ ] Comparar predicciones ICA con datos históricos
- [ ] Verificar coherencia con boletines oficiales Madrid
- [ ] Documentar casos de niveles desfavorables

### Archivos a Crear

```
R/06_calcular_ica_oficial.R          # Función cálculo ICA
R/07_generar_graficos_ica.R          # Gráficos con límites oficiales
data/ica_limites_oficiales.csv        # Tabla MITECO extraída
app/www/horas/pm25_hora_*.png        # 10 gráficos PM2.5
app/www/horas/so2_hora_*.png         # 10 gráficos SO₂
app/www/mapas_horas/mapa_pm25_*.png  # 10 mapas PM2.5
app/www/mapas_horas/mapa_so2_*.png   # 10 mapas SO₂
```

### Archivos a Modificar

```
app/global.R                   # Categorías ICA oficiales
app/ui.R                       # Nuevo tab ICA
app/server.R                   # Renderizado ICA + alertas
R/05_predicciones_horarias.R   # Integrar cálculo ICA
README.md                      # Documentar ICA implementado
```

### Referencias Normativas

- **BOE-A-2019-4494**: Orden TEC/351/2019 - Índice Nacional de Calidad del Aire
- **Resolución 02/09/2020**: Modificación tabla valores ICA
- **URL PDF**: https://www.miteco.gob.es/content/dam/miteco/es/calidad-y-evaluacion-ambiental/temas/atmosfera-y-calidad-del-aire/resolucion_02092020_modificacion_ica_tcm30-511596.pdf

### Prioridad Implementación

1. ⭐⭐⭐ **CRÍTICO**: Obtener tabla oficial límites ICA
2. ⭐⭐⭐ **CRÍTICO**: Generar gráficos PM2.5 y SO₂ con límites ICA
3. ⭐⭐ **ALTA**: Implementar función cálculo ICA
4. ⭐⭐ **ALTA**: Integrar ICA en predicciones
5. ⭐ **MEDIA**: Dashboard tab ICA + alertas
6. ⭐ **MEDIA**: Validación datos reales

### Notas Técnicas

- **Diferencia OMS vs ICA**: Los límites actuales usan OMS 2021, pero ICA oficial puede tener umbrales diferentes
- **Promedios móviles**: Requieren ventana temporal según contaminante
- **Peor categoría**: ICA estación = max(categorías de 5 contaminantes)
- **Colores oficiales**: Usar paleta MITECO, no OMS

---

**Última actualización**: 2025-10-10
**Estado dashboard**: ✅ Funcional con 5 contaminantes + estándares OMS 2021
**Próximo paso**: Obtener tabla oficial ICA y generar gráficos faltantes
