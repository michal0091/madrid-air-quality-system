#!/bin/bash

# VERIFICAR TAMAÑO DE BASE DE DATOS POSTGRESQL
# Ejecutar en Raspberry Pi para ver si cabe en cloud (512MB Neon / 500MB Supabase)

# Configuración
DB_NAME="air_quality_db"
DB_USER="air_quality_user"

echo "🔍 ANÁLISIS DE TAMAÑO DE BASE DE DATOS"
echo "======================================"
echo "📊 Base de datos: $DB_NAME"
echo "👤 Usuario: $DB_USER"
echo "📅 Fecha: $(date)"
echo ""

# Tamaño total de la base de datos
echo "📏 TAMAÑO TOTAL DE LA BASE DE DATOS:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    pg_database.datname as database_name,
    pg_size_pretty(pg_database_size(pg_database.datname)) as size_pretty,
    pg_database_size(pg_database.datname) as size_bytes
FROM pg_database
WHERE datname = '$DB_NAME';
"

echo ""
echo "📊 TAMAÑO POR TABLA:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) as size_pretty,
    pg_total_relation_size(schemaname||'.'||tablename) as size_bytes,
    pg_size_pretty(pg_relation_size(schemaname||'.'||tablename)) as table_size,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename) - pg_relation_size(schemaname||'.'||tablename)) as index_size
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
"

echo ""
echo "📈 NÚMERO DE REGISTROS POR TABLA:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    'dim_estaciones' as tabla,
    count(*) as registros,
    pg_size_pretty(pg_total_relation_size('dim_estaciones')) as tamaño
FROM dim_estaciones
UNION ALL
SELECT
    'dim_magnitudes' as tabla,
    count(*) as registros,
    pg_size_pretty(pg_total_relation_size('dim_magnitudes')) as tamaño
FROM dim_magnitudes
UNION ALL
SELECT
    'fact_mediciones' as tabla,
    count(*) as registros,
    pg_size_pretty(pg_total_relation_size('fact_mediciones')) as tamaño
FROM fact_mediciones
UNION ALL
SELECT
    'fact_meteo_diaria' as tabla,
    count(*) as registros,
    pg_size_pretty(pg_total_relation_size('fact_meteo_diaria')) as tamaño
FROM fact_meteo_diaria
ORDER BY registros DESC;
"

echo ""
echo "📊 RESUMEN DE ÍNDICES:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    tablename,
    indexname,
    pg_size_pretty(pg_relation_size(indexname::regclass)) as index_size
FROM pg_indexes
WHERE schemaname = 'public'
ORDER BY pg_relation_size(indexname::regclass) DESC;
"

# Análisis de datos por fecha
echo ""
echo "📅 ANÁLISIS TEMPORAL DE DATOS:"

# Fecha límite para últimos 10 años
FECHA_10_ANOS=$(date -d '10 years ago' '+%Y-%m-%d')
echo "   Fecha límite (10 años): $FECHA_10_ANOS"

# Contar registros por período
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    'Todos los datos' as periodo,
    count(*) as registros,
    min(fecha_hora::date) as fecha_inicio,
    max(fecha_hora::date) as fecha_fin
FROM fact_mediciones
UNION ALL
SELECT
    'Últimos 10 años' as periodo,
    count(*) as registros,
    min(fecha_hora::date) as fecha_inicio,
    max(fecha_hora::date) as fecha_fin
FROM fact_mediciones
WHERE fecha_hora::date >= '$FECHA_10_ANOS'
ORDER BY registros DESC;
"

# Estimar tamaño de últimos 10 años
echo ""
echo "💾 ESTIMACIÓN DE TAMAÑO DE DATOS RECIENTES:"

# Obtener tamaño promedio por registro
TOTAL_REGISTROS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM fact_mediciones;")
TOTAL_SIZE_MEDICIONES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT pg_total_relation_size('fact_mediciones');")
REGISTROS_10_ANOS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM fact_mediciones WHERE fecha_hora::date >= '$FECHA_10_ANOS';")

# Calcular tamaño estimado
BYTES_POR_REGISTRO=$((TOTAL_SIZE_MEDICIONES / TOTAL_REGISTROS))
SIZE_10_ANOS=$((REGISTROS_10_ANOS * BYTES_POR_REGISTRO))

# Añadir tamaño de tablas pequeñas (dims + meteo reciente)
SIZE_DIMS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT
    pg_total_relation_size('dim_estaciones') +
    pg_total_relation_size('dim_magnitudes');
")

# Calcular tamaño de datos meteorológicos recientes
METEO_REGISTROS_10_ANOS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM fact_meteo_diaria WHERE fecha >= '$FECHA_10_ANOS';")
METEO_TOTAL_REGISTROS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM fact_meteo_diaria;")
METEO_TOTAL_SIZE=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT pg_total_relation_size('fact_meteo_diaria');")

if [ $METEO_TOTAL_REGISTROS -gt 0 ]; then
    METEO_BYTES_POR_REGISTRO=$((METEO_TOTAL_SIZE / METEO_TOTAL_REGISTROS))
    SIZE_METEO_10_ANOS=$((METEO_REGISTROS_10_ANOS * METEO_BYTES_POR_REGISTRO))
else
    SIZE_METEO_10_ANOS=0
fi

TOTAL_SIZE_10_ANOS=$((SIZE_10_ANOS + SIZE_DIMS + SIZE_METEO_10_ANOS))
COMPRESSED_SIZE_10_ANOS=$((TOTAL_SIZE_10_ANOS / 3))

echo "   Registros totales: $(echo $TOTAL_REGISTROS | tr -d ' ')"
echo "   Registros últimos 10 años: $(echo $REGISTROS_10_ANOS | tr -d ' ')"
echo "   Reducción: $(echo "scale=1; $(echo $REGISTROS_10_ANOS | tr -d ' ') * 100 / $(echo $TOTAL_REGISTROS | tr -d ' ')" | bc)%"
echo ""
echo ""
echo "   Tamaño fact_mediciones (10 años): $(echo $SIZE_10_ANOS | awk '{print int($1/1024/1024)} MB')"
echo "   + Dimensiones: $(echo $SIZE_DIMS | awk '{print int($1/1024/1024)} MB')"
echo "   + Meteo (10 años): $(echo $SIZE_METEO_10_ANOS | awk '{print int($1/1024/1024)} MB')"
echo "   = Total estimado: $(echo $TOTAL_SIZE_10_ANOS | awk '{print int($1/1024/1024)} MB')"
echo "   Comprimido (.gz): ~$(echo $COMPRESSED_SIZE_10_ANOS | awk '{print int($1/1024/1024)} MB')"

# Calcular tamaño del backup completo también
TOTAL_SIZE=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT pg_database_size('$DB_NAME');")
COMPRESSED_SIZE=$((TOTAL_SIZE / 3))

echo ""
echo "💾 COMPARACIÓN BACKUP COMPLETO vs SELECTIVO:"
echo "   Backup completo: $(echo $TOTAL_SIZE | awk '{print int($1/1024/1024)} MB') → ~$(echo $COMPRESSED_SIZE | awk '{print int($1/1024/1024)} MB') comprimido"
echo "   Backup 10 años: $(echo $TOTAL_SIZE_10_ANOS | awk '{print int($1/1024/1024)} MB') → ~$(echo $COMPRESSED_SIZE_10_ANOS | awk '{print int($1/1024/1024)} MB') comprimido"

echo ""
echo "🎯 COMPATIBILIDAD CON SERVICIOS CLOUD:"

echo ""
echo "📊 BACKUP COMPLETO:"
# Neon Free Tier: 512MB
if [ $TOTAL_SIZE -lt 536870912 ]; then  # 512MB en bytes
    echo "   ✅ Neon Free (512MB): Compatible"
else
    echo "   ❌ Neon Free (512MB): Excede límite"
fi

# Supabase Free Tier: 500MB
if [ $TOTAL_SIZE -lt 524288000 ]; then  # 500MB en bytes
    echo "   ✅ Supabase Free (500MB): Compatible"
else
    echo "   ❌ Supabase Free (500MB): Excede límite"
fi

echo ""
echo "📊 BACKUP SELECTIVO (10 AÑOS):"
# Neon Free Tier: 512MB
if [ $TOTAL_SIZE_10_ANOS -lt 536870912 ]; then  # 512MB en bytes
    echo "   ✅ Neon Free (512MB): Compatible"
else
    echo "   ❌ Neon Free (512MB): Excede límite"
fi

# Supabase Free Tier: 500MB
if [ $TOTAL_SIZE_10_ANOS -lt 524288000 ]; then  # 500MB en bytes
    echo "   ✅ Supabase Free (500MB): Compatible"
else
    echo "   ❌ Supabase Free (500MB): Excede límite"
fi

# PlanetScale Free Tier: 1GB
if [ $TOTAL_SIZE_10_ANOS -lt 1073741824 ]; then  # 1GB en bytes
    echo "   ✅ PlanetScale Free (1GB): Compatible"
else
    echo "   ❌ PlanetScale Free (1GB): Excede límite"
fi

echo ""
echo "💡 RECOMENDACIONES:"
if [ $TOTAL_SIZE_10_ANOS -lt 536870912 ]; then
    echo "   ✅ Usar backup selectivo (10 años) para Neon Free"
    echo "   ✅ Mantiene capacidad completa de entrenamiento"
    echo "   ✅ Ahorro significativo vs backup completo"
elif [ $TOTAL_SIZE_10_ANOS -lt 1073741824 ]; then
    echo "   📊 Backup selectivo cabe en servicios de 1GB+"
    echo "   💰 Considerar Neon Pro si quieres más espacio"
else
    echo "   💰 Necesitarás Neon Pro ($19/mes para 10GB)"
    echo "   🔄 O implementar archivado más agresivo (5-7 años)"
fi