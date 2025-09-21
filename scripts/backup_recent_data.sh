#!/bin/bash

# BACKUP SELECTIVO - SOLO DATOS RECIENTES PARA CLOUD
# Migra últimos 10 años para entrenamiento + estructura completa

# Configuración
BACKUP_DIR="/home/pi/backups"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
BACKUP_FILE="$BACKUP_DIR/madrid_air_recent_backup_$DATE.sql"

# Fecha límite (10 años atrás)
FECHA_LIMITE=$(date -d '10 years ago' '+%Y-%m-%d')

mkdir -p $BACKUP_DIR

echo "🚀 Backup selectivo para migración cloud"
echo "📅 Fecha límite: $FECHA_LIMITE (últimos 10 años)"
echo "💾 Archivo: $BACKUP_FILE"

# Función para ejecutar psql con manejo de errores
run_psql() {
    local query="$1"
    local description="$2"

    echo "📋 $description..."
    psql -h localhost -U $DB_USER -d $DB_NAME -c "$query" >> "$BACKUP_FILE"

    if [ $? -eq 0 ]; then
        echo "✅ $description completado"
    else
        echo "❌ Error en: $description"
        exit 1
    fi
}

# Iniciar archivo SQL
cat > "$BACKUP_FILE" << 'EOF'
-- BACKUP SELECTIVO MADRID AIR QUALITY
-- Solo datos recientes para migración a cloud
-- Generado automáticamente

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

-- Crear base de datos si no existe
CREATE DATABASE air_quality_db WITH TEMPLATE = template0 ENCODING = 'UTF8';
\c air_quality_db;

EOF

echo "📝 Exportando estructura de tablas..."

# Exportar solo estructura (sin datos)
pg_dump -h localhost -U $DB_USER -d $DB_NAME \
  --schema-only \
  --no-owner \
  --no-privileges \
  >> "$BACKUP_FILE"

echo "📊 Exportando datos de dimensiones (completas)..."

# Exportar tablas de dimensiones completas (son pequeñas)
pg_dump -h localhost -U $DB_USER -d $DB_NAME \
  --data-only \
  --no-owner \
  --no-privileges \
  --table=dim_estaciones \
  --table=dim_magnitudes \
  >> "$BACKUP_FILE"

echo "📈 Exportando datos recientes de fact_mediciones..."

# Exportar solo mediciones de los últimos 10 años
cat >> "$BACKUP_FILE" << EOF

-- Datos recientes de fact_mediciones (últimos 10 años)
COPY fact_mediciones FROM stdin;
EOF

psql -h localhost -U $DB_USER -d $DB_NAME -c "
COPY (
    SELECT * FROM fact_mediciones
    WHERE fecha >= '$FECHA_LIMITE'
    ORDER BY fecha, id_estacion, id_magnitud
) TO STDOUT;
" >> "$BACKUP_FILE"

echo "\\." >> "$BACKUP_FILE"

echo "🌦️ Exportando datos meteorológicos recientes..."

# Exportar datos meteorológicos de los últimos 10 años
cat >> "$BACKUP_FILE" << EOF

-- Datos recientes de fact_meteo_diaria (últimos 10 años)
COPY fact_meteo_diaria FROM stdin;
EOF

psql -h localhost -U $DB_USER -d $DB_NAME -c "
COPY (
    SELECT * FROM fact_meteo_diaria
    WHERE fecha >= '$FECHA_LIMITE'
    ORDER BY fecha
) TO STDOUT;
" >> "$BACKUP_FILE"

echo "\\." >> "$BACKUP_FILE"

# Estadísticas del backup
echo ""
echo "📊 ESTADÍSTICAS DEL BACKUP SELECTIVO:"

# Contar registros que se migrarán
MEDICIONES_RECIENTES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT count(*) FROM fact_mediciones WHERE fecha >= '$FECHA_LIMITE';
")

METEO_RECIENTES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT count(*) FROM fact_meteo_diaria WHERE fecha >= '$FECHA_LIMITE';
")

echo "   Mediciones recientes: $(echo $MEDICIONES_RECIENTES | tr -d ' ') registros"
echo "   Datos meteorológicos: $(echo $METEO_RECIENTES | tr -d ' ') registros"
echo "   Estaciones: 24 registros"
echo "   Magnitudes: 14 registros"

# Comprimir backup
gzip "$BACKUP_FILE"
echo ""
echo "✅ Backup selectivo completado:"
echo "   📁 Archivo: ${BACKUP_FILE}.gz"
echo "   📏 Tamaño: $(du -h ${BACKUP_FILE}.gz | cut -f1)"

# Generar checksum
md5sum "${BACKUP_FILE}.gz" > "${BACKUP_FILE}.gz.md5"

echo ""
echo "🎯 BACKUP OPTIMIZADO PARA CLOUD:"
echo "   ✅ Solo datos necesarios para entrenamiento"
echo "   ✅ Debería caber en Neon Free (512MB)"
echo "   ✅ Mantiene capacidad predictiva completa"
echo ""
echo "📋 SIGUIENTE PASO:"
echo "   scp pi@tu-pi:${BACKUP_FILE}.gz ."
echo "   ./scripts/restore_to_cloud.sh ${BACKUP_FILE##*/}.gz"