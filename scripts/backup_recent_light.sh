#!/bin/bash

# BACKUP LIGERO - SOLO DATOS RECIENTES PARA CLOUD
# Migra últimos 2-3 años para operación diaria (NO para entrenamiento)

# Configuración
BACKUP_DIR="/home/pi/backups"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
BACKUP_FILE="$BACKUP_DIR/madrid_air_light_backup_$DATE.sql"

# Fecha límite (2 años para cloud ligero)
FECHA_LIMITE=$(date -d '2 years ago' '+%Y-%m-%d')

mkdir -p $BACKUP_DIR

echo "🚀 Backup ligero para cloud híbrido"
echo "📅 Fecha límite: $FECHA_LIMITE (últimos 2 años)"
echo "🎯 Propósito: Predicciones diarias, NO entrenamiento"
echo "💾 Archivo: $BACKUP_FILE"

# Iniciar archivo SQL
cat > "$BACKUP_FILE" << 'EOF'
-- BACKUP LIGERO MADRID AIR QUALITY
-- Solo datos recientes para operación diaria en cloud
-- El entrenamiento se hace local con datos completos

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;

CREATE DATABASE air_quality_db WITH TEMPLATE = template0 ENCODING = 'UTF8';
\c air_quality_db;

EOF

echo "📝 Exportando estructura de tablas..."

# Exportar estructura completa
pg_dump -h localhost -U $DB_USER -d $DB_NAME \
  --schema-only \
  --no-owner \
  --no-privileges \
  >> "$BACKUP_FILE"

echo "📊 Exportando dimensiones (completas)..."

# Dimensiones completas (son pequeñas)
pg_dump -h localhost -U $DB_USER -d $DB_NAME \
  --data-only \
  --no-owner \
  --no-privileges \
  --table=dim_estaciones \
  --table=dim_magnitudes \
  >> "$BACKUP_FILE"

echo "📈 Exportando mediciones recientes (2 años)..."

# Solo mediciones recientes para predicciones
cat >> "$BACKUP_FILE" << EOF

-- Mediciones recientes (últimos 2 años)
COPY fact_mediciones FROM stdin;
EOF

psql -h localhost -U $DB_USER -d $DB_NAME -c "
COPY (
    SELECT * FROM fact_mediciones
    WHERE fecha_hora::date >= '$FECHA_LIMITE'
    ORDER BY fecha_hora DESC, id_estacion, id_magnitud
) TO STDOUT;
" >> "$BACKUP_FILE"

echo "\\." >> "$BACKUP_FILE"

echo "🌦️ Exportando datos meteorológicos recientes..."

# Datos meteorológicos recientes
cat >> "$BACKUP_FILE" << EOF

-- Datos meteorológicos recientes (últimos 2 años)
COPY fact_meteo_diaria FROM stdin;
EOF

psql -h localhost -U $DB_USER -d $DB_NAME -c "
COPY (
    SELECT * FROM fact_meteo_diaria
    WHERE fecha >= '$FECHA_LIMITE'
    ORDER BY fecha DESC
) TO STDOUT;
" >> "$BACKUP_FILE"

echo "\\." >> "$BACKUP_FILE"

# Estadísticas del backup ligero
echo ""
echo "📊 ESTADÍSTICAS DEL BACKUP LIGERO:"

MEDICIONES_RECIENTES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT count(*) FROM fact_mediciones WHERE fecha_hora::date >= '$FECHA_LIMITE';
")

METEO_RECIENTES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT count(*) FROM fact_meteo_diaria WHERE fecha >= '$FECHA_LIMITE';
")

# Calcular reducción
TOTAL_MEDICIONES=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM fact_mediciones;")
REDUCCION=$(echo "scale=1; $(echo $MEDICIONES_RECIENTES | tr -d ' ') * 100 / $(echo $TOTAL_MEDICIONES | tr -d ' ')" | bc)

echo "   Mediciones (2 años): $(echo $MEDICIONES_RECIENTES | tr -d ' ') registros"
echo "   Meteo (2 años): $(echo $METEO_RECIENTES | tr -d ' ') registros"
echo "   Reducción: ${REDUCCION}% del total"

# Comprimir
gzip "$BACKUP_FILE"

echo ""
echo "✅ Backup ligero completado:"
echo "   📁 Archivo: ${BACKUP_FILE}.gz"
echo "   📏 Tamaño: $(du -h ${BACKUP_FILE}.gz | cut -f1)"

# Generar checksum
md5sum "${BACKUP_FILE}.gz" > "${BACKUP_FILE}.gz.md5"

echo ""
echo "🎯 ARQUITECTURA HÍBRIDA:"
echo "   ✅ Cloud: Datos ligeros para predicciones diarias"
echo "   ✅ Local: Datos completos para reentrenamiento"
echo "   ✅ Modelos: Sincronización via GitHub"
echo ""
echo "📋 PRÓXIMOS PASOS:"
echo "   1. Migrar a Neon: ./scripts/restore_to_cloud.sh ${BACKUP_FILE##*/}.gz"
echo "   2. Configurar sync de modelos: GitHub Releases/LFS"
echo "   3. Setup GitHub Actions para predicciones"