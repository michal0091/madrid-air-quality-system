#!/bin/bash

# BACKUP DE BASE DE DATOS POSTGRES
# Ejecutar en Raspberry Pi antes de migrar

# Configuración
BACKUP_DIR="/home/pi/backups"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
BACKUP_FILE="$BACKUP_DIR/madrid_air_quality_backup_$DATE.sql"

# Crear directorio de backup
mkdir -p $BACKUP_DIR

echo "🚀 Iniciando backup de base de datos..."
echo "📅 Fecha: $(date)"
echo "📊 Base de datos: $DB_NAME"
echo "💾 Archivo: $BACKUP_FILE"

# Backup completo con estructura y datos
pg_dump -h localhost -U $DB_USER -d $DB_NAME \
  --verbose \
  --clean \
  --if-exists \
  --create \
  --encoding=UTF8 \
  > "$BACKUP_FILE"

if [ $? -eq 0 ]; then
    echo "✅ Backup completado exitosamente"
    echo "📁 Archivo: $BACKUP_FILE"
    echo "📏 Tamaño: $(du -h $BACKUP_FILE | cut -f1)"

    # Comprimir backup
    gzip "$BACKUP_FILE"
    echo "🗜️ Comprimido: ${BACKUP_FILE}.gz"
    echo "📏 Tamaño comprimido: $(du -h ${BACKUP_FILE}.gz | cut -f1)"

    # Generar checksum para verificar integridad
    md5sum "${BACKUP_FILE}.gz" > "${BACKUP_FILE}.gz.md5"
    echo "🔐 Checksum: ${BACKUP_FILE}.gz.md5"

else
    echo "❌ Error en el backup"
    exit 1
fi

echo ""
echo "📋 RESUMEN DEL BACKUP:"
echo "   Archivo: ${BACKUP_FILE}.gz"
echo "   Tamaño: $(du -h ${BACKUP_FILE}.gz | cut -f1)"
echo "   Checksum: $(cat ${BACKUP_FILE}.gz.md5)"
echo ""
echo "🔄 SIGUIENTE PASO:"
echo "   1. Descargar: scp pi@tu-pi:${BACKUP_FILE}.gz ."
echo "   2. Crear BD en Neon/Supabase"
echo "   3. Restaurar: scripts/restore_to_cloud.sh"