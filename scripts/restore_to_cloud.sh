#!/bin/bash

# RESTAURAR BACKUP A BASE DE DATOS CLOUD
# Ejecutar después de crear BD en Neon/Supabase

# Configuración (actualizar con datos de tu BD cloud)
CLOUD_HOST="your-neon-host.neon.tech"  # Cambiar por tu host
CLOUD_PORT="5432"
CLOUD_DB="neondb"
CLOUD_USER="your-username"  # Cambiar por tu usuario
BACKUP_FILE=""

# Función para mostrar ayuda
show_help() {
    echo "USO: $0 <archivo_backup.sql.gz>"
    echo ""
    echo "EJEMPLO:"
    echo "  $0 madrid_air_quality_backup_20241214_120000.sql.gz"
    echo ""
    echo "CONFIGURACIÓN REQUERIDA:"
    echo "  1. Actualizar variables CLOUD_* en este script"
    echo "  2. Configurar variable PGPASSWORD:"
    echo "     export PGPASSWORD='tu-password-cloud'"
    echo ""
    echo "CREAR BD CLOUD:"
    echo "  Neon: https://neon.tech"
    echo "  Supabase: https://supabase.com"
}

# Verificar argumentos
if [ $# -eq 0 ]; then
    echo "❌ Error: Falta archivo de backup"
    show_help
    exit 1
fi

BACKUP_FILE="$1"

# Verificar que el archivo existe
if [ ! -f "$BACKUP_FILE" ]; then
    echo "❌ Error: No se encuentra $BACKUP_FILE"
    exit 1
fi

# Verificar que PGPASSWORD está configurado
if [ -z "$PGPASSWORD" ]; then
    echo "❌ Error: Variable PGPASSWORD no configurada"
    echo "Ejecutar: export PGPASSWORD='tu-password-cloud'"
    exit 1
fi

echo "🚀 Iniciando restauración a cloud..."
echo "📁 Archivo: $BACKUP_FILE"
echo "🌐 Destino: $CLOUD_HOST"
echo "📊 Base de datos: $CLOUD_DB"

# Verificar checksum si existe
if [ -f "${BACKUP_FILE}.md5" ]; then
    echo "🔐 Verificando integridad..."
    if md5sum -c "${BACKUP_FILE}.md5"; then
        echo "✅ Checksum válido"
    else
        echo "❌ Error: Checksum inválido"
        exit 1
    fi
fi

# Descomprimir si es necesario
if [[ "$BACKUP_FILE" == *.gz ]]; then
    echo "🗜️ Descomprimiendo..."
    gunzip -k "$BACKUP_FILE"
    BACKUP_FILE="${BACKUP_FILE%.gz}"
fi

# Test de conexión
echo "🔌 Probando conexión..."
pg_isready -h $CLOUD_HOST -p $CLOUD_PORT -U $CLOUD_USER -d $CLOUD_DB

if [ $? -ne 0 ]; then
    echo "❌ Error: No se puede conectar a la base de datos cloud"
    echo "Verificar:"
    echo "  - Host: $CLOUD_HOST"
    echo "  - Usuario: $CLOUD_USER"
    echo "  - Password: \$PGPASSWORD"
    exit 1
fi

echo "✅ Conexión exitosa"

# Restaurar backup
echo "📤 Restaurando datos..."
psql -h $CLOUD_HOST -p $CLOUD_PORT -U $CLOUD_USER -d $CLOUD_DB \
  -v ON_ERROR_STOP=1 \
  -f "$BACKUP_FILE"

if [ $? -eq 0 ]; then
    echo "✅ Restauración completada exitosamente"

    # Verificar datos
    echo "🔍 Verificando datos restaurados..."
    TABLES=$(psql -h $CLOUD_HOST -p $CLOUD_PORT -U $CLOUD_USER -d $CLOUD_DB \
             -t -c "SELECT count(*) FROM information_schema.tables WHERE table_schema='public';")

    echo "📊 Tablas restauradas: $(echo $TABLES | tr -d ' ')"

    # Mostrar resumen de datos principales
    echo ""
    echo "📋 RESUMEN DE DATOS:"
    psql -h $CLOUD_HOST -p $CLOUD_PORT -U $CLOUD_USER -d $CLOUD_DB \
         -c "SELECT
                'dim_estaciones' as tabla, count(*) as registros
             FROM dim_estaciones
             UNION ALL
             SELECT
                'dim_magnitudes' as tabla, count(*) as registros
             FROM dim_magnitudes
             UNION ALL
             SELECT
                'fact_mediciones' as tabla, count(*) as registros
             FROM fact_mediciones
             UNION ALL
             SELECT
                'fact_meteo_diaria' as tabla, count(*) as registros
             FROM fact_meteo_diaria;"

else
    echo "❌ Error en la restauración"
    exit 1
fi

echo ""
echo "🎯 MIGRACIÓN COMPLETADA"
echo ""
echo "🔄 SIGUIENTES PASOS:"
echo "   1. Actualizar .Renviron con nuevas credenciales"
echo "   2. Configurar GitHub Secrets"
echo "   3. Probar GitHub Actions"
echo ""
echo "📝 VARIABLES PARA .Renviron:"
echo "   DB_HOST=\"$CLOUD_HOST\""
echo "   DB_PORT=\"$CLOUD_PORT\""
echo "   DB_NAME=\"$CLOUD_DB\""
echo "   DB_USER=\"$CLOUD_USER\""
echo "   DB_PASSWORD=\"tu-password\""