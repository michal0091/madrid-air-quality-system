#!/bin/bash

# INSTALADOR DE CRON JOB PARA MADRID AIR QUALITY SYSTEM
# Ejecutar como: bash scripts/install_cron.sh

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
SCRIPT_PATH="$PROJECT_DIR/scripts/daily_update.sh"
CRON_FILE="/tmp/madrid_air_cron"

# Verificar que el script existe
if [ ! -f "$SCRIPT_PATH" ]; then
    echo "❌ Error: No se encuentra $SCRIPT_PATH"
    exit 1
fi

# Hacer el script ejecutable
chmod +x "$SCRIPT_PATH"

# Crear archivo de cron temporal
cat > "$CRON_FILE" << EOF
# Madrid Air Quality System - Actualización diaria a la 1:00 AM
0 1 * * * $SCRIPT_PATH >/dev/null 2>&1

# También ejecutar al reiniciar el sistema (opcional)
@reboot sleep 60 && $SCRIPT_PATH >/dev/null 2>&1
EOF

# Instalar cron job
crontab "$CRON_FILE"

if [ $? -eq 0 ]; then
    echo "✅ Cron job instalado correctamente"
    echo "📅 Se ejecutará diariamente a la 1:00 AM"
    echo ""
    echo "Para verificar:"
    echo "  crontab -l"
    echo ""
    echo "Para ver logs:"
    echo "  tail -f $PROJECT_DIR/logs/daily_update_*.log"
else
    echo "❌ Error instalando cron job"
    exit 1
fi

# Limpiar archivo temporal
rm "$CRON_FILE"

echo "🎯 Instalación completada"