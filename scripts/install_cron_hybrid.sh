#!/bin/bash

# INSTALADOR DE CRON JOBS PARA ARQUITECTURA HÍBRIDA
# Configura tanto reentrenamiento mensual como sincronización diaria

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
CRON_FILE="/tmp/madrid_air_hybrid_cron"

echo "🚀 Configurando automatización híbrida"
echo "📅 Reentrenamiento: Mensual (día 1 a las 2:00 AM)"
echo "🔄 Sincronización: Diaria (a las 5:00 AM)"

# Verificar scripts
MONTHLY_SCRIPT="$PROJECT_DIR/scripts/monthly_retrain.sh"
DAILY_SCRIPT="$PROJECT_DIR/scripts/daily_sync.sh"

if [ ! -f "$MONTHLY_SCRIPT" ]; then
    echo "❌ Error: No se encuentra $MONTHLY_SCRIPT"
    exit 1
fi

# Hacer scripts ejecutables
chmod +x "$MONTHLY_SCRIPT"
[ -f "$DAILY_SCRIPT" ] && chmod +x "$DAILY_SCRIPT"

# Crear archivo de cron
cat > "$CRON_FILE" << EOF
# MADRID AIR QUALITY SYSTEM - AUTOMATIZACIÓN HÍBRIDA
# Configuración para Raspberry Pi con GitHub Actions

# REENTRENAMIENTO MENSUAL
# Día 1 de cada mes a las 2:00 AM - Entrenar con datos verificados del mes anterior
0 2 1 * * $MONTHLY_SCRIPT >/dev/null 2>&1

# SINCRONIZACIÓN DIARIA DE DATOS
# Todos los días a las 5:00 AM - Enviar nuevos datos a cloud para GitHub Actions
0 5 * * * $PROJECT_DIR/scripts/daily_sync_data.sh >/dev/null 2>&1

# LIMPIEZA SEMANAL
# Domingos a las 3:00 AM - Limpiar logs y archivos temporales
0 3 * * 0 find $PROJECT_DIR/logs -name "*.log" -mtime +30 -delete >/dev/null 2>&1

# BACKUP SEMANAL
# Domingos a las 4:00 AM - Backup de modelos y configuración
0 4 * * 0 $PROJECT_DIR/scripts/weekly_backup.sh >/dev/null 2>&1

# MONITOREO DE SALUD DEL SISTEMA
# Cada 6 horas - Verificar estado de servicios
0 */6 * * * $PROJECT_DIR/scripts/health_check.sh >/dev/null 2>&1

EOF

# Instalar cron jobs
crontab "$CRON_FILE"

if [ $? -eq 0 ]; then
    echo "✅ Cron jobs instalados correctamente"
    echo ""
    echo "📋 TAREAS PROGRAMADAS:"
    echo "   🤖 Reentrenamiento: 1° de mes, 2:00 AM"
    echo "   📊 Sync datos: Diario, 5:00 AM"
    echo "   🧹 Limpieza: Domingos, 3:00 AM"
    echo "   💾 Backup: Domingos, 4:00 AM"
    echo "   🩺 Health check: Cada 6 horas"
    echo ""
    echo "🔍 Para verificar:"
    echo "   crontab -l"
    echo ""
    echo "📊 Para ver logs:"
    echo "   tail -f $PROJECT_DIR/logs/monthly_retrain_*.log"
    echo "   tail -f $PROJECT_DIR/logs/daily_sync_*.log"
else
    echo "❌ Error instalando cron jobs"
    exit 1
fi

# Limpiar archivo temporal
rm "$CRON_FILE"

echo ""
echo "🎯 CONFIGURACIÓN COMPLETADA"
echo ""
echo "💡 PRÓXIMOS PASOS:"
echo "   1. Configurar GitHub Secrets para la base de datos cloud"
echo "   2. Habilitar GitHub Actions workflow daily-predictions"
echo "   3. Ejecutar primera migración de datos a cloud"
echo "   4. Probar sincronización manual: $MONTHLY_SCRIPT"