#!/bin/bash

# Script para ejecutar modelo ranger ICA en background
# Uso: bash run_ranger_ica.sh

echo "Iniciando entrenamiento Ranger ICA..."
echo "Logs: logs/modelo_ranger_ica.log"
echo "Proceso en background, puedes cerrar el terminal"

# Ejecutar en background con nohup (ruta completa a Rscript)
nohup /usr/bin/Rscript R/02_modelo_ranger_ica.R > logs/ranger_ica_output.log 2>&1 &

# Guardar PID
PID=$!
echo $PID > logs/ranger_ica.pid
echo "PID del proceso: $PID"
echo ""
echo "Comandos útiles:"
echo "  - Ver progreso: tail -f logs/modelo_ranger_ica.log"
echo "  - Ver output: tail -f logs/ranger_ica_output.log"
echo "  - Verificar proceso: ps -p $PID"
echo "  - Matar proceso: kill $PID"
echo ""
echo "✅ Proceso iniciado correctamente"
