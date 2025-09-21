#!/bin/bash

# SCRIPT DE DESPLIEGUE PARA RASPBERRY PI
# Configura la aplicación Shiny en el servidor

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
SHINY_DIR="/srv/shiny-server"
APP_NAME="madrid-air-quality"
APP_DIR="$SHINY_DIR/$APP_NAME"

echo "🚀 INICIANDO DESPLIEGUE EN RASPBERRY PI"

# Función para logging
log_step() {
    echo "📋 $1"
}

# Paso 1: Crear directorio de aplicación en Shiny Server
log_step "Creando directorio de aplicación..."
sudo mkdir -p "$APP_DIR"
sudo chown -R shiny:shiny "$APP_DIR"

# Paso 2: Copiar archivos de la aplicación
log_step "Copiando archivos de la aplicación..."
sudo cp -r "$PROJECT_DIR/app/"* "$APP_DIR/"

# Paso 3: Asegurar permisos correctos
log_step "Configurando permisos..."
sudo chown -R shiny:shiny "$APP_DIR"
sudo chmod -R 755 "$APP_DIR"

# Paso 4: Crear enlace simbólico a datos (opcional)
log_step "Configurando acceso a datos..."
if [ ! -L "$APP_DIR/data" ]; then
    sudo ln -sf "$PROJECT_DIR/data" "$APP_DIR/data"
fi
if [ ! -L "$APP_DIR/output" ]; then
    sudo ln -sf "$PROJECT_DIR/output" "$APP_DIR/output"
fi
if [ ! -L "$APP_DIR/models" ]; then
    sudo ln -sf "$PROJECT_DIR/models" "$APP_DIR/models"
fi

# Paso 5: Copiar archivo .Renviron para credenciales de BD
log_step "Configurando variables de entorno..."
if [ -f "$PROJECT_DIR/.Renviron" ]; then
    sudo cp "$PROJECT_DIR/.Renviron" "$APP_DIR/.Renviron"
    sudo chown shiny:shiny "$APP_DIR/.Renviron"
    sudo chmod 600 "$APP_DIR/.Renviron"
else
    echo "⚠️ Advertencia: No se encuentra .Renviron - configurar credenciales de BD manualmente"
fi

# Paso 6: Instalar dependencias R faltantes
log_step "Verificando dependencias R..."
sudo su - -c "R -e \"
packages <- c('shinydashboard', 'leaflet', 'plotly', 'DT', 'sf', 'dplyr', 
              'lubridate', 'htmltools', 'shinycssloaders', 'ggplot2', 'tidyr', 
              'viridis', 'jsonlite', 'yaml', 'ggrepel', 'purrr', 'tidyterra', 
              'mapSpain', 'DBI', 'RPostgres')
new.packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/')
\""

# Paso 7: Configurar Shiny Server
log_step "Configurando Shiny Server..."
sudo tee /etc/shiny-server/shiny-server.conf > /dev/null << EOF
# Configuración Shiny Server para Madrid Air Quality
server {
  listen 3838;

  # Aplicación Madrid Air Quality
  location /madrid-air-quality {
    site_dir $APP_DIR;
    log_dir /var/log/shiny-server;
    directory_index on;
  }

  # Página principal redirige a la app
  location / {
    site_dir $APP_DIR;
    log_dir /var/log/shiny-server;
    directory_index off;
  }
}
EOF

# Paso 8: Reiniciar servicios
log_step "Reiniciando Shiny Server..."
sudo systemctl restart shiny-server
sudo systemctl enable shiny-server

# Paso 9: Verificar estado
if sudo systemctl is-active --quiet shiny-server; then
    echo "✅ Shiny Server está ejecutándose"
else
    echo "❌ Error: Shiny Server no está ejecutándose"
    echo "Verificar logs: sudo journalctl -u shiny-server -f"
fi

# Información final
echo ""
echo "🎯 DESPLIEGUE COMPLETADO"
echo ""
echo "📍 Aplicación disponible en:"
echo "   http://$(hostname -I | awk '{print $1}'):3838"
echo "   http://$(hostname -I | awk '{print $1}'):3838/madrid-air-quality"
echo ""
echo "📊 Logs de Shiny Server:"
echo "   sudo tail -f /var/log/shiny-server/*.log"
echo ""
echo "🔧 Para reinstalar después de cambios:"
echo "   bash $PROJECT_DIR/scripts/deploy_to_pi.sh"