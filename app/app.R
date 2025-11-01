# APP.R - PUNTO DE ENTRADA PRINCIPAL
# Dashboard de Calidad del Aire Madrid siguiendo estándares de Mastering Shiny

# Cargar configuración global
source("global.R")

# Cargar UI y Server
source("ui.R")
source("server.R")

# Función para ejecutar la aplicación
ejecutar_dashboard <- function(puerto = 3838) {
  cat("🚀 Iniciando Dashboard de Calidad del Aire Madrid\n")
  cat("📍 Accede en: http://localhost:", puerto, "\n")
  cat("📊 Datos: Predicciones 40h con XGBoost Native API\n")
  
  shinyApp(ui = ui, server = server, options = list(port = puerto, host = "0.0.0.0"))
}

# Ejecutar si se llama directamente
if(interactive()) {
  ejecutar_dashboard()
}