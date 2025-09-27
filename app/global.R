# GLOBAL.R - CONFIGURACIÓN GLOBAL DEL DASHBOARD
# Dependencias y configuración compartida siguiendo estándares de Mastering Shiny

# LIBRERÍAS ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(dplyr)
library(lubridate)
library(htmltools)
library(shinycssloaders)
library(ggplot2)
library(tidyr)
library(viridis)
library(jsonlite)
library(yaml)
library(ggrepel)
library(purrr)
library(tidyterra)
library(mapSpain)
library(gganimate)
library(transformr)

# CARGAR FUNCIONES AUXILIARES ----
# source("R/data_utils.R")           # TODO: crear si es necesario
# source("R/visualization_utils.R")  # TODO: crear si es necesario  
# source("R/brand_utils.R")          # TODO: crear si es necesario
# source("R/modern_ui_utils.R")      # TODO: crear si es necesario

# CONFIGURACIÓN GLOBAL ----

# Configurar opciones de DT para español
options(DT.options = list(
  language = list(
    url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
  )
))

# Configurar zona horaria
Sys.setenv(TZ = "Europe/Madrid")

# ESTÁNDARES OMS 2021 Y PROTOCOLO MADRID PARA CALIDAD DEL AIRE ----
ESTANDARES_OMS_2021 <- list(
  "Dióxido de Nitrógeno" = list(
    media_anual_oms = 10,     # µg/m³ - Recomendación OMS 2021
    media_24h_oms = 25,       # µg/m³ - Recomendación OMS 2021  
    media_1h_oms = 200,       # µg/m³ - Recomendación OMS 2021
    bueno = 25,               # Verde (mejorado vs EU)
    moderado = 50,            # Amarillo
    malo = 100,               # Naranja  
    muy_malo = 200            # Rojo
  ),
  "Partículas < 10 µm" = list(
    media_anual_oms = 15,     # µg/m³ - Recomendación OMS 2021
    media_24h_oms = 45,       # µg/m³ - Recomendación OMS 2021
    bueno = 20,               # Verde
    moderado = 40,            # Amarillo
    malo = 75,                # Naranja  
    muy_malo = 150            # Rojo
  ),
  "Ozono" = list(
    media_8h_oms = 100,       # µg/m³ - Recomendación OMS 2021 (temporada alta)
    media_8h_temporada_pico = 60, # µg/m³ - Recomendación OMS 2021
    bueno = 80,               # Verde
    moderado = 120,           # Amarillo
    malo = 160,               # Naranja
    muy_malo = 200            # Rojo
  )
)

# PROTOCOLO MADRID PARA EPISODIOS DE ALTA CONTAMINACIÓN NO2 ----
PROTOCOLO_MADRID_NO2 <- list(
  PREAVISO = list(
    descripcion = "Dos estaciones de una zona > 180 µg/m³ por 2h consecutivas O tres estaciones de la red > 180 µg/m³ por 3h consecutivas",
    umbral = 180,
    horas_consecutivas_zona = 2,
    estaciones_zona = 2,
    horas_consecutivas_red = 3,
    estaciones_red = 3
  ),
  AVISO = list(
    descripcion = "Dos estaciones de una zona > 200 µg/m³ por 2h consecutivas O tres estaciones de la red > 200 µg/m³ por 3h consecutivas",
    umbral = 200,
    horas_consecutivas_zona = 2,
    estaciones_zona = 2,
    horas_consecutivas_red = 3,
    estaciones_red = 3
  ),
  ALERTA = list(
    descripcion = "Tres estaciones de una zona (dos si es zona 4) > 400 µg/m³ por 3h consecutivas",
    umbral = 400,
    horas_consecutivas = 3,
    estaciones_zona_normal = 3,
    estaciones_zona_4 = 2
  )
)

# Función para clasificar calidad del aire según OMS 2021
clasificar_calidad_aire <- function(contaminante, valor) {
  est <- ESTANDARES_OMS_2021[[contaminante]]
  if (is.null(est)) return("unknown")
  
  if (valor <= est$bueno) return("bueno")
  else if (valor <= est$moderado) return("moderado") 
  else if (valor <= est$malo) return("malo")
  else return("muy_malo")
}

# Función para evaluar protocolo Madrid NO2
evaluar_protocolo_madrid <- function(datos_estaciones_no2) {
  # Implementación simplificada - evalúa valores actuales vs umbrales
  valores_altos <- sum(datos_estaciones_no2 >= 180, na.rm = TRUE)
  valores_muy_altos <- sum(datos_estaciones_no2 >= 200, na.rm = TRUE) 
  valores_criticos <- sum(datos_estaciones_no2 >= 400, na.rm = TRUE)
  
  if (valores_criticos >= 2) {
    return(list(nivel = "ALERTA", 
                mensaje = "⚠️ ALERTA: Valores críticos detectados (≥400 µg/m³)",
                color = "#e74c3c"))
  } else if (valores_muy_altos >= 2) {
    return(list(nivel = "AVISO", 
                mensaje = "🟡 AVISO: Valores elevados detectados (≥200 µg/m³)", 
                color = "#f39c12"))
  } else if (valores_altos >= 2) {
    return(list(nivel = "PREAVISO", 
                mensaje = "🟠 PREAVISO: Valores moderadamente altos (≥180 µg/m³)",
                color = "#e67e22"))
  } else {
    return(list(nivel = "NORMAL", 
                mensaje = "✅ NORMAL: Niveles dentro de rangos normales",
                color = "#2ecc71"))
  }
}

# Colores para clasificación
COLORES_CALIDAD <- list(
  bueno = "#2ecc71",      # Verde
  moderado = "#f1c40f",   # Amarillo
  malo = "#e67e22",       # Naranja
  muy_malo = "#e74c3c",   # Rojo
  unknown = "#95a5a6"     # Gris
)

# Tiles Madrid
madrid <- esp_get_munic_siane(munic  = "^Madrid$")
madrid_b <- st_buffer(madrid, dist = 10)
madrid_b <- st_as_sfc(st_bbox(madrid))
madrid_mask <- esp_getTiles(madrid_b, type = "IGNBase.Todo", mask = TRUE, crop = TRUE, zoommin = 3)