# DASHBOARD SHINY - CALIDAD DEL AIRE MADRID
# Versión simplificada inspirada en montreal.curbcut.ca
# Objetivo: Dashboard interactivo para visualización de predicciones de calidad del aire

# 1. LIBRERÍAS ----
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
library(ggrepel)
library(purrr)

# 1b. ESTÁNDARES EUROPEOS DE CALIDAD DEL AIRE ----
ESTANDARES_EU <- list(
  "Dióxido de Nitrógeno" = list(
    bueno = 40, moderado = 100, malo = 200, muy_malo = 400
  ),
  "Partículas < 10 µm" = list(
    bueno = 25, moderado = 50, malo = 90, muy_malo = 180
  ),
  "Ozono" = list(
    bueno = 100, moderado = 140, malo = 180, muy_malo = 240
  )
)

# Colores para clasificación
COLORES_CALIDAD <- list(
  bueno = "#2ecc71", moderado = "#f1c40f", 
  malo = "#e67e22", muy_malo = "#e74c3c", unknown = "#95a5a6"
)

# Función para clasificar calidad del aire
clasificar_calidad_aire <- function(contaminante, valor) {
  est <- ESTANDARES_EU[[contaminante]]
  if (is.null(est)) return("unknown")
  if (valor <= est$bueno) return("bueno")
  else if (valor <= est$moderado) return("moderado") 
  else if (valor <= est$malo) return("malo")
  else return("muy_malo")
}

# 2. FUNCIONES AUXILIARES ----

# Cargar datos de predicciones
cargar_datos <- function() {
  tryCatch({
    if(file.exists("output/predicciones_40h_latest.rds")) {
      return(readRDS("output/predicciones_40h_latest.rds"))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Cargar datos meteorológicos
cargar_meteo <- function() {
  tryCatch({
    if(file.exists("output/meteo_40h_latest.rds")) {
      return(readRDS("output/meteo_40h_latest.rds"))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Función para crear paleta de colores
crear_paleta <- function(contaminante, valores) {
  if(grepl("Nitrógeno", contaminante)) {
    colorNumeric(c("#00FF00", "#FFFF00", "#FF6600", "#FF0000"), domain = c(0, 50))
  } else if(grepl("Partículas", contaminante)) {
    colorNumeric(c("#0066CC", "#00CCFF", "#FFCC00", "#FF3300"), domain = c(0, 40))
  } else if(grepl("Ozono", contaminante)) {
    colorNumeric(c("#66FF66", "#CCFF00", "#FF9900", "#CC0000"), domain = c(0, 80))
  } else {
    colorNumeric("viridis", domain = range(valores, na.rm = TRUE))
  }
}

# 3. INTERFAZ DE USUARIO ----
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "Madrid Air Quality | Predicciones en Tiempo Real",
    titleWidth = 450
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("🌍 Mapa Interactivo", tabName = "mapa", icon = icon("map")),
      menuItem("📊 Evolución Temporal", tabName = "graficos", icon = icon("chart-line")),
      menuItem("🌡️ Condiciones Meteorológicas", tabName = "meteo", icon = icon("cloud-sun")),
      menuItem("📋 Datos y Estadísticas", tabName = "datos", icon = icon("table")),
      menuItem("ℹ️ Información", tabName = "info", icon = icon("info-circle"))
    ),
    
    # Controles
    hr(),
    h4("Controles", style = "padding-left: 15px; color: white;"),
    
    # Selector de contaminante
    div(style = "padding: 15px;",
      selectInput("contaminante_sel", 
                  "Contaminante:",
                  choices = c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Ozono"),
                  selected = "Dióxido de Nitrógeno")
    ),
    
    # Selector de hora
    div(style = "padding: 15px;",
      sliderInput("hora_sel",
                  "Hora de Predicción:",
                  min = 1, max = 40, value = 1,
                  step = 1,
                  animate = animationOptions(interval = 2000))
    ),
    
    # Información del sistema
    hr(),
    div(style = "padding: 15px; color: #aaa; font-size: 12px;",
      p("Última actualización:"),
      textOutput("ultima_actualizacion"),
      br(),
      p("Sistema basado en modelos CARET"),
      p("16 estaciones de monitoreo")
    )
  ),
  
  # Body
  dashboardBody(
    # CSS personalizado
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .leaflet-container {
          border-radius: 8px;
        }
        .metric-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 8px;
          text-align: center;
          margin: 10px 0;
        }
        .metric-value {
          font-size: 28px;
          font-weight: bold;
          margin: 10px 0;
        }
        .metric-label {
          font-size: 14px;
          opacity: 0.9;
        }
      "))
    ),
    
    tabItems(
      
      # TAB 1: MAPA INTERACTIVO ----
      tabItem(tabName = "mapa",
        fluidRow(
          # Métricas principales
          column(3,
            div(class = "metric-box",
              div(class = "metric-label", "Promedio Actual"),
              div(class = "metric-value", textOutput("promedio_actual")),
              div(class = "metric-label", "µg/m³")
            )
          ),
          column(3,
            div(class = "metric-box",
              div(class = "metric-label", "Estación Máxima"),
              div(class = "metric-value", textOutput("estacion_maxima")),
              div(class = "metric-label", textOutput("valor_maximo"))
            )
          ),
          column(3,
            div(class = "metric-box",
              div(class = "metric-label", "Estación Mínima"),
              div(class = "metric-value", textOutput("estacion_minima")),
              div(class = "metric-label", textOutput("valor_minimo"))
            )
          ),
          column(3,
            div(class = "metric-box",
              div(class = "metric-label", "Variabilidad"),
              div(class = "metric-value", textOutput("variabilidad")),
              div(class = "metric-label", "Desv. Estándar")
            )
          )
        ),
        
        # Mapa principal
        fluidRow(
          box(
            title = "Mapa de Predicciones - Madrid", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            withSpinner(
              leafletOutput("mapa_principal", height = "550px"),
              color = "#3c8dbc"
            )
          )
        ),
        
        # Información adicional
        fluidRow(
          box(
            title = "Información de la Predicción",
            status = "info",
            width = 12,
            verbatimTextOutput("info_prediccion")
          )
        )
      ),
      
      # TAB 2: GRÁFICOS TEMPORALES ----
      tabItem(tabName = "graficos",
        fluidRow(
          # Gráfico principal de evolución
          box(
            title = "Evolución Temporal - Próximas 40 Horas",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            withSpinner(
              plotlyOutput("grafico_temporal", height = "450px"),
              color = "#3c8dbc"
            )
          )
        ),
        
        fluidRow(
          # Estadísticas por hora
          box(
            title = "Estadísticas Horarias",
            status = "info",
            width = 6,
            DT::dataTableOutput("tabla_estadisticas")
          ),
          
          # Comparación de contaminantes
          box(
            title = "Comparación de Contaminantes",
            status = "success",
            width = 6,
            plotlyOutput("grafico_comparacion")
          )
        )
      ),
      
      # TAB 3: METEOROLOGÍA ----
      tabItem(tabName = "meteo",
        fluidRow(
          # Gráfico meteorológico
          box(
            title = "Condiciones Meteorológicas - Próximas 40 Horas",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            withSpinner(
              plotlyOutput("grafico_meteo", height = "450px"),
              color = "#f39c12"
            )
          )
        ),
        
        fluidRow(
          # Métricas meteorológicas actuales
          column(4,
            valueBoxOutput("temp_actual", width = NULL)
          ),
          column(4,
            valueBoxOutput("humedad_actual", width = NULL)
          ),
          column(4,
            valueBoxOutput("viento_actual", width = NULL)
          )
        ),
        
        fluidRow(
          box(
            title = "Tabla de Datos Meteorológicos",
            status = "warning",
            width = 12,
            DT::dataTableOutput("tabla_meteo")
          )
        )
      ),
      
      # TAB 4: DATOS ----
      tabItem(tabName = "datos",
        fluidRow(
          box(
            title = "Datos de Predicciones",
            status = "primary",
            width = 12,
            DT::dataTableOutput("tabla_predicciones")
          )
        ),
        
        fluidRow(
          box(
            title = "Descargar Datos",
            status = "success",
            width = 6,
            p("Descarga los datos de predicciones en diferentes formatos:"),
            br(),
            downloadButton("download_csv", "Descargar CSV", class = "btn-primary"),
            br(), br(),
            downloadButton("download_json", "Descargar JSON", class = "btn-info")
          ),
          
          box(
            title = "Resumen del Sistema",
            status = "info",
            width = 6,
            verbatimTextOutput("resumen_sistema")
          )
        )
      ),
      
      # TAB 5: INFORMACIÓN ----
      tabItem(tabName = "info",
        fluidRow(
          box(
            title = "Acerca del Sistema",
            status = "primary",
            width = 12,
            HTML("
              <h3>Sistema de Predicción de Calidad del Aire - Madrid</h3>
              <p>Este dashboard presenta predicciones de calidad del aire para Madrid utilizando modelos de Machine Learning avanzados.</p>
              
              <h4>Características Principales:</h4>
              <ul>
                <li><strong>Predicciones 40 horas:</strong> Pronósticos horarios para las próximas 40 horas</li>
                <li><strong>16 Estaciones:</strong> Red de monitoreo distribuida por Madrid</li>
                <li><strong>3 Contaminantes:</strong> NO₂, PM10, y O₃</li>
                <li><strong>Modelos CARET:</strong> Random Forest con R² > 0.96</li>
              </ul>
              
              <h4>Contaminantes Monitoreados:</h4>
              <ul>
                <li><strong>Dióxido de Nitrógeno (NO₂):</strong> Principal contaminante del tráfico</li>
                <li><strong>Partículas < 10 µm (PM10):</strong> Partículas suspendidas en el aire</li>
                <li><strong>Ozono (O₃):</strong> Contaminante fotoquímico</li>
              </ul>
              
              <h4>Tecnología:</h4>
              <ul>
                <li>Desarrollado en R con Shiny</li>
                <li>Mapas interactivos con Leaflet</li>
                <li>Gráficos interactivos con Plotly</li>
                <li>Inspirado en <a href='https://montreal.curbcut.ca/' target='_blank'>Montreal Curbcut</a></li>
              </ul>
            ")
          )
        )
      )
    )
  )
)

# 4. SERVIDOR ----
server <- function(input, output, session) {
  
  # Datos reactivos
  datos_predicciones <- reactive({
    cargar_datos()
  })
  
  datos_meteo <- reactive({
    cargar_meteo()
  })
  
  # Filtrar datos según selección
  datos_filtrados <- reactive({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    horas_disponibles <- sort(unique(datos$fecha_hora))
    hora_objetivo <- horas_disponibles[min(input$hora_sel, length(horas_disponibles))]
    
    datos %>%
      filter(
        fecha_hora == hora_objetivo,
        contaminante == input$contaminante_sel
      )
  })
  
  # OUTPUTS PRINCIPALES ----
  
  # Última actualización
  output$ultima_actualizacion <- renderText({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      "No disponible"
    } else {
      format(max(datos$timestamp_prediccion), "%d/%m %H:%M")
    }
  })
  
  # Métricas principales
  output$promedio_actual <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    round(mean(datos$prediccion, na.rm = TRUE), 1)
  })
  
  output$estacion_maxima <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    idx_max <- which.max(datos$prediccion)
    substr(datos$nombre_estacion[idx_max], 1, 15)
  })
  
  output$valor_maximo <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    paste(round(max(datos$prediccion, na.rm = TRUE), 1), "µg/m³")
  })
  
  output$estacion_minima <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    idx_min <- which.min(datos$prediccion)
    substr(datos$nombre_estacion[idx_min], 1, 15)
  })
  
  output$valor_minimo <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    paste(round(min(datos$prediccion, na.rm = TRUE), 1), "µg/m³")
  })
  
  output$variabilidad <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    round(sd(datos$prediccion, na.rm = TRUE), 1)
  })
  
  # Mapa principal
  output$mapa_principal <- renderLeaflet({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) {
      return(leaflet() %>% addTiles() %>% setView(-3.7, 40.42, zoom = 11))
    }
    
    # Extraer coordenadas
    datos_coords <- datos %>%
      mutate(
        coords = st_coordinates(.),
        lng = coords[,1],
        lat = coords[,2]
      ) %>%
      st_drop_geometry()
    
    # Crear paleta
    pal <- crear_paleta(input$contaminante_sel, datos_coords$prediccion)
    
    leaflet(datos_coords) %>%
      addTiles() %>%
      setView(lng = -3.7, lat = 40.42, zoom = 11) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 15,
        color = "white",
        weight = 2,
        opacity = 0.8,
        fillColor = ~pal(prediccion),
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>", nombre_estacion, "</b><br/>",
          "Predicción: ", round(prediccion, 1), " µg/m³<br/>",
          "Hora: ", format(fecha_hora, "%d/%m %H:%M"), "<br/>",
          "Temperatura: ", round(temp_media_c, 1), "°C"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~prediccion,
        title = "µg/m³",
        opacity = 0.8
      )
  })
  
  # Información de predicción
  output$info_prediccion <- renderText({
    datos <- datos_filtrados()
    if(is.null(datos) || nrow(datos) == 0) {
      return("No hay datos disponibles para la selección actual.")
    }
    
    horas_disponibles <- sort(unique(datos_predicciones()$fecha_hora))
    hora_actual <- horas_disponibles[min(input$hora_sel, length(horas_disponibles))]
    
    paste0(
      "Contaminante: ", input$contaminante_sel, "\n",
      "Fecha y Hora: ", format(hora_actual, "%d/%m/%Y %H:%M"), "\n",
      "Estaciones: ", nrow(datos), "\n",
      "Promedio: ", round(mean(datos$prediccion), 1), " µg/m³\n",
      "Rango: ", round(min(datos$prediccion), 1), " - ", round(max(datos$prediccion), 1), " µg/m³\n",
      "Modelo RMSE: ", round(mean(datos$rmse_modelo), 2)
    )
  })
  
  # Gráfico temporal
  output$grafico_temporal <- renderPlotly({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    datos_contaminante <- datos %>%
      filter(contaminante == input$contaminante_sel) %>%
      group_by(fecha_hora) %>%
      summarise(
        promedio = mean(prediccion, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(datos_contaminante, aes(x = fecha_hora, y = promedio)) +
      geom_line(color = "#3c8dbc", size = 1.2) +
      geom_point(color = "#3c8dbc", size = 2) +
      labs(
        title = paste("Evolución Temporal -", input$contaminante_sel),
        x = "Fecha y Hora",
        y = "Concentración (µg/m³)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Tabla de estadísticas
  output$tabla_estadisticas <- DT::renderDataTable({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    stats <- datos %>%
      filter(contaminante == input$contaminante_sel) %>%
      group_by(fecha_hora) %>%
      summarise(
        Promedio = round(mean(prediccion, na.rm = TRUE), 1),
        Mínimo = round(min(prediccion, na.rm = TRUE), 1),
        Máximo = round(max(prediccion, na.rm = TRUE), 1),
        `Desv. Est.` = round(sd(prediccion, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(
        `Fecha/Hora` = format(fecha_hora, "%d/%m %H:%M")
      ) %>%
      select(`Fecha/Hora`, everything(), -fecha_hora)
    
    DT::datatable(
      stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    )
  })
  
  # Gráfico comparación
  output$grafico_comparacion <- renderPlotly({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    datos_comp <- datos %>%
      group_by(fecha_hora, contaminante) %>%
      summarise(
        promedio = mean(prediccion, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(datos_comp, aes(x = fecha_hora, y = promedio, color = contaminante)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "Comparación de Contaminantes",
        x = "Fecha y Hora",
        y = "Concentración (µg/m³)",
        color = "Contaminante"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      scale_color_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Gráfico meteorológico
  output$grafico_meteo <- renderPlotly({
    datos <- datos_meteo()
    if(is.null(datos)) return(NULL)
    
    p <- datos %>%
      select(fecha_hora, temp_media_c, humedad_media_pct) %>%
      distinct() %>%
      gather(key = "variable", value = "valor", -fecha_hora) %>%
      mutate(
        variable = case_when(
          variable == "temp_media_c" ~ "Temperatura (°C)",
          variable == "humedad_media_pct" ~ "Humedad (%)",
          TRUE ~ variable
        )
      ) %>%
      ggplot(aes(x = fecha_hora, y = valor, color = variable)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Condiciones Meteorológicas",
        x = "Fecha y Hora",
        y = "Valor",
        color = "Variable"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_color_manual(values = c("#ff6b6b", "#4ecdc4"))
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Value boxes meteorológicos
  output$temp_actual <- renderValueBox({
    datos <- datos_meteo()
    if(is.null(datos)) {
      temp <- "--"
    } else {
      temp <- paste0(round(mean(datos$temp_media_c, na.rm = TRUE), 1), "°C")
    }
    
    valueBox(
      value = temp,
      subtitle = "Temperatura Promedio",
      icon = icon("thermometer-half"),
      color = "red"
    )
  })
  
  output$humedad_actual <- renderValueBox({
    datos <- datos_meteo()
    if(is.null(datos)) {
      hum <- "--"
    } else {
      hum <- paste0(round(mean(datos$humedad_media_pct, na.rm = TRUE), 1), "%")
    }
    
    valueBox(
      value = hum,
      subtitle = "Humedad Promedio",
      icon = icon("tint"),
      color = "blue"
    )
  })
  
  output$viento_actual <- renderValueBox({
    datos <- datos_meteo()
    if(is.null(datos)) {
      viento <- "--"
    } else {
      viento <- paste0(round(mean(datos$velocidad_viento_kmh, na.rm = TRUE), 1), " km/h")
    }
    
    valueBox(
      value = viento,
      subtitle = "Velocidad del Viento",
      icon = icon("wind"),
      color = "green"
    )
  })
  
  # Tabla meteorológica
  output$tabla_meteo <- DT::renderDataTable({
    datos <- datos_meteo()
    if(is.null(datos)) return(NULL)
    
    tabla_meteo <- datos %>%
      select(fecha_hora, temp_media_c, humedad_media_pct, velocidad_viento_kmh, dir_viento_grados) %>%
      distinct() %>%
      mutate(
        `Fecha/Hora` = format(fecha_hora, "%d/%m %H:%M"),
        `Temperatura (°C)` = round(temp_media_c, 1),
        `Humedad (%)` = round(humedad_media_pct, 1),
        `Viento (km/h)` = round(velocidad_viento_kmh, 1),
        `Dirección Viento (°)` = round(dir_viento_grados, 0)
      ) %>%
      select(`Fecha/Hora`, `Temperatura (°C)`, `Humedad (%)`, `Viento (km/h)`, `Dirección Viento (°)`)
    
    DT::datatable(
      tabla_meteo,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    )
  })
  
  # Tabla de predicciones
  output$tabla_predicciones <- DT::renderDataTable({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    tabla <- datos %>%
      st_drop_geometry() %>%
      mutate(
        `Fecha/Hora` = format(fecha_hora, "%d/%m %H:%M"),
        `Predicción (µg/m³)` = round(prediccion, 1),
        `RMSE Modelo` = round(rmse_modelo, 2)
      ) %>%
      select(`Fecha/Hora`, nombre_estacion, contaminante, `Predicción (µg/m³)`, `RMSE Modelo`)
    
    DT::datatable(
      tabla,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Descargas
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("predicciones_madrid_", Sys.Date(), ".csv")
    },
    content = function(file) {
      datos <- datos_predicciones()
      if(!is.null(datos)) {
        datos_csv <- datos %>%
          st_drop_geometry() %>%
          mutate(
            lat = st_coordinates(datos)[,2],
            lng = st_coordinates(datos)[,1]
          )
        write.csv(datos_csv, file, row.names = FALSE)
      }
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() {
      paste0("predicciones_madrid_", Sys.Date(), ".json")
    },
    content = function(file) {
      datos <- datos_predicciones()
      if(!is.null(datos)) {
        datos_json <- datos %>%
          st_drop_geometry() %>%
          mutate(
            lat = st_coordinates(datos)[,2],
            lng = st_coordinates(datos)[,1]
          )
        jsonlite::write_json(datos_json, file, pretty = TRUE)
      }
    }
  )
  
  # Resumen del sistema
  output$resumen_sistema <- renderText({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      return("Sistema: Sin datos disponibles")
    }
    
    n_estaciones <- length(unique(datos$nombre_estacion))
    n_contaminantes <- length(unique(datos$contaminante))
    n_predicciones <- nrow(datos)
    fecha_min <- min(datos$fecha_hora)
    fecha_max <- max(datos$fecha_hora)
    
    paste0(
      "=== RESUMEN DEL SISTEMA ===\n",
      "Estaciones: ", n_estaciones, "\n",
      "Contaminantes: ", n_contaminantes, "\n",
      "Total predicciones: ", n_predicciones, "\n",
      "Periodo: ", format(fecha_min, "%d/%m %H:%M"), " - ", format(fecha_max, "%d/%m %H:%M"), "\n",
      "Modelo: CARET Random Forest\n",
      "RMSE promedio: ", round(mean(datos$rmse_modelo, na.rm = TRUE), 2), "\n",
      "Actualización: ", format(Sys.time(), "%d/%m/%Y %H:%M")
    )
  })
}

# 5. FUNCIÓN PARA EJECUTAR LA APP ----
ejecutar_dashboard <- function(puerto = 3838) {
  cat("🚀 Iniciando Dashboard de Calidad del Aire Madrid\n")
  cat("📍 Accede en: http://localhost:", puerto, "\n")
  cat("📊 Datos: Predicciones 40h con modelos CARET\n")
  cat("🗺️ Inspirado en: montreal.curbcut.ca\n\n")
  
  shinyApp(ui = ui, server = server, options = list(port = puerto, host = "0.0.0.0"))
}

# NUEVA ESTRUCTURA MODULAR EN app/ ----
# El dashboard ha sido reestructurado siguiendo estándares de Mastering Shiny
# Para ejecutar la aplicación, usar:
# 
# setwd("app")
# source("app.R")
# ejecutar_dashboard()
#
# O directamente desde el directorio raíz:
ejecutar_dashboard_modular <- function(puerto = 3838) {
  # Cambiar al directorio de la app
  directorio_original <- getwd()
  on.exit(setwd(directorio_original))
  
  if(dir.exists("app")) {
    setwd("app")
    source("app.R")
    ejecutar_dashboard(puerto)
  } else {
    stop("Directorio 'app/' no encontrado. Ejecuta desde el directorio raíz del proyecto.")
  }
}

# EJECUTAR SI SE LLAMA DIRECTAMENTE
if(interactive()) {
  ejecutar_dashboard_modular()
}