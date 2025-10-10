# UI - MADRID AIR QUALITY DASHBOARD
# Modern interface inspired by Montreal Curbcut and Kinelytics branding

ui <- dashboardPage(
  skin = "black",

  # Window title (browser tab)
  title = "Madrid Air Quality - Kinelytics",
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; font-weight: 600; font-size: 13px; white-space: nowrap; overflow: hidden; max-width: 320px;",
      tags$i(class = "fa fa-leaf", style = "margin-right: 6px; color: white; font-size: 20px;"),
      tags$span("Madrid Air Quality", style = "overflow: hidden; text-overflow: ellipsis;")
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Panel Principal", tabName = "mapa", icon = icon("tachometer-alt")),
      menuItem("Análisis Estaciones", tabName = "graficos", icon = icon("map-pin")),
      menuItem("Condiciones Meteorológicas", tabName = "meteo", icon = icon("cloud-sun")),
      menuItem("Datos y Estadísticas", tabName = "datos", icon = icon("database")),
      menuItem("Acerca del Sistema", tabName = "info", icon = icon("info-circle"))
    ),
    
    # Controls Section
    tags$div(class = "sidebar-controls", style = "margin-top: 20px;",
      tags$div(style = "padding: 20px; background: rgba(240, 245, 239, 0.1); border-radius: 8px; margin: 15px;",
        h4("Controles", style = "color: #b8d6c6; margin-bottom: 20px; font-weight: 600;"),
        
        # Pollutant selector - ICA 5 contaminants
        div(style = "margin-bottom: 20px;",
          tags$label("Contaminante:", style = "color: #f0f5ef; font-weight: 500; display: block; margin-bottom: 8px;"),
          selectInput("contaminante_sel",
                      label = NULL,
                      choices = list(
                        "Dióxido de Nitrógeno (NO₂)" = "Dióxido de Nitrógeno",
                        "Partículas < 10 µm (PM10)" = "Partículas < 10 µm",
                        "Partículas < 2.5 µm (PM2.5)" = "Partículas < 2.5 µm",
                        "Ozono (O₃)" = "Ozono",
                        "Dióxido de Azufre (SO₂)" = "Dióxido de Azufre"
                      ),
                      selected = "Dióxido de Nitrógeno")
        ),
        
        # Información sobre gráficos
        div(style = "margin-bottom: 15px;",
          tags$label("Visualización por Estación:", style = "color: #f0f5ef; font-weight: 500; display: block; margin-bottom: 8px;"),
          div(class = "metric-box", style = "background: linear-gradient(135deg, #27ae60, #2ecc71); color: white; padding: 12px; text-align: center;",
            tags$i(class = "fa fa-chart-bar", style = "font-size: 16px; margin-right: 6px;"),
            tags$span("Análisis Actual", style = "font-weight: 600;")
          ),
          tags$p("Actualiza automáticamente según el contaminante seleccionado", 
                 style = "color: #b8d6c6; font-size: 11px; text-align: center; margin-top: 8px;")
        )
      )
    ),
    
    # System info
    tags$div(style = "margin: 20px 15px; padding: 15px; background: rgba(184, 214, 198, 0.1); border-radius: 8px; border-left: 4px solid #b8d6c6;",
      h5("Estado del Sistema", style = "color: #b8d6c6; margin-bottom: 12px; font-weight: 600;"),
      div(style = "color: #f0f5ef; font-size: 13px; line-height: 1.6;",
        tags$div(style = "margin-bottom: 8px;", 
          tags$strong("Última actualización: "),
          textOutput("ultima_actualizacion", inline = TRUE)
        ),
        tags$div(style = "margin-bottom: 8px;", 
          tags$strong("Modelo: "), "CARET Random Forest R² > 92%"
        ),
        tags$div(
          tags$strong("Estaciones: "), "16 puntos de monitoreo"
        )
      )
    ),
    
    # Kinelytics logo justo debajo
    tags$div(style = "margin: 15px; text-align: center;",
      tags$img(src = "kinelytic-header-logo-white.png", 
               style = "max-width: 90%; height: auto; max-height: 120px; opacity: .9;")
    )
  ),
  
  # Body
  dashboardBody(
    # Modern CSS with Kinelytics branding
    tags$head(
      tags$style(HTML("
        /* Reducir altura del header */
        .main-header .navbar {
          min-height: 45px !important;
        }
        .main-header .logo {
          height: 45px !important;
          line-height: 45px !important;
        }
        .main-header .navbar-nav > li > a {
          padding-top: 12px !important;
          padding-bottom: 12px !important;
        }
        
        /* Corregir desplazamiento del contenido */
        .content-wrapper, .right-side {
          margin-left: 280px !important;
        }
        .main-sidebar {
          position: fixed !important;
        }
        body.sidebar-mini .content-wrapper {
          margin-left: 280px !important;
        }
        
        .metric-box {
          background: linear-gradient(135deg, #f8f9fa, #e9ecef);
          border-radius: 10px;
          padding: 20px;
          text-align: center;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-bottom: 15px;
        }
        .metric-label {
          font-size: 12px;
          color: #6c757d;
          font-weight: 500;
        }
        .metric-value {
          font-size: 24px;
          font-weight: bold;
          color: #2c3e50;
          margin: 5px 0;
        }
      ")),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
    ),
    
    tabItems(
      
      # TAB 1: MAIN DASHBOARD ----
      tabItem(tabName = "mapa",
        # Key metrics row
        fluidRow(class = "metrics-row",
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
              div(class = "metric-label", "Calidad del Aire"),
              div(class = "metric-value", textOutput("clasificacion_general")),
              div(class = "metric-label", "Estándar OMS")
            )
          )
        ),
        
        # Main content: Layout dos columnas - Mapa izquierda, gráfico horas clave derecha
        fluidRow(
          # Mapa de Madrid (izquierda)
          column(7,
            box(
              title = tags$div(
                style = "display: flex; align-items: center;",
                tags$i(class = "fa fa-map-marked-alt", style = "margin-right: 8px;"),
                "Mapa Animado Madrid - Predicciones 40h"
              ), 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              height = "600px",
              withSpinner(
                uiOutput("mapa_animado_madrid"),
                color = "#3c8dbc"
              )
            )
          ),
          
          # Gráfico de horas clave (derecha)
          column(5,
            box(
              title = tags$div(
                style = "display: flex; align-items: center;",
                tags$i(class = "fa fa-clock-o", style = "margin-right: 8px;"),
                "Horas Clave - Predicciones"
              ), 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              height = "600px",
              withSpinner(
                plotOutput("grafico_horas_clave", height = "530px"),
                color = "#f39c12"
              )
            )
          )
        ),
        
        # Espaciado adicional
        div(style = "margin-top: 30px;"),
        
        fluidRow(
          # Gráfico de niveles (abajo, minimizado por defecto)
          column(12,
            box(
              title = tags$div(
                style = "display: flex; align-items: center;",
                tags$i(class = "fa fa-chart-bar", style = "margin-right: 8px;"),
                "Niveles por Estación - Evolución Temporal"
              ), 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              height = "400px",
              withSpinner(
                uiOutput("animacion_temporal"),
                color = "#00a65a"
              )
            )
          )
        )
      ),
      
      # TAB 2: TEMPORAL EVOLUTION WITH INTERACTIVE STATIONS ----
      tabItem(tabName = "graficos",
        # Station selector and info
        fluidRow(
          column(8,
            div(class = "metric-box", style = "background: linear-gradient(135deg, #3498db, #2980b9); color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
              h4("Análisis Interactivo de Estaciones", style = "margin: 0; color: white;"),
              p("Haga clic en cualquier estación del mapa para ver su evolución temporal. Las estaciones están coloreadas según estándares OMS 2021.", style = "margin: 5px 0 0 0; opacity: 0.9;")
            )
          ),
          column(4,
            div(class = "metric-box",
              div(class = "metric-label", "Estación Seleccionada"),
              div(class = "metric-value", textOutput("estacion_seleccionada", inline = TRUE)),
              div(class = "metric-label", "Haga clic en el mapa para seleccionar")
            )
          )
        ),
        
        fluidRow(
          # Left: Interactive Leaflet map with EU standards
          column(6,
            box(
              title = tags$div(
                style = "display: flex; align-items: center;",
                tags$i(class = "fa fa-map-pin", style = "margin-right: 8px;"),
                "Estaciones de Monitoreo Madrid - Estándares OMS"
              ), 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              height = "600px",
              withSpinner(
                leafletOutput("mapa_estaciones_reactivo", height = "530px"),
                color = "#3c8dbc"
              ),
              # Legend
              div(style = "margin-top: 10px; font-size: 12px;",
                HTML("<strong>Estándares OMS 2021:</strong><br/>
                     <span style='color: #2ecc71;'>● Bueno</span> | 
                     <span style='color: #f1c40f;'>● Moderado</span> | 
                     <span style='color: #e67e22;'>● Malo</span> | 
                     <span style='color: #e74c3c;'>● Muy Malo</span>")
              )
            )
          ),
          
          # Right: Temporal evolution for selected station
          column(6,
            box(
              title = tags$div(
                style = "display: flex; align-items: center;",
                tags$i(class = "fa fa-chart-line", style = "margin-right: 8px;"),
                "Evolución Temporal - Estación Seleccionada"
              ), 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              height = "600px",
              withSpinner(
                plotlyOutput("grafico_temporal_estacion", height = "530px"),
                color = "#00a65a"
              )
            )
          )
        ),
        
        # Espaciado adicional para evitar bloquear la leyenda OMS
        div(style = "margin-top: 60px;"),
        
        # Comparison and statistics
        fluidRow(
          # Station comparison
          box(
            title = "Comparación de Estaciones a las 12:00 Mañana",
            status = "info",
            width = 6,
            DT::dataTableOutput("tabla_comparacion_estaciones")
          ),
          
          # Air quality classification summary
          box(
            title = "Resumen Calidad del Aire - Estándares OMS",
            status = "warning",
            width = 6,
            withSpinner(
              plotlyOutput("grafico_clasificacion_eu", height = "300px"),
              color = "#f39c12"
            )
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
                <li><strong>5 Contaminantes ICA:</strong> NO₂, PM10, PM2.5, O₃, SO₂</li>
                <li><strong>Modelos RANGER:</strong> Random Forest con R² > 0.75 (UTM projection)</li>
              </ul>

              <h4>Contaminantes ICA Monitoreados:</h4>
              <ul>
                <li><strong>Dióxido de Nitrógeno (NO₂):</strong> Principal contaminante del tráfico</li>
                <li><strong>Partículas < 10 µm (PM10):</strong> Partículas suspendidas en el aire</li>
                <li><strong>Partículas < 2.5 µm (PM2.5):</strong> Partículas finas respirables</li>
                <li><strong>Ozono (O₃):</strong> Contaminante fotoquímico</li>
                <li><strong>Dióxido de Azufre (SO₂):</strong> Emisiones industriales y combustión</li>
              </ul>
              
              <h4>Tecnología:</h4>
              <ul>
                <li>Desarrollado en R con Shiny</li>
                <li>Mapas interactivos con Leaflet</li>
                <li>Gráficos interactivos con Plotly</li>
              </ul>
            ")
          )
        )
      )
    )
  )
)