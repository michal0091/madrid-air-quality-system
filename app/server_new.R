# SERVER - DASHBOARD CALIDAD DEL AIRE MADRID (ACTUALIZADO)
# Lógica del servidor con mejoras solicitadas

server <- function(input, output, session) {
  
  # DATOS REACTIVOS ----
  
  # Cargar datos de predicciones
  datos_predicciones <- reactive({
    tryCatch({
      if(file.exists("../output/predicciones_40h_latest.rds")) {
        datos <- readRDS("../output/predicciones_40h_latest.rds")
        # Convertir geometry a coordenadas si es sf
        if("sf" %in% class(datos)) {
          coords <- st_coordinates(datos)
          datos <- st_drop_geometry(datos) %>%
            mutate(
              lon = coords[,1],
              lat = coords[,2]
            )
        }
        return(datos)
      }
      return(NULL)
    }, error = function(e) {
      cat("Error cargando predicciones:", e$message, "\n")
      return(NULL)
    })
  })
  
  # Cargar datos meteorológicos
  datos_meteo <- reactive({
    tryCatch({
      if(file.exists("../output/meteo_40h_latest.rds")) {
        return(readRDS("../output/meteo_40h_latest.rds"))
      }
      return(NULL)
    }, error = function(e) {
      cat("Error cargando meteorología:", e$message, "\n")
      return(NULL)
    })
  })
  
  # Estado de estación seleccionada (reactivo)
  estacion_seleccionada <- reactiveVal(4)  # Por defecto Pza. de España
  
  # FILTROS Y DATOS DERIVADOS ----
  
  # Datos para hora específica (para página principal)
  datos_hora_actual <- reactive({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    req(input$hora_sel, input$contaminante_sel)
    
    horas_disponibles <- sort(unique(datos$fecha_hora))
    if(length(horas_disponibles) == 0) return(NULL)
    
    hora_objetivo <- horas_disponibles[min(input$hora_sel, length(horas_disponibles))]
    
    datos %>%
      filter(
        fecha_hora == hora_objetivo,
        contaminante == input$contaminante_sel
      ) %>%
      mutate(
        # Clasificar según estándares UE
        clasificacion = map_chr(prediccion, ~clasificar_calidad_aire(input$contaminante_sel, .x)),
        color_clasificacion = map_chr(clasificacion, ~COLORES_CALIDAD[[.x]])
      )
  })
  
  # Datos para 12:00 del día siguiente (para clasificación EU)
  datos_mediodía_mañana <- reactive({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    req(input$contaminante_sel)
    
    # Buscar el primer 12:00 disponible
    datos_12h <- datos %>%
      mutate(hora_del_dia = hour(fecha_hora)) %>%
      filter(
        hora_del_dia == 12,
        contaminante == input$contaminante_sel
      ) %>%
      arrange(fecha_hora)
    
    if(nrow(datos_12h) == 0) return(NULL)
    
    # Tomar el primer 12:00 disponible
    primera_12h <- min(datos_12h$fecha_hora)
    
    datos_12h %>%
      filter(fecha_hora == primera_12h) %>%
      mutate(
        clasificacion = map_chr(prediccion, ~clasificar_calidad_aire(input$contaminante_sel, .x)),
        color_clasificacion = map_chr(clasificacion, ~COLORES_CALIDAD[[.x]])
      )
  })
  
  # PÁGINA PRINCIPAL - OUTPUTS ----
  
  # Métricas principales
  output$promedio_actual <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    round(mean(datos$prediccion, na.rm = TRUE), 1)
  })
  
  output$estacion_maxima <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    datos %>%
      filter(prediccion == max(prediccion, na.rm = TRUE)) %>%
      pull(nombre_estacion) %>%
      first()
  })
  
  output$valor_maximo <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    paste(round(max(datos$prediccion, na.rm = TRUE), 1), "µg/m³")
  })
  
  output$estacion_minima <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    datos %>%
      filter(prediccion == min(prediccion, na.rm = TRUE)) %>%
      pull(nombre_estacion) %>%
      first()
  })
  
  output$valor_minimo <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("--")
    paste(round(min(datos$prediccion, na.rm = TRUE), 1), "µg/m³")
  })
  
  output$clasificacion_general <- renderText({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) return("Unknown")
    
    # Clasificación general basada en el promedio
    promedio <- mean(datos$prediccion, na.rm = TRUE)
    clasificacion <- clasificar_calidad_aire(input$contaminante_sel, promedio)
    
    switch(clasificacion,
      "bueno" = "Good",
      "moderado" = "Moderate", 
      "malo" = "Poor",
      "muy_malo" = "Very Poor",
      "Unknown"
    )
  })
  
  # Gráfico de barras por estaciones (izquierda)
  output$grafico_barras_estaciones <- renderPlotly({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    # Ordenar por valor para mejor visualización
    datos_ordenados <- datos %>%
      arrange(prediccion) %>%
      mutate(nombre_estacion = factor(nombre_estacion, levels = nombre_estacion))
    
    p <- plot_ly(
      data = datos_ordenados,
      x = ~prediccion,
      y = ~nombre_estacion,
      type = "bar",
      orientation = "h",
      marker = list(color = ~color_clasificacion, line = list(color = 'black', width = 0.5)),
      text = ~paste0(nombre_estacion, "<br>", round(prediccion, 1), " µg/m³<br>", 
                     switch(clasificacion, 
                            "bueno" = "Good", "moderado" = "Moderate", 
                            "malo" = "Poor", "muy_malo" = "Very Poor")),
      textposition = "none",
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste("Current Levels -", input$contaminante_sel), font = list(size = 16)),
        xaxis = list(title = "Concentration (µg/m³)", zeroline = TRUE),
        yaxis = list(title = "", categoryorder = "array", categoryarray = datos_ordenados$nombre_estacion),
        margin = list(l = 120, r = 20, t = 50, b = 50),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    
    return(p)
  })
  
  # Evolución temporal principal (derecha)
  output$grafico_evolucion_principal <- renderPlotly({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    req(input$contaminante_sel)
    
    # Calcular promedio por hora
    datos_promedio <- datos %>%
      filter(contaminante == input$contaminante_sel) %>%
      group_by(fecha_hora) %>%
      summarise(
        prediccion_promedio = mean(prediccion, na.rm = TRUE),
        prediccion_min = min(prediccion, na.rm = TRUE),
        prediccion_max = max(prediccion, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(fecha_hora)
    
    p <- plot_ly(data = datos_promedio, x = ~fecha_hora, y = ~prediccion_promedio, type = "scatter", mode = "lines+markers",
                 line = list(color = "#00a65a", width = 3),
                 marker = list(color = "#00a65a", size = 6),
                 name = "Average",
                 text = ~paste0("Time: ", format(fecha_hora, "%d/%m %H:%M"), 
                               "<br>Average: ", round(prediccion_promedio, 1), " µg/m³"),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_ribbons(
        ymin = ~prediccion_min, ymax = ~prediccion_max,
        fillcolor = "rgba(0, 166, 90, 0.2)",
        line = list(color = "transparent"),
        name = "Range",
        hovertemplate = "Min: %{ymin:.1f}<br>Max: %{ymax:.1f}<extra></extra>"
      ) %>%
      layout(
        title = list(text = paste("40-Hour Prediction -", input$contaminante_sel), font = list(size = 16)),
        xaxis = list(title = "Time", tickangle = 45),
        yaxis = list(title = "Concentration (µg/m³)", zeroline = TRUE),
        margin = list(l = 60, r = 20, t = 50, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE
      )
    
    return(p)
  })
  
  # Mapa estático de Madrid
  output$mapa_estatico_madrid <- renderPlot({
    datos <- datos_hora_actual()
    if(is.null(datos) || nrow(datos) == 0) {
      return(ggplot() + 
             annotate("text", x = 0, y = 0, label = "No data available", size = 6) +
             theme_void())
    }
    
    # Crear mapa base de Madrid con coordenadas reales
    madrid_bounds <- data.frame(
      lon = c(-3.85, -3.55),
      lat = c(40.35, 40.55)
    )
    
    p <- ggplot(datos) +
      # Fondo de Madrid (rectangulo aproximado)
      geom_rect(aes(xmin = -3.85, xmax = -3.55, ymin = 40.35, ymax = 40.55), 
                fill = "#f8f9fa", color = "#dee2e6", size = 0.5, alpha = 0.7) +
      # Río Manzanares aproximado (línea diagonal)
      geom_curve(aes(x = -3.75, y = 40.38, xend = -3.68, yend = 40.48),
                 color = "#3498db", size = 2, alpha = 0.6, curvature = 0.2) +
      # Parque del Retiro
      geom_rect(aes(xmin = -3.69, xmax = -3.67, ymin = 40.41, ymax = 40.43),
                fill = "#2ecc71", alpha = 0.3) +
      # Casa de Campo
      geom_rect(aes(xmin = -3.76, xmax = -3.73, ymin = 40.40, ymax = 40.44),
                fill = "#27ae60", alpha = 0.3) +
      # Estaciones de monitoreo
      geom_point(aes(x = lon, y = lat, size = prediccion, color = color_clasificacion),
                 alpha = 0.8, stroke = 1.5) +
      # Etiquetas de estaciones principales
      geom_text_repel(aes(x = lon, y = lat, 
                         label = ifelse(prediccion >= quantile(prediccion, 0.8) | 
                                      prediccion <= quantile(prediccion, 0.2),
                                      paste0(substr(nombre_estacion, 1, 8), "\n", round(prediccion, 1)), "")),
                     size = 2.5, max.overlaps = 6) +
      scale_color_identity() +
      scale_size_continuous(range = c(4, 12), guide = guide_legend(title = "µg/m³")) +
      coord_fixed(ratio = 1.3, xlim = c(-3.85, -3.55), ylim = c(40.35, 40.55)) +
      labs(
        title = paste("Madrid Air Quality Map -", input$contaminante_sel),
        subtitle = paste("Prediction for", format(datos$fecha_hora[1], "%d/%m/%Y %H:%M")),
        caption = "Colors based on EU air quality standards"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        panel.grid = element_line(color = "#dee2e6", size = 0.3)
      )
    
    return(p)
  })
  
  # PÁGINA STATION ANALYSIS - OUTPUTS ----
  
  # Mapa leaflet reactivo con estaciones
  output$mapa_estaciones_reactivo <- renderLeaflet({
    datos <- datos_mediodía_mañana()
    if(is.null(datos) || nrow(datos) == 0) {
      return(leaflet() %>% setView(lng = -3.7, lat = 40.42, zoom = 11))
    }
    
    # Crear mapa base
    mapa <- leaflet(datos) %>%
      setView(lng = -3.7, lat = 40.42, zoom = 11) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB"))
    
    # Añadir marcadores con clasificación EU
    for(i in 1:nrow(datos)) {
      row <- datos[i, ]
      
      popup_text <- paste0(
        "<strong>", row$nombre_estacion, "</strong><br/>",
        "Concentration: ", round(row$prediccion, 1), " µg/m³<br/>",
        "Classification: ", switch(row$clasificacion,
                                   "bueno" = "Good", "moderado" = "Moderate", 
                                   "malo" = "Poor", "muy_malo" = "Very Poor"), "<br/>",
        "Prediction for: 12:00 tomorrow"
      )
      
      mapa <- mapa %>%
        addCircleMarkers(
          lng = row$lon, lat = row$lat,
          radius = 10 + (row$prediccion / max(datos$prediccion, na.rm = TRUE)) * 10,
          color = "white",
          fillColor = row$color_clasificacion,
          fillOpacity = 0.8,
          weight = 2,
          popup = popup_text,
          layerId = row$id_estacion
        )
    }
    
    return(mapa)
  })
  
  # Observer para clicks en el mapa
  observeEvent(input$mapa_estaciones_reactivo_marker_click, {
    click_info <- input$mapa_estaciones_reactivo_marker_click
    if(!is.null(click_info$id)) {
      estacion_seleccionada(click_info$id)
    }
  })
  
  # Nombre de estación seleccionada
  output$estacion_seleccionada <- renderText({
    datos <- datos_predicciones()
    if(is.null(datos)) return("None")
    
    estacion_id <- estacion_seleccionada()
    nombre <- datos %>%
      filter(id_estacion == estacion_id) %>%
      pull(nombre_estacion) %>%
      first()
    
    ifelse(is.na(nombre), "None", nombre)
  })
  
  # Gráfico temporal para estación seleccionada
  output$grafico_temporal_estacion <- renderPlotly({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    req(input$contaminante_sel)
    estacion_id <- estacion_seleccionada()
    
    datos_estacion <- datos %>%
      filter(
        id_estacion == estacion_id,
        contaminante == input$contaminante_sel
      ) %>%
      arrange(fecha_hora)
    
    if(nrow(datos_estacion) == 0) {
      return(plot_ly() %>% add_text(text = "No data for selected station", x = 0.5, y = 0.5))
    }
    
    nombre_estacion <- first(datos_estacion$nombre_estacion)
    
    p <- plot_ly(
      data = datos_estacion,
      x = ~fecha_hora,
      y = ~prediccion,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#3c8dbc", width = 3),
      marker = list(color = "#3c8dbc", size = 6),
      text = ~paste0("Time: ", format(fecha_hora, "%d/%m %H:%M"), 
                    "<br>", nombre_estacion,
                    "<br>", round(prediccion, 1), " µg/m³"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste(nombre_estacion, "-", input$contaminante_sel), font = list(size = 16)),
        xaxis = list(title = "Time", tickangle = 45),
        yaxis = list(title = "Concentration (µg/m³)", zeroline = TRUE),
        margin = list(l = 60, r = 20, t = 50, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    
    # Añadir líneas de referencia EU
    est <- ESTANDARES_EU[[input$contaminante_sel]]
    if(!is.null(est)) {
      p <- p %>%
        add_hline(y = est$bueno, line = list(color = "#2ecc71", dash = "dash"), 
                  annotation = list(text = "Good limit", x = 0.02)) %>%
        add_hline(y = est$moderado, line = list(color = "#f1c40f", dash = "dash"),
                  annotation = list(text = "Moderate limit", x = 0.02)) %>%
        add_hline(y = est$malo, line = list(color = "#e67e22", dash = "dash"),
                  annotation = list(text = "Poor limit", x = 0.02))
    }
    
    return(p)
  })
  
  # Tabla comparación de estaciones a las 12:00
  output$tabla_comparacion_estaciones <- DT::renderDataTable({
    datos <- datos_mediodía_mañana()
    if(is.null(datos) || nrow(datos) == 0) return(NULL)
    
    tabla <- datos %>%
      select(nombre_estacion, prediccion, clasificacion) %>%
      mutate(
        prediccion = round(prediccion, 1),
        clasificacion = case_when(
          clasificacion == "bueno" ~ "Good",
          clasificacion == "moderado" ~ "Moderate", 
          clasificacion == "malo" ~ "Poor",
          clasificacion == "muy_malo" ~ "Very Poor",
          TRUE ~ clasificacion
        )
      ) %>%
      arrange(desc(prediccion)) %>%
      rename(
        Station = nombre_estacion,
        `Concentration (µg/m³)` = prediccion,
        `EU Classification` = clasificacion
      )
    
    DT::datatable(
      tabla,
      options = list(
        pageLength = 16,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'EU Classification',
        backgroundColor = DT::styleEqual(
          c('Good', 'Moderate', 'Poor', 'Very Poor'),
          c('#d4edda', '#fff3cd', '#f8d7da', '#f5c6cb')
        )
      )
  })
  
  # Gráfico resumen de clasificación EU
  output$grafico_clasificacion_eu <- renderPlotly({
    datos <- datos_mediodía_mañana()
    if(is.null(datos) || nrow(datos) == 0) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    resumen <- datos %>%
      count(clasificacion) %>%
      mutate(
        clasificacion_en = case_when(
          clasificacion == "bueno" ~ "Good",
          clasificacion == "moderado" ~ "Moderate", 
          clasificacion == "malo" ~ "Poor",
          clasificacion == "muy_malo" ~ "Very Poor",
          TRUE ~ clasificacion
        ),
        color = case_when(
          clasificacion == "bueno" ~ "#2ecc71",
          clasificacion == "moderado" ~ "#f1c40f", 
          clasificacion == "malo" ~ "#e67e22",
          clasificacion == "muy_malo" ~ "#e74c3c",
          TRUE ~ "#95a5a6"
        )
      )
    
    p <- plot_ly(
      data = resumen,
      labels = ~clasificacion_en,
      values = ~n,
      type = "pie",
      marker = list(colors = ~color, line = list(color = 'white', width = 2)),
      textinfo = 'label+percent+value',
      textposition = 'outside',
      hovertemplate = "%{label}<br>Stations: %{value}<br>Percentage: %{percent}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Air Quality Distribution at 12:00 Tomorrow", font = list(size = 14)),
        showlegend = FALSE,
        margin = list(t = 50, b = 20, l = 20, r = 20)
      )
    
    return(p)
  })
}