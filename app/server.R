# SERVER - DASHBOARD CALIDAD DEL AIRE MADRID (ACTUALIZADO)
# Lógica del servidor con mejoras solicitadas

server <- function(input, output, session) {
  
  # DATOS REACTIVOS ----
  
  # Cargar datos de predicciones
  datos_predicciones <- reactive({
    tryCatch({
      # Buscar archivo en múltiples ubicaciones posibles
      archivo_predicciones <- NULL
      rutas_posibles <- c(
        "data/predicciones_40h_latest.rds",
        "../output/predicciones_40h_latest.rds",
        "output/predicciones_40h_latest.rds"
      )

      for(ruta in rutas_posibles) {
        if(file.exists(ruta)) {
          archivo_predicciones <- ruta
          break
        }
      }

      if(!is.null(archivo_predicciones)) {
        datos <- readRDS(archivo_predicciones)
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
      # Buscar archivo meteorológico en múltiples ubicaciones
      rutas_meteo <- c(
        "data/meteo_40h_latest.rds",
        "../output/meteo_40h_latest.rds",
        "output/meteo_40h_latest.rds"
      )

      for(ruta in rutas_meteo) {
        if(file.exists(ruta)) {
          return(readRDS(ruta))
        }
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
  
  # Datos para hora actual (primera hora disponible)
  datos_hora_actual <- reactive({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    req(input$contaminante_sel)
    
    horas_disponibles <- sort(unique(datos$fecha_hora))
    if(length(horas_disponibles) == 0) return(NULL)
    
    # Usar la primera hora disponible (más actual)
    hora_objetivo <- horas_disponibles[1]
    
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
    
    if(nrow(datos_12h) == 0) {
      cat("DATOS DEBUG: No hay datos para las 12:00 del contaminante", input$contaminante_sel, "\n")
      return(NULL)
    }
    
    # Tomar el primer 12:00 disponible
    primera_12h <- min(datos_12h$fecha_hora)
    cat("DATOS DEBUG: Procesando hora", format(primera_12h, "%Y-%m-%d %H:%M"), "\n")
    
    resultado <- datos_12h %>%
      filter(fecha_hora == primera_12h) %>%
      mutate(
        clasificacion = map_chr(prediccion, ~clasificar_calidad_aire(input$contaminante_sel, .x)),
        color_clasificacion = map_chr(clasificacion, ~COLORES_CALIDAD[[.x]])
      )
    
    cat("DATOS DEBUG: Resultado final -", nrow(resultado), "estaciones\n")
    cat("DATOS DEBUG: Coordenadas - lat:", range(resultado$lat, na.rm = TRUE), "lon:", range(resultado$lon, na.rm = TRUE), "\n")
    
    return(resultado)
  })
  
  # OUTPUTS GENERALES ----
  
  # Última actualización basada en timestamp de archivos
  output$ultima_actualizacion <- renderText({
    tryCatch({
      archivos <- c(
        "data/predicciones_40h_latest.rds", "data/meteo_40h_latest.rds",
        "../output/predicciones_40h_latest.rds", "../output/meteo_40h_latest.rds"
      )
      archivos_existentes <- archivos[file.exists(archivos)]
      
      if(length(archivos_existentes) > 0) {
        timestamps <- sapply(archivos_existentes, function(x) file.info(x)$mtime)
        ultimo_timestamp <- max(timestamps, na.rm = TRUE)
        format(ultimo_timestamp, "%d/%m %H:%M")
      } else {
        format(Sys.time(), "%d/%m %H:%M")
      }
    }, error = function(e) {
      format(Sys.time(), "%d/%m %H:%M")
    })
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
      "bueno" = "Bueno",
      "moderado" = "Moderado", 
      "malo" = "Malo",
      "muy_malo" = "Muy Malo",
      "Desconocido"
    )
  })
  
  # Sistema de animación por horas usando JavaScript
  output$animacion_temporal <- renderUI({
    # Obtener datos para extraer horas reales
    datos <- datos_predicciones()
    if(is.null(datos)) return(tags$p("Cargando datos..."))

    # Obtener horas disponibles (primeras 10 horas cada 4h)
    horas_disponibles <- sort(unique(datos$fecha_hora))
    indices_estrategicos <- seq(1, min(40, length(horas_disponibles)), by = 4)[1:10]
    horas_estrategicas <- horas_disponibles[indices_estrategicos[!is.na(indices_estrategicos)]]

    # Formatear horas para JavaScript
    horas_formateadas <- sapply(seq_along(horas_estrategicas), function(i) {
      hora <- horas_estrategicas[i]
      dia <- if(as.Date(hora) == as.Date(horas_estrategicas[1])) 1 else 2
      sprintf("%s (Día %d)", format(hora, "%H:%M"), dia)
    })

    # Generar JavaScript con horas dinámicas
    horas_js <- paste0("'", horas_formateadas, "'", collapse = ", ")

    # Mapear nombres de contaminantes a archivos
    cont_archivo <- switch(input$contaminante_sel,
      "Dióxido de Nitrógeno" = "no2",
      "Partículas < 10 µm" = "pm10",
      "Ozono" = "o3",
      "no2"  # default
    )

    # Generar lista de URLs de imágenes para JavaScript - usar gráficos de estaciones
    urls_imagenes <- paste0("horas/", cont_archivo, "_hora_", sprintf("%02d", 1:10), ".png")
    urls_js <- paste0("'", urls_imagenes, "'", collapse = ", ")
    
    tagList(
      # Div contenedor para la imagen
      tags$div(
        id = "contenedor-animacion",
        style = "text-align: center; height: 430px; background: white;",
        tags$img(
          id = "imagen-animada",
          src = urls_imagenes[1],  # Primera imagen
          style = "max-width: 100%; height: 430px; object-fit: contain;",
          alt = paste("Niveles actuales", input$contaminante_sel, "por estación")
        ),
        # Controles de animación
        tags$div(
          style = "margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
          tags$button(
            id = "btn-play", class = "btn btn-success btn-sm",
            onclick = "window.MadridAirAnimation.toggle()", 
            tags$i(class = "fa fa-play"), " Play"
          ),
          tags$button(
            id = "btn-reset", class = "btn btn-info btn-sm", style = "margin-left: 5px;",
            onclick = "window.MadridAirAnimation.reset()", 
            tags$i(class = "fa fa-refresh"), " Reset"
          ),
          tags$span(
            id = "hora-actual",
            style = "margin-left: 15px; font-weight: bold; color: #2c3e50;",
            paste("Hora:", horas_formateadas[1])
          )
        )
      ),
      
      # JavaScript para animación reactiva (aislado en namespace)
      tags$script(HTML(paste0("
        (function() {
          // Variables privadas para animación
          let imagenes = [", urls_js, "];
          let indiceActual = 0;
          let animacionActiva = false;
          let intervalId = null;
        
        // Función para actualizar lista de imágenes (llamada desde Shiny)
        function actualizarImagenes(nuevasImagenes) {
          // Pausar animación actual sin reiniciarla automáticamente
          pausarAnimacion();
          imagenes = nuevasImagenes;
          indiceActual = 0;
          cambiarImagen();
          // NO reiniciar automáticamente - usuario debe presionar Play
        }
        
        // Función para cambiar imagen
        function cambiarImagen() {
          const img = document.getElementById('imagen-animada');
          const contador = document.getElementById('hora-actual');
          
          if (img && imagenes[indiceActual]) {
            img.src = imagenes[indiceActual];
            const horasEstrategicas = [", horas_js, "];
            contador.textContent = 'Hora: ' + horasEstrategicas[indiceActual];
            
            indiceActual = (indiceActual + 1) % imagenes.length;
          }
        }
        
        // Función para iniciar/parar animación
        function toggleAnimacion() {
          if (animacionActiva) {
            pausarAnimacion();
          } else {
            iniciarAnimacion();
          }
        }
        
        // Iniciar animación
        function iniciarAnimacion() {
          if (!animacionActiva) {
            animacionActiva = true;
            intervalId = setInterval(cambiarImagen, 1500); // Cambiar cada 1.5 segundos
            document.getElementById('btn-play').innerHTML = '<i class=\"fa fa-stop\"></i> Stop';
          }
        }
        
        // Pausar animación
        function pausarAnimacion() {
          if (animacionActiva) {
            animacionActiva = false;
            clearInterval(intervalId);
            document.getElementById('btn-play').innerHTML = '<i class=\"fa fa-play\"></i> Play';
          }
        }
        
        // Reset animación al inicio
        function resetAnimacion() {
          pausarAnimacion();
          indiceActual = 0;
          cambiarImagen();
        }
        
        // NO auto-iniciar animación - solo manual con el botón Play
        
        // Handler para mensajes personalizados de Shiny (con verificación)
        if (typeof Shiny !== 'undefined') {
          Shiny.addCustomMessageHandler('actualizarAnimacion', function(data) {
            actualizarImagenes(data.imagenes);
          });
        }
        
        // Exponer funciones globalmente para botones (evitar conflictos con dashboard)
        window.MadridAirAnimation = {
          toggle: toggleAnimacion,
          reset: resetAnimacion
        };
        
        })(); // Cerrar IIFE
      ")))
    )
  })
  
  # Observador para actualizar animación cuando cambia el contaminante
  observeEvent(input$contaminante_sel, {
    # Evitar ejecutar en la primera carga
    if(is.null(input$contaminante_sel)) return()
    # Mapear nombres de contaminantes a archivos
    cont_archivo <- switch(input$contaminante_sel,
      "Dióxido de Nitrógeno" = "no2",
      "Partículas < 10 µm" = "pm10", 
      "Ozono" = "o3",
      "no2"  # default
    )
    
    # Generar lista de URLs de imágenes
    urls_imagenes <- paste0("horas/", cont_archivo, "_hora_", sprintf("%02d", 1:10), ".png")
    urls_js <- paste0("'", urls_imagenes, "'", collapse = ", ")
    
    # Ejecutar JavaScript para actualizar las imágenes de gráficos
    session$sendCustomMessage(type = 'actualizarAnimacion', 
                              message = list(imagenes = urls_imagenes))
    
    # También actualizar mapas animados
    urls_mapas <- paste0("mapas_horas/mapa_", cont_archivo, "_hora_", sprintf("%02d", 1:10), ".png")
    session$sendCustomMessage(type = 'actualizarAnimacionMapa', 
                              message = list(mapas = urls_mapas))
  })
  
  # Mapa animado de Madrid (nueva funcionalidad)
  output$mapa_animado_madrid <- renderUI({
    # Obtener datos para extraer horas reales (igual que animacion_temporal)
    datos <- datos_predicciones()
    if(is.null(datos)) return(tags$p("Cargando datos..."))

    # Obtener horas disponibles (primeras 10 horas cada 4h)
    horas_disponibles <- sort(unique(datos$fecha_hora))
    indices_estrategicos <- seq(1, min(40, length(horas_disponibles)), by = 4)[1:10]
    horas_estrategicas <- horas_disponibles[indices_estrategicos[!is.na(indices_estrategicos)]]

    # Formatear horas para JavaScript (igual formato)
    horas_formateadas <- sapply(seq_along(horas_estrategicas), function(i) {
      hora <- horas_estrategicas[i]
      dia <- if(as.Date(hora) == as.Date(horas_estrategicas[1])) 1 else 2
      sprintf("%s (Día %d)", format(hora, "%H:%M"), dia)
    })

    # Generar JavaScript con horas dinámicas
    horas_js_mapa <- paste0("'", horas_formateadas, "'", collapse = ", ")

    # Mapear nombres de contaminantes a archivos
    cont_archivo <- switch(input$contaminante_sel,
      "Dióxido de Nitrógeno" = "no2",
      "Partículas < 10 µm" = "pm10",
      "Ozono" = "o3",
      "no2"  # default
    )

    # Generar lista de URLs de mapas para JavaScript
    urls_mapas <- paste0("mapas_horas/mapa_", cont_archivo, "_hora_", sprintf("%02d", 1:10), ".png")
    urls_js <- paste0("'", urls_mapas, "'", collapse = ", ")
    
    tagList(
      # Div contenedor para el mapa
      tags$div(
        id = "contenedor-mapa-animado",
        style = "text-align: center; height: 530px; background: white;",
        tags$img(
          id = "mapa-animado",
          src = urls_mapas[1],  # Primer mapa
          style = "max-width: 100%; height: 530px; object-fit: contain;",
          alt = paste("Mapa animado Madrid -", input$contaminante_sel)
        ),
        # Controles de animación para mapa
        tags$div(
          style = "margin-top: 10px; padding: 10px; background: #e3f2fd; border-radius: 5px;",
          tags$button(
            id = "btn-play-mapa", class = "btn btn-primary btn-sm",
            onclick = "window.MadridMapAnimation.toggle()", 
            tags$i(class = "fa fa-play"), " Play Mapa"
          ),
          tags$button(
            id = "btn-reset-mapa", class = "btn btn-info btn-sm", style = "margin-left: 5px;",
            onclick = "window.MadridMapAnimation.reset()", 
            tags$i(class = "fa fa-refresh"), " Reset"
          ),
          tags$span(
            id = "hora-actual-mapa",
            style = "margin-left: 15px; font-weight: bold; color: #1565c0;",
            paste("Hora:", horas_formateadas[1])
          )
        )
      ),
      
      # JavaScript para animación de mapa (unificado)
      tags$script(HTML(paste0("
        // Solo inicializar MadridMapAnimation si no existe
        if (!window.MadridMapAnimation) {
        (function() {
          // Variables privadas para animación de mapa
          let mapas = [", urls_js, "];
          let indiceActualMapa = 0;
          let animacionActivaMapa = false;
          let intervalIdMapa = null;
        
        // Función para cambiar mapa
        function cambiarMapa() {
          const img = document.getElementById('mapa-animado');
          const contador = document.getElementById('hora-actual-mapa');

          // Horas estratégicas dinámicas de los datos reales
          const horasEstrategicas = [", horas_js_mapa, "];

          if (img && mapas[indiceActualMapa]) {
            img.src = mapas[indiceActualMapa];
            contador.textContent = 'Hora: ' + horasEstrategicas[indiceActualMapa];

            indiceActualMapa = (indiceActualMapa + 1) % mapas.length;
          }
        }
        
        // Función para establecer mapas (pública)
        function setMapas(nuevosMapas) {
          mapas = nuevosMapas;
          indiceActualMapa = 0;
          if (mapas.length > 0) {
            cambiarMapa();
          }
        }
        
        // Función para actualizar mapas (llamada desde Shiny)
        function actualizarMapas(nuevosMapas) {
          const estabaActiva = animacionActivaMapa;
          pausarAnimacionMapa();
          setMapas(nuevosMapas);
          if (estabaActiva) {
            setTimeout(iniciarAnimacionMapa, 500);
          }
        }
        
        // Función para iniciar/parar animación de mapa
        function toggleAnimacionMapa() {
          if (animacionActivaMapa) {
            pausarAnimacionMapa();
          } else {
            iniciarAnimacionMapa();
          }
        }
        
        // Iniciar animación de mapa
        function iniciarAnimacionMapa() {
          if (!animacionActivaMapa) {
            animacionActivaMapa = true;
            intervalIdMapa = setInterval(cambiarMapa, 2000); // 2 segundos por mapa
            document.getElementById('btn-play-mapa').innerHTML = '<i class=\"fa fa-stop\"></i> Stop';
          }
        }
        
        // Pausar animación de mapa
        function pausarAnimacionMapa() {
          if (animacionActivaMapa) {
            animacionActivaMapa = false;
            clearInterval(intervalIdMapa);
            document.getElementById('btn-play-mapa').innerHTML = '<i class=\"fa fa-play\"></i> Play Mapa';
          }
        }
        
        // Reset animación de mapa
        function resetAnimacionMapa() {
          pausarAnimacionMapa();
          indiceActualMapa = 0;
          cambiarMapa();
        }
        
        // Auto-iniciar animación de mapa después de 2 segundos (evitar conflictos con box collapse)
        setTimeout(function() {
          if (mapas.length > 1 && document.getElementById('mapa-animado')) {
            iniciarAnimacionMapa();
          }
        }, 2000);
        
        // Handler para mensajes personalizados de mapa (con verificación)
        if (typeof Shiny !== 'undefined') {
          Shiny.addCustomMessageHandler('actualizarAnimacionMapa', function(data) {
            actualizarMapas(data.mapas);
          });
        }
        
        // Exponer funciones de mapa globalmente (evitar conflictos con dashboard)
        window.MadridMapAnimation = {
          toggle: toggleAnimacionMapa,
          reset: resetAnimacionMapa,
          setMapas: setMapas
        };
        
        })(); // Cerrar IIFE mapa
        } // Cerrar if (!window.MadridMapAnimation)
      ")))
    ) # Cerrar tagList
  })
  
  # Gráfico de horas clave (08:00, 14:00, 20:00 del día siguiente)
  output$grafico_horas_clave <- renderPlot({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      return(ggplot() + 
             annotate("text", x = 0, y = 0, label = "Cargando datos...", size = 6, color = "gray50") +
             theme_void())
    }
    
    # Extraer coordenadas si es sf
    if("sf" %in% class(datos)) {
      coords <- st_coordinates(datos)
      datos$lon <- coords[,1]
      datos$lat <- coords[,2]
      datos <- st_drop_geometry(datos)
    }
    
    # Definir horas clave del día siguiente
    horas_clave <- c("08:00", "14:00", "20:00")
    
    # Filtrar datos para horas clave
    datos_horas_clave <- datos %>%
      mutate(
        hora_str = format(fecha_hora, "%H:%M"),
        fecha_str = format(fecha_hora, "%Y-%m-%d")
      ) %>%
      filter(hora_str %in% horas_clave) %>%
      group_by(contaminante, hora_str) %>%
      summarise(
        promedio_prediccion = mean(prediccion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # Añadir clasificación promedio
        clasificacion = pmap_chr(list(contaminante, promedio_prediccion), clasificar_calidad_aire),
        # Crear etiqueta más descriptiva
        hora_label = case_when(
          hora_str == "08:00" ~ "Mañana\n(08:00)",
          hora_str == "14:00" ~ "Mediodía\n(14:00)", 
          hora_str == "20:00" ~ "Noche\n(20:00)",
          TRUE ~ hora_str
        ),
        # Abreviar nombres de contaminantes
        cont_corto = case_when(
          contaminante == "Dióxido de Nitrógeno" ~ "NO₂",
          contaminante == "Partículas < 10 µm" ~ "PM10",
          contaminante == "Ozono" ~ "O₃",
          TRUE ~ contaminante
        )
      )
    
    if(nrow(datos_horas_clave) == 0) {
      return(ggplot() + 
             annotate("text", x = 0, y = 0, label = "Sin datos para horas clave", size = 5, color = "gray50") +
             theme_void())
    }
    
    # Crear gráfico de barras agrupadas
    p <- ggplot(datos_horas_clave, aes(x = hora_label, y = promedio_prediccion, fill = clasificacion)) +
      geom_col(alpha = 0.85, color = "white", linewidth = 0.5) +
      
      # Facetas por contaminante
      facet_wrap(~ cont_corto, scales = "free_y", ncol = 1,
                strip.position = "right") +
      
      # Colores consistentes con el resto
      scale_fill_manual(
        values = COLORES_CALIDAD,
        name = "Calidad",
        labels = c("bueno" = "Bueno", "moderado" = "Moderado", 
                  "malo" = "Malo", "muy_malo" = "Muy Malo", "unknown" = "Desconocido")
      ) +
      
      # Etiquetas de valores dentro de las barras
      geom_text(aes(label = paste0(round(promedio_prediccion, 1), " µg/m³")), 
                position = position_dodge(width = 0.9), 
                vjust = 1.2, size = 5, fontface = "bold", color = "black") +
      
      labs(
        title = "Predicciones Horas Clave",
        subtitle = "Promedio de todas las estaciones",
        x = NULL,
        y = "Concentración (µg/m³)",
        caption = "Colores según estándares OMS 2021"
      ) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        plot.caption = element_text(size = 9, color = "gray60"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray95", color = "gray80"),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    return(p)
  })
  
  # Funciones de control de animación eliminadas (ya no necesarias con GIFs pregenerados)
  
  # Funciones plotly eliminadas - reemplazadas por animación gganimate
  
  # Mapa estático de Madrid usando imágenes pregeneradas
  output$mapa_estatico_madrid <- renderImage({
    # Mapear nombres de contaminantes a archivos de mapa
    cont_archivo <- switch(input$contaminante_sel,
      "Dióxido de Nitrógeno" = "no2",
      "Partículas < 10 µm" = "pm10", 
      "Ozono" = "o3",
      "no2"  # default
    )
    
    # Ruta del mapa pregenerado
    ruta_mapa <- file.path("www", "mapas", paste0("mapa_", cont_archivo, ".png"))
    
    # Verificar si existe el archivo
    if(!file.exists(ruta_mapa)) {
      # Crear imagen placeholder si no existe
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 800, height = 600)
      par(bg = "white", mar = c(2,2,3,2))
      plot(1:10, 1:10, type = "n", xlab = "", ylab = "", 
           main = paste("Mapa", input$contaminante_sel), 
           axes = FALSE)
      text(5.5, 6, paste("Mapa no disponible para", input$contaminante_sel), 
           cex = 1.2, col = "navy")
      text(5.5, 4.5, "Ejecute: Rscript generar_mapa_madrid.R", cex = 1, col = "gray50")
      rect(2, 3.5, 9, 4.5, col = "lightblue", border = "navy")
      dev.off()
      
      return(list(src = outfile, contentType = "image/png", 
                  width = "100%", height = 430, 
                  delete = TRUE))
    }
    
    # Retornar el mapa pregenerado
    return(list(src = ruta_mapa, contentType = "image/png", 
                width = "100%", height = 430,
                alt = paste("Mapa de Madrid -", input$contaminante_sel)))
  })
  
  # PÁGINA STATION ANALYSIS - OUTPUTS ----
  
  # Mapa leaflet reactivo con estaciones
  output$mapa_estaciones_reactivo <- renderLeaflet({
    datos <- datos_mediodía_mañana()
    if(is.null(datos) || nrow(datos) == 0) {
      cat("LEAFLET DEBUG: No hay datos disponibles\n")
      return(leaflet() %>% 
        setView(lng = -3.7, lat = 40.42, zoom = 11) %>%
        addTiles())
    }
    
    cat("LEAFLET DEBUG: Datos disponibles -", nrow(datos), "filas\n")
    cat("LEAFLET DEBUG: Columnas:", paste(names(datos), collapse = ", "), "\n")
    
    # Asegurar que tenemos las columnas necesarias
    if(!all(c("lon", "lat") %in% names(datos))) {
      cat("LEAFLET DEBUG: Faltan coordenadas lon/lat\n")
      return(leaflet() %>% 
        setView(lng = -3.7, lat = 40.42, zoom = 11) %>%
        addTiles())
    }
    
    # Añadir clasificación si no existe
    if(!"clasificacion" %in% names(datos)) {
      datos$clasificacion <- clasificar_calidad_aire(input$contaminante_sel, datos$prediccion)
    }
    
    # Añadir colores si no existen
    if(!"color_clasificacion" %in% names(datos)) {
      datos$color_clasificacion <- sapply(datos$clasificacion, function(x) COLORES_CALIDAD[[x]])
    }
    
    cat("LEAFLET DEBUG: Rango lat:", range(datos$lat, na.rm = TRUE), "\n")
    cat("LEAFLET DEBUG: Rango lon:", range(datos$lon, na.rm = TRUE), "\n")
    
    # Crear mapa base con CartoDB Voyager
    mapa <- leaflet() %>%
      setView(lng = -3.7, lat = 40.42, zoom = 11) %>%
      addProviderTiles("CartoDB.Voyager", group = "CartoDB Voyager") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
      addTiles(group = "OpenStreetMap") %>%
      addLayersControl(baseGroups = c("CartoDB Voyager", "CartoDB Light", "OpenStreetMap"))
    
    # Debug: verificar datos antes de agregar marcadores
    cat("LEAFLET DEBUG: Agregando", nrow(datos), "marcadores\n")
    cat("LEAFLET DEBUG: Ejemplo clasificacion:", head(datos$clasificacion, 3), "\n")
    cat("LEAFLET DEBUG: Ejemplo colores:", head(datos$color_clasificacion, 3), "\n")
    
    # Añadir marcadores con método simplificado usando addAwesomeMarkers
    colors_pal <- colorFactor(
      palette = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#95a5a6"),
      domain = c("bueno", "moderado", "malo", "muy_malo", "unknown")
    )
    
    mapa <- mapa %>%
      addCircleMarkers(
        data = datos,
        lng = ~lon, lat = ~lat,
        radius = 15,
        color = "white",
        fillColor = ~colors_pal(clasificacion),
        fillOpacity = 0.8,
        weight = 2,
        opacity = 1,
        popup = ~paste0(
          "<strong>", nombre_estacion, "</strong><br/>",
          "Concentración: ", round(prediccion, 1), " µg/m³<br/>",
          "Clasificación: ", clasificacion, "<br/>",
          "Predicción para: 12:00"
        ),
        layerId = ~id_estacion
      )
    
    return(mapa)
  })
  
  # Observer para clicks en el mapa
  observeEvent(input$mapa_estaciones_reactivo_marker_click, {
    click_info <- input$mapa_estaciones_reactivo_marker_click
    cat("CLICK DEBUG: Click detectado en mapa\n")
    cat("CLICK DEBUG: Info del click:", str(click_info), "\n")
    if(!is.null(click_info$id)) {
      cat("CLICK DEBUG: Estación clickeada ID:", click_info$id, "\n")
      estacion_seleccionada(click_info$id)
      cat("CLICK DEBUG: estacion_seleccionada actualizada a:", click_info$id, "\n")
    }
  })
  
  # Nombre de estación seleccionada
  output$estacion_seleccionada <- renderText({
    datos <- datos_predicciones()
    if(is.null(datos)) return("None")
    
    estacion_id <- estacion_seleccionada()
    cat("TEXT DEBUG: Renderizando estación seleccionada:", estacion_id, "\n")
    
    nombre <- datos %>%
      filter(id_estacion == estacion_id) %>%
      pull(nombre_estacion) %>%
      first()
    
    resultado <- ifelse(is.na(nombre), paste("ID", estacion_id), nombre)
    cat("TEXT DEBUG: Nombre resultado:", resultado, "\n")
    return(resultado)
  })
  
  # Gráfico temporal para estación seleccionada
  output$grafico_temporal_estacion <- renderPlotly({
    datos <- datos_predicciones()
    if(is.null(datos)) {
      return(plot_ly() %>% 
             add_annotations(text = "Cargando datos...", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    req(input$contaminante_sel)
    estacion_id <- estacion_seleccionada()
    
    cat("PLOTLY DEBUG: === RENDERPLOTLY EJECUTÁNDOSE ===\n")
    cat("PLOTLY DEBUG: Estación seleccionada:", estacion_id, "Contaminante:", input$contaminante_sel, "\n")
    cat("PLOTLY DEBUG: Timestamp:", Sys.time(), "\n")
    
    datos_estacion <- datos %>%
      filter(
        id_estacion == estacion_id,
        contaminante == input$contaminante_sel
      ) %>%
      arrange(fecha_hora)
    
    cat("PLOTLY DEBUG: Datos encontrados:", nrow(datos_estacion), "filas\n")
    
    if(nrow(datos_estacion) == 0) {
      return(plot_ly() %>% 
             add_annotations(text = "Sin datos para la estación seleccionada", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    nombre_estacion <- first(datos_estacion$nombre_estacion)
    cat("PLOTLY DEBUG: Creando gráfico para:", nombre_estacion, "\n")
    
    # Limpiar y simplificar el gráfico para evitar problemas de hover
    p <- plot_ly(
      data = datos_estacion,
      x = ~fecha_hora,
      y = ~prediccion,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#3c8dbc", width = 3),
      marker = list(color = "#3c8dbc", size = 8),
      name = nombre_estacion,
      customdata = ~paste0(format(fecha_hora, "%d/%m %H:%M")),
      hovertemplate = "%{customdata}<br>%{y:.1f} µg/m³<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste(nombre_estacion, "-", input$contaminante_sel), font = list(size = 14)),
        xaxis = list(title = "Hora", tickangle = 45),
        yaxis = list(title = "Concentración (µg/m³)", zeroline = TRUE),
        margin = list(l = 60, r = 20, t = 50, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE,
        hovermode = "x unified",  # Mejor alineación del hover
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(size = 12, color = "black")
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        responsive = TRUE,  # Mejora la responsividad
        modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
      )
    
    # Añadir sombreado por días (método seguro)
    datos_estacion$fecha <- as.Date(datos_estacion$fecha_hora)
    dias_unicos <- unique(datos_estacion$fecha)
    cat("PLOTLY DEBUG: Gráfico básico creado, añadiendo", length(dias_unicos), "días\n")
    
    # Solo añadir sombreado si hay múltiples días
    if(length(dias_unicos) > 1) {
      shapes_list <- list()
      
      for(i in seq_along(dias_unicos)) {
        if(i %% 2 == 0) {  # Solo días pares para alternancia
          dia_inicio <- as.POSIXct(paste(dias_unicos[i], "00:00:00"), tz = "Europe/Madrid")
          dia_fin <- as.POSIXct(paste(dias_unicos[i], "23:59:59"), tz = "Europe/Madrid")
          
          # Añadir shape a la lista
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "rect",
            x0 = dia_inicio,
            x1 = dia_fin,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            fillcolor = "rgba(200, 200, 200, 0.2)",
            line = list(color = "transparent"),
            layer = "below"
          )
        }
      }
      
      # Aplicar todas las shapes de una vez
      if(length(shapes_list) > 0) {
        current_layout <- p$x$layout
        current_layout$shapes <- shapes_list
        p$x$layout <- current_layout
      }
    }
    
    # Añadir líneas de referencia OMS (usando add_segments en lugar de add_hline)
    est <- ESTANDARES_OMS_2021[[input$contaminante_sel]]
    if(!is.null(est)) {
      x_min <- min(datos_estacion$fecha_hora, na.rm = TRUE)
      x_max <- max(datos_estacion$fecha_hora, na.rm = TRUE)
      
      p <- p %>%
        add_segments(
          x = x_min, xend = x_max, 
          y = est$bueno, yend = est$bueno,
          line = list(color = "#2ecc71", dash = "dash", width = 1),
          name = "Bueno", showlegend = FALSE, hoverinfo = "none"
        ) %>%
        add_segments(
          x = x_min, xend = x_max,
          y = est$moderado, yend = est$moderado,
          line = list(color = "#f1c40f", dash = "dash", width = 1),
          name = "Moderado", showlegend = FALSE, hoverinfo = "none"
        ) %>%
        add_segments(
          x = x_min, xend = x_max,
          y = est$malo, yend = est$malo,
          line = list(color = "#e67e22", dash = "dash", width = 1),
          name = "Malo", showlegend = FALSE, hoverinfo = "none"
        )
    }
    
    return(p)
  })
  
  # Tabla comparación de estaciones a las 12:00
  output$tabla_comparacion_estaciones <- DT::renderDataTable({
    datos <- datos_mediodía_mañana()
    if(is.null(datos) || nrow(datos) == 0) return(NULL)
    
    tabla <- datos %>%
      select(id_estacion, nombre_estacion, prediccion, clasificacion) %>%
      mutate(
        prediccion = round(prediccion, 1),
        clasificacion = case_when(
          clasificacion == "bueno" ~ "Bueno",
          clasificacion == "moderado" ~ "Moderado", 
          clasificacion == "malo" ~ "Malo",
          clasificacion == "muy_malo" ~ "Muy Malo",
          TRUE ~ clasificacion
        )
      ) %>%
      arrange(id_estacion) %>%
      rename(
        ID = id_estacion,
        Estación = nombre_estacion,
        `Concentración (µg/m³)` = prediccion,
        `Clasificación OMS` = clasificacion
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
        'Clasificación OMS',
        backgroundColor = DT::styleEqual(
          c('Bueno', 'Moderado', 'Malo', 'Muy Malo'),
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
        clasificacion_es = case_when(
          clasificacion == "bueno" ~ "Bueno",
          clasificacion == "moderado" ~ "Moderado", 
          clasificacion == "malo" ~ "Malo",
          clasificacion == "muy_malo" ~ "Muy Malo",
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
      labels = ~clasificacion_es,
      values = ~n,
      type = "pie",
      marker = list(colors = ~color, line = list(color = 'white', width = 2)),
      textinfo = 'label+percent+value',
      textposition = 'outside',
      hovertemplate = "%{label}<br>Estaciones: %{value}<br>Porcentaje: %{percent}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Distribución Calidad del Aire a las 12:00", font = list(size = 14)),
        showlegend = FALSE,
        margin = list(t = 50, b = 20, l = 20, r = 20)
      )
    
    return(p)
  })
  
  # OUTPUTS FALTANTES PARA TABS METEOROLOGÍA Y DATOS ----
  
  # Gráfico meteorológico
  output$grafico_meteo <- renderPlotly({
    meteo <- datos_meteo()
    if(is.null(meteo) || nrow(meteo) == 0) {
      return(plot_ly() %>% add_text(text = "No hay datos meteorológicos disponibles", x = 0.5, y = 0.5))
    }
    
    # Crear sombreado por días - solo para el rango de datos
    fechas_unicas <- sort(as.Date(unique(meteo$fecha_hora)))
    shapes_dias <- list()
    
    # Obtener rango real de los datos
    tiempo_min <- min(meteo$fecha_hora)
    tiempo_max <- max(meteo$fecha_hora)
    
    for(i in seq_along(fechas_unicas)) {
      fecha <- fechas_unicas[i]
      inicio_dia <- as.POSIXct(paste(fecha, "00:00:00"), tz = "Europe/Madrid")
      fin_dia <- as.POSIXct(paste(fecha, "23:59:59"), tz = "Europe/Madrid")
      
      # Ajustar a los límites reales de los datos
      inicio_efectivo <- max(inicio_dia, tiempo_min)
      fin_efectivo <- min(fin_dia, tiempo_max)
      
      # Solo crear sombreado si hay datos en este día
      if(inicio_efectivo <= fin_efectivo) {
        # Alternar colores de sombreado
        color_fondo <- if(i %% 2 == 1) "rgba(220, 220, 220, 0.2)" else "rgba(240, 240, 240, 0.3)"
        
        shapes_dias[[length(shapes_dias) + 1]] <- list(
          type = "rect",
          x0 = inicio_efectivo,
          x1 = fin_efectivo,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          fillcolor = color_fondo,
          line = list(width = 0),
          layer = "below"
        )
      }
    }
    
    p <- plot_ly(meteo, x = ~fecha_hora, y = ~temp_media_c, type = "scatter", mode = "lines+markers",
                 line = list(color = "#f39c12", width = 2),
                 marker = list(color = "#f39c12", size = 4),
                 name = "Temperatura",
                 text = ~paste0("Hora: ", format(fecha_hora, "%d/%m %H:%M"), 
                               "<br>Temperatura: ", round(temp_media_c, 1), "°C"),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_trace(y = ~humedad_media_pct, name = "Humedad", 
                line = list(color = "#3498db", width = 2),
                marker = list(color = "#3498db", size = 4),
                text = ~paste0("Hora: ", format(fecha_hora, "%d/%m %H:%M"), 
                               "<br>Humedad: ", round(humedad_media_pct, 1), "%"),
                hovertemplate = "%{text}<extra></extra>", yaxis = "y2") %>%
      layout(
        title = "Condiciones Meteorológicas - Próximas 40 Horas",
        xaxis = list(
          title = "Hora",
          range = c(tiempo_min, tiempo_max),
          type = "date",
          tickformat = "%d/%m %H:%M",
          dtick = 8 * 60 * 60 * 1000,  # Cada 8 horas en milisegundos
          tickangle = 0,
          tickfont = list(size = 11),
          showgrid = TRUE,
          gridcolor = "rgba(128, 128, 128, 0.2)",
          zeroline = FALSE,
          automargin = TRUE
        ),
        yaxis = list(
          title = "Temperatura (°C)", 
          side = "left", 
          color = "#f39c12",
          showgrid = TRUE,
          gridcolor = "rgba(248, 156, 18, 0.1)",
          zeroline = FALSE
        ),
        yaxis2 = list(
          title = "Humedad (%)", 
          side = "right", 
          overlaying = "y", 
          color = "#3498db",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        shapes = shapes_dias,
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = 'center',
          y = -0.1,
          yanchor = 'top'
        ),
        margin = list(b = 80, l = 60, r = 60)
      )

    # Aplicar sombreado de días (igual que en plotly de estaciones)
    if(length(shapes_dias) > 0) {
      current_layout <- p$x$layout
      current_layout$shapes <- shapes_dias
      p$x$layout <- current_layout
    }

    return(p)
  })
  
  # Value boxes meteorológicos
  output$temp_actual <- renderValueBox({
    meteo <- datos_meteo()
    if(is.null(meteo) || nrow(meteo) == 0) {
      temp_val <- "N/A"
    } else {
      temp_val <- paste0(round(meteo$temp_media_c[1], 1), "°C")
    }
    
    valueBox(
      value = temp_val,
      subtitle = "Temperatura Actual",
      icon = icon("thermometer-half"),
      color = "orange"
    )
  })
  
  output$humedad_actual <- renderValueBox({
    meteo <- datos_meteo()
    if(is.null(meteo) || nrow(meteo) == 0) {
      hum_val <- "N/A"
    } else {
      hum_val <- paste0(round(meteo$humedad_media_pct[1], 1), "%")
    }
    
    valueBox(
      value = hum_val,
      subtitle = "Humedad",
      icon = icon("tint"),
      color = "blue"
    )
  })
  
  output$viento_actual <- renderValueBox({
    meteo <- datos_meteo()
    if(is.null(meteo) || nrow(meteo) == 0) {
      viento_val <- "N/A"
    } else {
      viento_val <- paste0(round(meteo$vel_viento_media_ms[1], 1), " m/s")
    }
    
    valueBox(
      value = viento_val,
      subtitle = "Velocidad Viento",
      icon = icon("wind"),
      color = "green"
    )
  })
  
  # Tabla meteorológica
  output$tabla_meteo <- DT::renderDataTable({
    meteo <- datos_meteo()
    if(is.null(meteo)) return(NULL)
    
    tabla <- meteo %>%
      mutate(
        Fecha = format(fecha_hora, "%d/%m/%Y"),
        Hora = format(fecha_hora, "%H:%M"),
        `Temperatura (°C)` = round(temp_media_c, 1),
        `Humedad (%)` = round(humedad_media_pct, 1),
        `Viento (m/s)` = round(vel_viento_media_ms, 1),
        `Presión (hPa)` = round(presion_maxima_hpa, 1)
      ) %>%
      select(Fecha, Hora, `Temperatura (°C)`, `Humedad (%)`, `Viento (m/s)`, `Presión (hPa)`)
    
    DT::datatable(tabla, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Tabla de predicciones principales
  output$tabla_predicciones <- DT::renderDataTable({
    datos <- datos_predicciones()
    if(is.null(datos)) return(NULL)
    
    tabla <- datos %>%
      st_drop_geometry() %>%
      mutate(
        Fecha = format(fecha_hora, "%d/%m/%Y"),
        Hora = format(fecha_hora, "%H:%M"),
        Estación = nombre_estacion,
        Contaminante = contaminante,
        `Concentración (µg/m³)` = round(prediccion, 1)
      ) %>%
      select(Fecha, Hora, Estación, Contaminante, `Concentración (µg/m³)`) %>%
      arrange(desc(Fecha), desc(Hora))
    
    DT::datatable(tabla, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Descargas
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("madrid_air_quality_predictions_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      datos <- datos_predicciones()
      if(!is.null(datos)) {
        write.csv(st_drop_geometry(datos), file, row.names = FALSE)
      }
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() {
      paste("madrid_air_quality_predictions_", Sys.Date(), ".json", sep = "")
    },
    content = function(file) {
      datos <- datos_predicciones()
      if(!is.null(datos)) {
        jsonlite::write_json(st_drop_geometry(datos), file, pretty = TRUE)
      }
    }
  )
  
  # Resumen del sistema
  output$resumen_sistema <- renderText({
    datos <- datos_predicciones()
    meteo <- datos_meteo()
    
    if(is.null(datos)) {
      return("No data available")
    }
    
    resumen <- paste(
      "MADRID AIR QUALITY SYSTEM",
      "========================",
      "",
      paste("Predictions loaded:", nrow(datos), "records"),
      paste("Time range:", length(unique(datos$fecha_hora)), "hours"),
      paste("Monitoring stations:", length(unique(datos$id_estacion))),
      paste("Pollutants tracked:", length(unique(datos$contaminante))),
      "",
      "MODEL INFORMATION:",
      "- Algorithm: CARET Random Forest",
      "- R² performance: > 92%",
      "- Training data: 10 years historical",
      "- Variables: 34 predictors",
      "",
      paste("Last prediction update:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "EU AIR QUALITY STANDARDS:",
      "- NO₂: Good <40, Moderate <100, Poor <200 µg/m³",
      "- PM10: Good <25, Moderate <50, Poor <90 µg/m³", 
      "- O₃: Good <100, Moderate <140, Poor <180 µg/m³",
      sep = "\n"
    )
    
    return(resumen)
  })
}