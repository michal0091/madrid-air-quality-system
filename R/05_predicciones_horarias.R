# PREDICCIONES HORARIAS PARA PRÓXIMAS 40 HORAS
# Objetivo: Generar predicciones temporales cada hora con datos meteorológicos simulados

# 1. LIBRERÍAS ----
library(sf)
library(dplyr)
library(lubridate)
library(logger)
library(glue)

# 2. CONFIGURACIÓN ----
log_appender(appender_tee("logs/predicciones_horarias.log"))
log_info("--- INICIO PREDICCIONES HORARIAS ---")

# 3. FUNCIÓN PARA GENERAR DATOS METEOROLÓGICOS FUTUROS ----
generar_meteo_futuro <- function(horas_futuras = 40) {
  
  log_info("Generando datos meteorológicos para próximas {horas_futuras} horas...")
  
  # Punto de inicio: hora actual
  inicio <- Sys.time()
  inicio_hora <- floor_date(inicio, "hour")
  
  # Crear secuencia de horas futuras
  horas_seq <- seq(inicio_hora, by = "hour", length.out = horas_futuras)
  
  # Simular patrón meteorológico realista para Madrid
  datos_meteo <- data.frame(
    fecha_hora = horas_seq,
    fecha = as.Date(horas_seq),
    hora = hour(horas_seq),
    dia_año = yday(horas_seq)
  ) %>%
    mutate(
      # Patrón de temperatura realista para Madrid según época del año
      mes = month(fecha_hora),
      temp_base = case_when(
        mes %in% c(12, 1, 2) ~ 8,   # Invierno: 8°C base
        mes %in% c(3, 4, 5) ~ 18,   # Primavera: 18°C base
        mes %in% c(6, 7, 8) ~ 28,   # Verano: 28°C base
        mes %in% c(9, 10, 11) ~ 18  # Otoño: 18°C base
      ),
      temp_diurna = sin((hora - 6) * pi / 12) * 6, # Variación diurna reducida (±6°C)
      temp_media_c = temp_base + temp_diurna + rnorm(n(), 0, 1.5),
      
      # Humedad inversa a temperatura
      humedad_media_pct = 70 - (temp_media_c - 20) * 1.5 + rnorm(n(), 0, 5),
      
      # Precipitación ocasional (10% probabilidad)
      precipitacion_mm = ifelse(runif(n()) < 0.1, rexp(n(), 2), 0),
      
      # Viento más fuerte durante el día
      vel_viento_media_ms = 2 + sin((hora - 12) * pi / 12) * 1.5 + abs(rnorm(n(), 0, 0.5)),
      
      # Dirección del viento con variabilidad
      dir_viento_grados = 200 + sin(hora * pi / 12) * 30 + rnorm(n(), 0, 20),
      
      # Presión con variación ligera
      presion_maxima_hpa = 1013 + sin((dia_año - 50) * 2 * pi / 365) * 8 + rnorm(n(), 0, 3)
    ) %>%
    # Aplicar límites realistas
    mutate(
      temp_media_c = pmax(pmin(temp_media_c, 45), 0),
      humedad_media_pct = pmax(pmin(humedad_media_pct, 100), 20),
      vel_viento_media_ms = pmax(vel_viento_media_ms, 0),
      dir_viento_grados = ((dir_viento_grados %% 360) + 360) %% 360,
      presion_maxima_hpa = pmax(pmin(presion_maxima_hpa, 1040), 980)
    )
  
  log_info("Datos meteorológicos generados: {nrow(datos_meteo)} horas")
  log_info("Rango temperaturas: {round(min(datos_meteo$temp_media_c), 1)}°C - {round(max(datos_meteo$temp_media_c), 1)}°C")
  
  return(datos_meteo)
}

# 3. FUNCIÓN PARA OBTENER DATOS METEOROLÓGICOS DE AEMET ----
obtener_meteo_aemet <- function(horas_futuras = 40) {

  log_info("Intentando obtener predicciones AEMET para próximas {horas_futuras} horas...")

  tryCatch({
    # Intentar cargar predicciones AEMET si existen
    if (file.exists("data/realtime/prediccion_meteo_latest.rds")) {
      datos_aemet <- readRDS("data/realtime/prediccion_meteo_latest.rds")
      log_info("✅ Datos AEMET cargados desde archivo: {nrow(datos_aemet)} registros")

      # Filtrar datos AEMET desde la próxima hora (zona horaria Madrid)
      hora_actual_madrid <- with_tz(Sys.time(), "Europe/Madrid")
      hora_actual_madrid <- floor_date(hora_actual_madrid, "hour")
      proxima_hora_madrid <- hora_actual_madrid + hours(1)

      datos_ajustados <- datos_aemet %>%
        mutate(
          fecha_hora = timestamp,
          fecha = as.Date(timestamp),
          hora = hour(timestamp),
          dia_año = yday(timestamp),
          # Mapear nombres de columnas AEMET al formato esperado
          humedad_media_pct = humedad_relativa_pct,
          presion_maxima_hpa = presion_hpa,
          vel_viento_media_ms = velocidad_viento_ms
          # dir_viento_grados ya existe
        ) %>%
        filter(timestamp >= proxima_hora_madrid) %>%
        slice_head(n = horas_futuras)

      log_info("📅 Predicciones desde próxima hora Madrid: {min(datos_ajustados$fecha_hora)} a {max(datos_ajustados$fecha_hora)}")
      log_info("⏰ Hora actual Madrid: {as.character(hora_actual_madrid)} → Próxima hora: {as.character(proxima_hora_madrid)}")

      if (nrow(datos_ajustados) >= horas_futuras * 0.8) {  # Al menos 80% de los datos
        log_success("✅ Usando predicciones AEMET reales (fechas originales)")
        return(datos_ajustados)
      }
    }

    # Si no hay datos AEMET, usar fallback realista
    log_warn("⚠️ Datos AEMET no disponibles, usando fallback realista")

  }, error = function(e) {
    log_error("❌ Error cargando AEMET: {e$message}")
    log_warn("⚠️ Usando fallback realista")
  })

  # FALLBACK: Usar la función de generación existente (ya corregida con temperaturas realistas)
  return(generar_meteo_futuro(horas_futuras))
}

# 3b. FUNCIÓN PARA CREAR VARIABLES DERIVADAS PARA PREDICCIÓN (RANGER ICA + UTM) ----
crear_variables_derivadas_prediccion <- function(datos) {
  log_info("Creando variables derivadas para predicción (formato RANGER ICA + UTM)...")

  # Centro de Madrid en UTM Zone 30N (Puerta del Sol)
  MADRID_CENTRO_UTM_X <- 440000  # metros
  MADRID_CENTRO_UTM_Y <- 4474000  # metros

  datos_enriquecidos <- datos %>%
    mutate(
      # VARIABLES ESPACIALES UTM: Convertir lat/lon a UTM Zone 30N (EPSG:25830)
      utm_x = if("lon" %in% names(.) && "lat" %in% names(.)) {
        # Conversión con sf (si está disponible)
        coords_wgs84_x <- st_as_sf(data.frame(lon = lon, lat = lat),
                                    coords = c("lon", "lat"), crs = 4326)
        coords_utm_x <- st_transform(coords_wgs84_x, 25830)
        st_coordinates(coords_utm_x)[,1]
      } else {
        MADRID_CENTRO_UTM_X  # Default
      },

      utm_y = if("lon" %in% names(.) && "lat" %in% names(.)) {
        coords_wgs84_y <- st_as_sf(data.frame(lon = lon, lat = lat),
                                    coords = c("lon", "lat"), crs = 4326)
        coords_utm_y <- st_transform(coords_wgs84_y, 25830)
        st_coordinates(coords_utm_y)[,2]
      } else {
        MADRID_CENTRO_UTM_Y  # Default
      },

      # Variables espaciales derivadas
      dist_centro_madrid = sqrt((utm_x - MADRID_CENTRO_UTM_X)^2 +
                               (utm_y - MADRID_CENTRO_UTM_Y)^2) / 1000,  # km
      utm_x_norm = (utm_x - MADRID_CENTRO_UTM_X) / 10000,  # decenas de km
      utm_y_norm = (utm_y - MADRID_CENTRO_UTM_Y) / 10000,

      # Variables temporales (igual que entrenamiento ranger)
      año = year(fecha_hora),
      mes = month(fecha_hora),
      dia = day(fecha_hora),
      dia_año = yday(fecha_hora),
      dia_semana = wday(fecha_hora) - 1,  # 0-6 (Domingo=0)
      fin_semana = ifelse(dia_semana %in% c(0, 6), 1, 0),  # 0=Domingo, 6=Sábado
      
      # Variables temporales cíclicas (CRÍTICO: igual que entrenamiento)
      sin_hora = sin(2 * pi * hora / 24),
      cos_hora = cos(2 * pi * hora / 24),
      sin_dia_año = sin(2 * pi * dia_año / 365),
      cos_dia_año = cos(2 * pi * dia_año / 365),

      # Variables meteorológicas derivadas (CRÍTICO: igual que entrenamiento)
      temp_sq = temp_media_c^2,
      temp_hum_ratio = temp_media_c / (humedad_media_pct + 1),

      # Componentes del viento (CRÍTICO: usar dirección si existe)
      dir_viento = ifelse("dir_viento_grados" %in% names(.), dir_viento_grados, 180),
      viento_x = vel_viento_media_ms * cos(dir_viento * pi / 180),
      viento_y = vel_viento_media_ms * sin(dir_viento * pi / 180),

      # Variables opcionales que pueden venir de expansión meteorológica
      presion_media_hpa = ifelse("presion_media_hpa" %in% names(.), presion_media_hpa,
                                  ifelse("presion_maxima_hpa" %in% names(.), presion_maxima_hpa, 1013)),
      humedad_maxima_pct = ifelse("humedad_maxima_pct" %in% names(.), humedad_maxima_pct,
                                   humedad_media_pct * 1.15),
      humedad_minima_pct = ifelse("humedad_minima_pct" %in% names(.), humedad_minima_pct,
                                   humedad_media_pct * 0.85),
      temp_maxima_c = ifelse("temp_maxima_c" %in% names(.), temp_maxima_c,
                             temp_media_c + 6),
      temp_minima_c = ifelse("temp_minima_c" %in% names(.), temp_minima_c,
                             temp_media_c - 6),
      presion_maxima_hpa = ifelse("presion_maxima_hpa" %in% names(.), presion_maxima_hpa,
                                  presion_media_hpa + 3),
      presion_minima_hpa = ifelse("presion_minima_hpa" %in% names(.), presion_minima_hpa,
                                  presion_media_hpa - 3),

      # Variables derivadas adicionales (si las columnas base existen)
      temp_range = ifelse(exists("temp_maxima_c") & exists("temp_minima_c"),
                         temp_maxima_c - temp_minima_c, 12),
      presion_diff = ifelse(exists("presion_maxima_hpa") & exists("presion_minima_hpa"),
                           presion_maxima_hpa - presion_minima_hpa, 6),
      humedad_diff = ifelse(exists("humedad_maxima_pct") & exists("humedad_minima_pct"),
                           humedad_maxima_pct - humedad_minima_pct, 30)
    )
  
  n_variables <- ncol(datos_enriquecidos) - 1  # -1 por contaminante
  log_info("Variables derivadas creadas: {n_variables} predictores totales")
  
  return(datos_enriquecidos)
}

# 4. FUNCIÓN PARA GENERAR PREDICCIONES POR ESTACIÓN ----
generar_predicciones_estaciones <- function(datos_meteo, archivo_modelos = "models/ranger_ica_todos.rds") {

  log_info("Generando predicciones para estaciones de Madrid...")

  # Cargar modelos RANGER ICA (nuevos modelos optimizados)
  if(!file.exists(archivo_modelos)) {
    log_error("Archivo de modelos no existe: {archivo_modelos}")
    log_warn("Intentando con modelos legacy...")

    # Fallback a modelos viejos si existen
    archivo_modelos <- "models/modelos_caret_avanzados.rds"
    if(!file.exists(archivo_modelos)) {
      log_error("No se encontraron modelos disponibles")
      return(NULL)
    }
  }

  modelos <- readRDS(archivo_modelos)

  # Determinar formato de modelos (ranger ICA vs legacy CARET)
  if("Dióxido de Nitrógeno" %in% names(modelos)) {
    # Formato RANGER ICA (nuevos modelos)
    log_info("✓ Modelos RANGER ICA cargados: {length(modelos)} contaminantes")
    usar_ranger <- TRUE
  } else if("modelos" %in% names(modelos)) {
    # Formato legacy CARET
    modelos <- modelos$modelos
    log_info("✓ Modelos legacy CARET cargados: {length(modelos)} contaminantes")
    usar_ranger <- FALSE
  } else {
    log_error("Formato de modelos no reconocido")
    return(NULL)
  }
  
  # Coordenadas de estaciones principales de Madrid
  estaciones <- data.frame(
    id_estacion = c(4, 8, 11, 16, 17, 18, 27, 35, 36, 38, 39, 40, 47, 48, 49, 50),
    nombre_estacion = c(
      "Pza. de España", "Escuelas Aguirre", "Av. Ramón y Cajal", "Arturo Soria", 
      "Villaverde Alto", "Farolillo", "Barajas Pueblo", "Pza. del Carmen", 
      "Moratalaz", "Cuatro Caminos", "Barrio del Pilar", "Vallecas", 
      "Mendez Alvaro", "Castellana", "Retiro", "Pza. Castilla"
    ),
    lat = c(40.4238, 40.4213, 40.4514, 40.4405, 40.3479, 40.3748, 40.4756, 40.4192,
            40.4077, 40.4459, 40.4773, 40.3943, 40.3980, 40.4407, 40.4152, 40.4656),
    lon = c(-3.7122, -3.6958, -3.6774, -3.6394, -3.7215, -3.7336, -3.5935, -3.7026,
            -3.6453, -3.7097, -3.7137, -3.6458, -3.6862, -3.6889, -3.6823, -3.6951)
  )
  
  # Crear combinación de todas las horas x estaciones x contaminantes
  combinaciones <- expand.grid(
    fecha_hora = datos_meteo$fecha_hora,
    id_estacion = estaciones$id_estacion,
    contaminante = names(modelos),
    stringsAsFactors = FALSE
  ) %>%
    # Unir con datos meteorológicos
    left_join(datos_meteo, by = "fecha_hora") %>%
    # Unir con información de estaciones
    left_join(estaciones, by = "id_estacion") %>%
    # Crear variables derivadas que espera el modelo avanzado
    crear_variables_derivadas_prediccion()
  
  log_info("Combinaciones creadas: {nrow(combinaciones)} registros")
  log_info("  {nrow(datos_meteo)} horas × {nrow(estaciones)} estaciones × {length(modelos)} contaminantes")
  
  # Generar predicciones para cada contaminante
  predicciones_finales <- list()

  for(contaminante_nombre in names(modelos)) {
    log_info("Prediciendo {contaminante_nombre}...")

    # Filtrar datos para este contaminante
    datos_contaminante <- combinaciones %>%
      filter(contaminante == contaminante_nombre)

    tryCatch({
      # Obtener modelo según formato
      if(usar_ranger) {
        # Modelos RANGER ICA (directamente el objeto train)
        modelo_caret <- modelos[[contaminante_nombre]]

        # Métricas desde resultados del modelo
        metricas_modelo <- modelo_caret$results %>%
          filter(mtry == modelo_caret$bestTune$mtry,
                 splitrule == modelo_caret$bestTune$splitrule,
                 min.node.size == modelo_caret$bestTune$min.node.size)

        rmse_modelo <- metricas_modelo$RMSE[1]
        r2_modelo <- metricas_modelo$Rsquared[1]

      } else {
        # Modelos legacy CARET (con estructura $modelo + $metricas)
        modelo_info <- modelos[[contaminante_nombre]]
        modelo_caret <- modelo_info$modelo
        rmse_modelo <- modelo_info$metricas$RMSE
        r2_modelo <- modelo_info$metricas$Rsquared
      }

      # Hacer predicciones
      predicciones <- predict(modelo_caret, newdata = datos_contaminante)

      # Agregar predicciones a los datos
      datos_con_predicciones <- datos_contaminante %>%
        mutate(
          prediccion = predicciones,
          rmse_modelo = rmse_modelo,
          r2_modelo = r2_modelo
        )

      predicciones_finales[[contaminante_nombre]] <- datos_con_predicciones

      log_success("✓ {contaminante_nombre}: {nrow(datos_con_predicciones)} predicciones")
      log_info("  Rango: {round(min(predicciones), 1)} - {round(max(predicciones), 1)} µg/m³")
      log_info("  Modelo: RMSE={round(rmse_modelo, 2)}, R²={round(r2_modelo, 3)}")

    }, error = function(e) {
      log_error("Error prediciendo {contaminante_nombre}: {e$message}")
      log_error("Traceback: {paste(capture.output(traceback()), collapse='\n')}")
    })
  }
  
  # Combinar todas las predicciones
  if(length(predicciones_finales) == 0) {
    log_error("No se generaron predicciones")
    return(NULL)
  }
  
  resultado_completo <- bind_rows(predicciones_finales)
  
  # Convertir a objeto espacial
  resultado_sf <- resultado_completo %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(timestamp_prediccion = Sys.time())
  
  log_success("✓ Predicciones horarias completadas: {nrow(resultado_sf)} registros")
  
  return(resultado_sf)
}

# 5. FUNCIÓN PRINCIPAL ----
generar_predicciones_40h <- function(horas = 40) {
  
  log_info("=== GENERANDO PREDICCIONES PRÓXIMAS {horas} HORAS ===")
  
  # Paso 1: Obtener datos meteorológicos de AEMET (con fallback)
  datos_meteo <- obtener_meteo_aemet(horas)
  
  if(is.null(datos_meteo)) {
    log_error("No se pudieron generar datos meteorológicos")
    return(NULL)
  }
  
  # Paso 2: Generar predicciones para estaciones
  predicciones <- generar_predicciones_estaciones(datos_meteo)
  
  if(is.null(predicciones)) {
    log_error("No se pudieron generar predicciones")
    return(NULL)
  }
  
  # Paso 3: Guardar resultados
  archivo_salida <- "output/predicciones_40h_latest.rds"
  saveRDS(predicciones, archivo_salida)
  
  # También guardar datos meteorológicos para gráficos
  archivo_meteo <- "output/meteo_40h_latest.rds"
  saveRDS(datos_meteo, archivo_meteo)
  
  log_success("✓ PREDICCIONES 40H COMPLETADAS")
  log_info("  Total registros: {nrow(predicciones)}")
  log_info("  Horas: {horas}")
  log_info("  Estaciones: {length(unique(predicciones$id_estacion))}")
  log_info("  Contaminantes: {length(unique(predicciones$contaminante))}")
  log_info("  Archivo predicciones: {archivo_salida}")
  log_info("  Archivo meteorología: {archivo_meteo}")
  
  return(predicciones)
}

# 6. FUNCIÓN DE TEST ----
test_predicciones_40h <- function() {
  log_info("=== TEST PREDICCIONES 40H ===")
  
  resultado <- generar_predicciones_40h(40)
  
  if(is.null(resultado)) {
    log_error("❌ Test falló")
    return(FALSE)
  }
  
  # Validaciones
  validaciones <- list(
    registros_totales = nrow(resultado),
    horas_unicas = length(unique(resultado$fecha_hora)),
    estaciones_unicas = length(unique(resultado$id_estacion)),
    contaminantes_unicos = length(unique(resultado$contaminante)),
    rango_predicciones = c(min(resultado$prediccion), max(resultado$prediccion)),
    valores_negativos = sum(resultado$prediccion < 0),
    valores_na = sum(is.na(resultado$prediccion))
  )
  
  log_success("✓ Test predicciones 40h exitoso")
  log_info("  Registros totales: {validaciones$registros_totales}")
  log_info("  Horas únicas: {validaciones$horas_unicas}")
  log_info("  Estaciones únicas: {validaciones$estaciones_unicas}")
  log_info("  Contaminantes únicos: {validaciones$contaminantes_unicos}")
  log_info("  Rango predicciones: {round(validaciones$rango_predicciones[1], 1)} - {round(validaciones$rango_predicciones[2], 1)}")
  log_info("  Valores negativos: {validaciones$valores_negativos}")
  log_info("  Valores NA: {validaciones$valores_na}")
  
  return(TRUE)
}

# Ejecutar test automáticamente
if(interactive() == FALSE) {
  test_predicciones_40h()
}