# FUNCIONES AUXILIARES PARA DATOS
# Utilidades para cargar y procesar datos del dashboard

# Cargar datos de predicciones
cargar_datos <- function() {
  tryCatch({
    # Try multiple path combinations
    paths <- c(
      "output/predicciones_xgb_nativo_40h_latest.rds",
      "../output/predicciones_xgb_nativo_40h_latest.rds",
      file.path(getwd(), "..", "output", "predicciones_xgb_nativo_40h_latest.rds")
    )
    
    for(path in paths) {
      if(file.exists(path)) {
        cat("Cargando datos desde:", path, "\n")
        return(readRDS(path))
      }
    }
    
    # Si no hay datos, crear datos de ejemplo
    cat("No se encontraron datos reales, generando datos de ejemplo\n")
    return(generar_datos_ejemplo())
  }, error = function(e) {
    cat("Error cargando datos:", e$message, "\n")
    return(generar_datos_ejemplo())
  })
}

# Cargar datos meteorológicos
cargar_meteo <- function() {
  tryCatch({
    # Try multiple path combinations
    paths <- c(
      "output/meteo_40h_latest.rds",
      "../output/meteo_40h_latest.rds", 
      file.path(getwd(), "..", "output", "meteo_40h_latest.rds")
    )
    
    for(path in paths) {
      if(file.exists(path)) {
        cat("Cargando datos meteo desde:", path, "\n")
        return(readRDS(path))
      }
    }
    
    # Si no hay datos, crear datos de ejemplo
    cat("No se encontraron datos meteo reales, generando datos de ejemplo\n")
    return(generar_meteo_ejemplo())
  }, error = function(e) {
    cat("Error cargando datos meteo:", e$message, "\n")
    return(generar_meteo_ejemplo())
  })
}

# Generar datos de ejemplo para predicciones
generar_datos_ejemplo <- function() {
  library(sf)
  library(lubridate)
  
  # Coordenadas de estaciones de Madrid (ejemplos reales)
  estaciones <- data.frame(
    nombre_estacion = c(
      "Plaza de España", "Escuelas Aguirre", "Ramón y Cajal", 
      "Arturo Soria", "Villaverde", "Farolillo", "Casa de Campo",
      "Barajas Pueblo", "Plaza del Carmen", "Moratalaz",
      "Cuatro Caminos", "Barrio del Pilar", "Vallecas",
      "Méndez Álvaro", "Castellana", "Retiro"
    ),
    lon = c(-3.712, -3.682, -3.677, -3.639, -3.706, -3.731, -3.751,
            -3.579, -3.703, -3.645, -3.703, -3.703, -3.651, 
            -3.682, -3.688, -3.682),
    lat = c(40.424, 40.421, 40.457, 40.440, 40.347, 40.381, 40.419,
            40.479, 40.419, 40.407, 40.431, 40.477, 40.392,
            40.398, 40.440, 40.415)
  )
  
  # Generar datos para las próximas 40 horas
  fechas <- seq(from = Sys.time(), by = "hour", length.out = 40)
  contaminantes <- c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Ozono")
  
  datos_ejemplo <- expand.grid(
    fecha_hora = fechas,
    nombre_estacion = estaciones$nombre_estacion,
    contaminante = contaminantes,
    stringsAsFactors = FALSE
  )
  
  # Añadir coordenadas
  datos_ejemplo <- merge(datos_ejemplo, estaciones, by = "nombre_estacion")
  
  # Generar predicciones realistas con variación por estación
  # NO usar set.seed para tener variación real
  
  # Crear variación base por estación (algunas zonas más contaminadas)
  factores_estacion <- c(
    "Plaza de España" = 1.3, "Escuelas Aguirre" = 1.2, "Ramón y Cajal" = 0.9,
    "Arturo Soria" = 0.8, "Villaverde" = 1.4, "Farolillo" = 1.1, "Casa de Campo" = 0.7,
    "Barajas Pueblo" = 0.9, "Plaza del Carmen" = 1.3, "Moratalaz" = 1.0,
    "Cuatro Caminos" = 1.2, "Barrio del Pilar" = 0.9, "Vallecas" = 1.3,
    "Méndez Álvaro" = 1.4, "Castellana" = 1.1, "Retiro" = 0.8
  )
  
  # Añadir factor de estación
  datos_ejemplo$factor_estacion <- factores_estacion[datos_ejemplo$nombre_estacion]
  
  # Generar predicciones con variación temporal y espacial
  datos_ejemplo$hora_dia <- as.numeric(format(datos_ejemplo$fecha_hora, "%H"))
  datos_ejemplo$factor_temporal <- 0.8 + 0.4 * sin(2 * pi * (datos_ejemplo$hora_dia + 6) / 24)
  
  # Predicciones base por contaminante con variación estacional y espacial
  datos_ejemplo$prediccion <- case_when(
    datos_ejemplo$contaminante == "Dióxido de Nitrógeno" ~ 
      (15 + rnorm(nrow(datos_ejemplo), 0, 6)) * datos_ejemplo$factor_estacion * datos_ejemplo$factor_temporal,
    datos_ejemplo$contaminante == "Partículas < 10 µm" ~ 
      (12 + rnorm(nrow(datos_ejemplo), 0, 4)) * datos_ejemplo$factor_estacion * (1.2 - datos_ejemplo$factor_temporal * 0.3),
    datos_ejemplo$contaminante == "Ozono" ~ 
      (35 + rnorm(nrow(datos_ejemplo), 0, 10)) * (2 - datos_ejemplo$factor_estacion) * datos_ejemplo$factor_temporal
  )
  
  # Asegurar valores positivos y realistas
  datos_ejemplo$prediccion <- pmax(datos_ejemplo$prediccion, 1)
  datos_ejemplo$prediccion <- pmin(datos_ejemplo$prediccion, 
    case_when(
      datos_ejemplo$contaminante == "Dióxido de Nitrógeno" ~ 80,
      datos_ejemplo$contaminante == "Partículas < 10 µm" ~ 60,
      datos_ejemplo$contaminante == "Ozono" ~ 120
    )
  )
  
  # Añadir otras columnas necesarias con variación realista
  datos_ejemplo$rmse_modelo <- runif(nrow(datos_ejemplo), 2, 5)
  datos_ejemplo$timestamp_prediccion <- Sys.time()
  datos_ejemplo$temp_media_c <- 15 + 10 * sin(2 * pi * datos_ejemplo$hora_dia / 24) + rnorm(nrow(datos_ejemplo), 0, 3)
  datos_ejemplo$humedad_media_pct <- 50 + 25 * cos(2 * pi * datos_ejemplo$hora_dia / 24) + runif(nrow(datos_ejemplo), -10, 10)
  datos_ejemplo$velocidad_viento_kmh <- abs(rnorm(nrow(datos_ejemplo), mean = 12, sd = 4))
  datos_ejemplo$dir_viento_grados <- runif(nrow(datos_ejemplo), 0, 360)
  
  # Limpiar columnas temporales
  datos_ejemplo$factor_estacion <- NULL
  datos_ejemplo$hora_dia <- NULL
  datos_ejemplo$factor_temporal <- NULL
  
  # Convertir a sf object
  datos_sf <- st_as_sf(datos_ejemplo, coords = c("lon", "lat"), crs = 4326)
  
  return(datos_sf)
}

# Generar datos meteorológicos de ejemplo
generar_meteo_ejemplo <- function() {
  library(lubridate)
  
  # Generar datos para las próximas 40 horas
  fechas <- seq(from = Sys.time(), by = "hour", length.out = 40)
  
  datos_meteo <- data.frame(
    fecha_hora = fechas,
    temp_media_c = 15 + 10 * sin(2 * pi * (as.numeric(fechas) - as.numeric(fechas[1])) / (24 * 3600)) + rnorm(40, 0, 2),
    humedad_media_pct = 60 + 20 * sin(2 * pi * (as.numeric(fechas) - as.numeric(fechas[1])) / (24 * 3600)) + rnorm(40, 0, 5),
    velocidad_viento_kmh = abs(rnorm(40, mean = 12, sd = 4)),
    dir_viento_grados = runif(40, 0, 360),
    presion_hpa = rnorm(40, mean = 1013, sd = 5)
  )
  
  return(datos_meteo)
}