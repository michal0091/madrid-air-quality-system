# --- SCRIPT 04: INGESTA DE DATOS METEOROLÓGICOS DIARIOS DE AEMET ---
# --------------------------------------------------------------------
# Objetivo: Descargar datos climatológicos diarios históricos (10+ años) 
# de estaciones de Madrid usando la API directa de AEMET.
# Endpoint: /api/valores/climatologicos/diarios/datos/fechaini/.../fechafin/.../estacion/...
# --------------------------------------------------------------------

# --- 1. CARGA DE LIBRERÍAS Y ENTORNO ----
renv::load()
library(DBI)
library(RPostgres)
library(data.table)
library(lubridate)
library(httr2)
library(jsonlite)
library(logger)
library(glue)

# Cargar funciones auxiliares
source("R/utils.R")

# --- 2. CONFIGURACIÓN ----
log_appender(appender_tee(glue("logs/aemet_load_{format(Sys.Date(), '%Y%m%d')}.log")))
log_info("--- INICIO DEL PROCESO DE INGESTA HISTÓRICA DE DATOS AEMET ---")

# Configuración temporal para datos diarios
ANO_INICIO_MINIMO <- 2010  # Mínimo 10 años de histórico
ANO_FIN <- year(today())
MESES_POR_BATCH <- 3  # Consultas de 3 meses (limitación API: máximo 6 meses)

# Verificar API Key
api_key <- Sys.getenv("AEMET_API_KEY")
if (nchar(api_key) == 0) {
  log_error("La variable de entorno AEMET_API_KEY no está configurada")
  stop("Configure AEMET_API_KEY antes de ejecutar este script")
}

# --- 3. FUNCIONES AUXILIARES ----

#' Verifica qué trimestres ya existen en la base de datos
verificar_trimestres_existentes <- function(db_conn, id_estacion) {
  tryCatch({
    query <- "
    SELECT DISTINCT 
      EXTRACT(YEAR FROM fecha) as ano,
      CEIL(EXTRACT(MONTH FROM fecha) / 3.0) as trimestre
    FROM fact_meteo_diaria 
    WHERE id_estacion_aemet = $1 
    ORDER BY ano, trimestre
    "
    
    result <- dbGetQuery(db_conn, query, params = list(id_estacion))
    if (nrow(result) > 0) {
      # Crear identificadores de trimestre "YYYY-Q1", "YYYY-Q2", etc.
      return(paste0(result$ano, "-Q", result$trimestre))
    } else {
      return(character(0))
    }
  }, error = function(e) {
    log_warn("Error al verificar trimestres existentes: {e$message}")
    return(character(0))
  })
}

#' Lista de estaciones meteorológicas principales de Madrid
obtener_estaciones_madrid <- function() {
  estaciones_madrid <- list(
    list(id = "3195", nombre = "Madrid-Retiro"),
    list(id = "3129", nombre = "Madrid-Barajas")
  )
  
  log_success("Configuradas {length(estaciones_madrid)} estaciones principales de Madrid")
  return(estaciones_madrid)
}

#' Descarga datos climatológicos diarios por trimestres (3 meses)
descargar_historico_por_trimestres <- function(api_key, estacion_id, trimestres_necesarios) {
  log_info("Descargando datos diarios para {length(trimestres_necesarios)} trimestres de estación {estacion_id}")
  
  datos_completos <- list()
  
  for (trimestre_id in trimestres_necesarios) {
    partes <- strsplit(trimestre_id, "-Q")[[1]]
    ano <- as.integer(partes[1])
    num_trimestre <- as.integer(partes[2])
    
    # [FIX 4] Lógica de cálculo de meses simplificada
    mes_inicio <- (num_trimestre - 1) * 3 + 1
    mes_fin <- num_trimestre * 3
    
    fecha_desde <- as.Date(paste0(ano, "-", sprintf("%02d", mes_inicio), "-01"))
    fecha_hasta <- as.Date(paste0(ano, "-", sprintf("%02d", mes_fin), "-01")) + months(1) - days(1)

    if (fecha_desde > Sys.Date()) next
    
    if (fecha_hasta > Sys.Date()) {
      fecha_hasta <- Sys.Date()
    }
    
    log_info("Descargando {trimestre_id} ({fecha_desde} a {fecha_hasta}) para estación {estacion_id}...")
    
    tryCatch({
      datos_trimestre <- obtener_datos_aemet_periodo(
        api_key = api_key,
        estacion_id = estacion_id,
        fecha_inicio = fecha_desde,
        fecha_fin = fecha_hasta,
        max_intentos = 3
      )
      
      if (!is.null(datos_trimestre) && nrow(datos_trimestre) > 0) {
        datos_completos[[trimestre_id]] <- datos_trimestre
        log_success("Trimestre {trimestre_id}: {nrow(datos_trimestre)} registros diarios")
      } else {
        log_warn("Sin datos para trimestre {trimestre_id}")
      }
      
      Sys.sleep(3)
      
    }, error = function(e) {
      log_error("Error descargando trimestre {trimestre_id}: {e$message}")
    })
  }
  
  if (length(datos_completos) > 0) {
    resultado <- rbindlist(datos_completos, fill = TRUE)
    log_info("Combinados datos de {length(datos_completos)} trimestres: {nrow(resultado)} registros totales")
    return(resultado)
  } else {
    return(data.table())
  }
}

# --- 4. SCRIPT PRINCIPAL ----
db_conn <- NULL

tryCatch({
  
  # --- CONEXIÓN A LA BASE DE DATOS ---
  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), 
    port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), 
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")
  
  # --- OBTENER ESTACIONES DE MADRID ---
  estaciones_madrid <- obtener_estaciones_madrid()
  total_registros_insertados <- 0
  
  # --- PROCESAR CADA ESTACIÓN ---
  for (i in seq_along(estaciones_madrid)) {
    estacion <- estaciones_madrid[[i]]
    estacion_id <- estacion$id
    estacion_nombre <- estacion$nombre
    
    log_info("--- Procesando estación {i}/{length(estaciones_madrid)}: {estacion_nombre} ({estacion_id}) ---")
    
    # Verificar qué trimestres ya existen
    trimestres_existentes <- verificar_trimestres_existentes(db_conn, estacion_id)
    
    # 1. Generar todas las combinaciones
    todos_trimestres_dt <- setDT(expand.grid(
      ano = ANO_INICIO_MINIMO:ANO_FIN,
      trimestre = 1:4
    ))
    todos_trimestres_dt[, trimestre_id := paste0(ano, "-Q", trimestre)]

    # 2. Convertir existentes a data.table para anti-join
    trimestres_existentes_dt <- data.table(trimestre_id = trimestres_existentes)

    # 3. Anti-join para encontrar los que faltan
    trimestres_necesarios_dt <- todos_trimestres_dt[!trimestres_existentes_dt, on = "trimestre_id"]
    
    # 4. Obtener vector y filtrar futuros
    trimestre_actual <- paste0(year(Sys.Date()), "-Q", ceiling(month(Sys.Date()) / 3))
    trimestres_necesarios <- trimestres_necesarios_dt[trimestre_id <= trimestre_actual, trimestre_id]

    
    if (length(trimestres_necesarios) == 0) {
      log_info("Todos los trimestres históricos ya están descargados para {estacion_nombre}")
      next
    }
    
    log_info("Faltan {length(trimestres_necesarios)} trimestres para {estacion_nombre}")
    log_info("Rango: {head(trimestres_necesarios, 1)} a {tail(trimestres_necesarios, 1)}")
    
    # Descargar datos históricos por trimestres
    datos_estacion <- descargar_historico_por_trimestres(
      api_key = api_key,
      estacion_id = estacion_id,
      trimestres_necesarios = trimestres_necesarios
    )
    
    if (nrow(datos_estacion) == 0) {
      log_warn("No se obtuvieron datos históricos para {estacion_nombre}")
      next
    }
    
    # Asignar por referencia
    if (is.na(datos_estacion$ubicacion[1]) || is.null(datos_estacion$ubicacion[1])) {
      datos_estacion[, ubicacion := estacion_nombre]
    }
    
    log_success("Descargados {nrow(datos_estacion)} registros climatológicos para {estacion_nombre}")
    
    # --- CARGAR DATOS EN LA BASE DE DATOS ---
    log_info("Cargando datos en la base de datos...")
    
    registros_insertados <- cargar_meteo_diarios_bulk(
      datos_meteo = datos_estacion,
      db_conn = db_conn,
      tabla_destino = "fact_meteo_diaria", 
      manejar_duplicados = TRUE
    )
    
    total_registros_insertados <- total_registros_insertados + registros_insertados
    log_success("Insertados {registros_insertados} nuevos registros para {estacion_nombre}")
  }
  
  # --- ESTADÍSTICAS FINALES ---
  log_success("=== PROCESO DE CARGA HISTÓRICA COMPLETADO ===")
  log_success("Total de registros insertados: {total_registros_insertados}")
  log_success("Estaciones procesadas: {length(estaciones_madrid)}")
  log_success("Período cubierto: {ANO_INICIO_MINIMO}-{ANO_FIN}")
  
  # --- VERIFICACIÓN FINAL ---
  log_info("Verificando datos en la base de datos...")
  
  query_verificacion <- "
  SELECT 
    id_estacion_aemet,
    ubicacion,
    COUNT(*) as total_registros,
    MIN(fecha) as fecha_min,
    MAX(fecha) as fecha_max,
    AVG(temp_media_c) as temp_media,
    AVG(precipitacion_mm) as precip_media_diaria,
    COUNT(DISTINCT DATE(fecha)) as dias_unicos
  FROM fact_meteo_diaria
  GROUP BY id_estacion_aemet, ubicacion
  ORDER BY id_estacion_aemet
  "
  
  verificacion <- dbGetQuery(db_conn, query_verificacion)
  
  if (nrow(verificacion) > 0) {
    log_info("Resumen final por estación:")
    # Este bucle 'for' para logging es aceptable
    for (i in 1:nrow(verificacion)) {
      est <- verificacion[i, ]
      log_info("  {est$id_estacion_aemet} ({est$ubicacion}):")
      log_info("    - Registros: {est$total_registros} ({est$dias_unicos} días únicos)")
      log_info("    - Período: {est$fecha_min} a {est$fecha_max}")
      log_info("    - Temp. media: {round(est$temp_media, 1)}°C")
      log_info("    - Precipitación media diaria: {round(est$precip_media_diaria, 1)}mm")
    }
    
    query_cobertura <- "
    SELECT 
      EXTRACT(YEAR FROM fecha) as ano,
      CEIL(EXTRACT(MONTH FROM fecha) / 3.0) as trimestre,
      COUNT(DISTINCT id_estacion_aemet) as estaciones_activas,
      COUNT(*) as registros_totales,
      AVG(temp_media_c) as temp_media_trimestral,
      SUM(precipitacion_mm) as precipitacion_total_trimestral
    FROM fact_meteo_diaria
    WHERE EXTRACT(YEAR FROM fecha) >= $1
    GROUP BY EXTRACT(YEAR FROM fecha), CEIL(EXTRACT(MONTH FROM fecha) / 3.0)
    ORDER BY ano, trimestre
    "
    
    cobertura <- dbGetQuery(db_conn, query_cobertura, params = list(ANO_INICIO_MINIMO))
    
    log_info("Cobertura temporal por trimestres (últimos 5 trimestres):")
    if (nrow(cobertura) > 0) {
      ultimos_trimestres <- tail(cobertura, 5)
      for (i in 1:nrow(ultimos_trimestres)) {
        cob <- ultimos_trimestres[i, ]
        log_info("  {cob$ano}-Q{cob$trimestre}: {cob$estaciones_activas} estaciones, {cob$registros_totales} días, temp {round(cob$temp_media_trimestral, 1)}°C, precipitación {round(cob$precipitacion_total_trimestral, 0)}mm")
      }
      log_info("  (Total de {nrow(cobertura)} trimestres en la base de datos)")
    }
    
    query_calidad <- "
    SELECT 
      COUNT(*) as total_registros,
      COUNT(temp_media_c) as registros_con_temperatura,
      COUNT(precipitacion_mm) as registros_con_precipitacion,
      COUNT(vel_viento_media_ms) as registros_con_viento,
      COUNT(humedad_media_pct) as registros_con_humedad,
      ROUND(COUNT(temp_media_c) * 100.0 / COUNT(*), 1) as pct_completitud_temp
    FROM fact_meteo_diaria
    "
    
    calidad <- dbGetQuery(db_conn, query_calidad)
    
    log_info("Calidad de los datos:")
    log_info("  - Completitud temperatura: {calidad$pct_completitud_temp}%")
    log_info("  - Registros con precipitación: {calidad$registros_con_precipitacion}")
    log_info("  - Registros con viento: {calidad$registros_con_viento}")
    log_info("  - Registros con humedad: {calidad$registros_con_humedad}")
    
  } else {
    log_warn("No se encontraron datos en la tabla fact_meteo_diaria")
  }
  
}, error = function(e) {
  log_error("Fallo durante la ingesta histórica de datos climatológicos: {e$message}")
  log_error("Stack trace: {paste(deparse(sys.calls()), collapse = '\n')}")
  
}, finally = {
  log_info("Cerrando conexión a la base de datos.")
  # Comprobación robusta
  if (!is.null(db_conn) && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})

log_info("--- PROCESO DE INGESTA HISTÓRICA AEMET FINALIZADO ---")