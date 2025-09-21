# FUNCIONES AUXILIARES PARA VISUALIZACIÓN
# Utilidades para mapas y gráficos del dashboard

# Función para crear paleta de colores según contaminante
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