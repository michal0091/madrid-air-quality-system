# BRAND UTILITIES
# Funciones para cargar y aplicar la configuración de marca

library(yaml)

# Cargar configuración de marca
load_brand_config <- function() {
  yaml::read_yaml("config/brand.yml")
}

# Obtener colores de la paleta
get_brand_colors <- function() {
  brand_config <- load_brand_config()
  brand_config$color$palette
}

# Obtener color específico
get_brand_color <- function(color_name) {
  colors <- get_brand_colors()
  colors[[color_name]]
}

# Obtener paleta semántica
get_semantic_colors <- function() {
  brand_config <- load_brand_config()
  semantic <- brand_config$color
  
  list(
    foreground = semantic$foreground,
    background = semantic$background,
    primary = semantic$primary,
    secondary = semantic$secondary,
    success = semantic$success,
    info = semantic$info,
    warning = semantic$warning,
    danger = semantic$danger,
    light = semantic$light,
    dark = semantic$dark,
    link = semantic$link
  )
}

# Generar CSS personalizado basado en la marca
generate_brand_css <- function() {
  colors <- get_brand_colors()
  semantic <- get_semantic_colors()
  
  # Mapear colores semánticos a valores de paleta
  resolve_color <- function(color_ref) {
    if(color_ref %in% names(colors)) {
      colors[[color_ref]]
    } else {
      color_ref
    }
  }
  
  primary_color <- resolve_color(semantic$primary)
  secondary_color <- resolve_color(semantic$secondary)
  background_color <- resolve_color(semantic$background)
  foreground_color <- resolve_color(semantic$foreground)
  light_color <- resolve_color(semantic$light)
  
  css <- paste0("
    /* Kinelytics Brand Colors */
    :root {
      --brand-primary: ", primary_color, ";
      --brand-secondary: ", secondary_color, ";
      --brand-background: ", background_color, ";
      --brand-foreground: ", foreground_color, ";
      --brand-light: ", light_color, ";
      --brand-success: ", resolve_color(semantic$success), ";
      --brand-warning: ", resolve_color(semantic$warning), ";
      --brand-danger: ", resolve_color(semantic$danger), ";
      --brand-info: ", resolve_color(semantic$info), ";
    }
    
    /* Import Google Fonts */
    @import url('https://fonts.googleapis.com/css2?family=Exo+2:wght@400;600;800&family=Fira+Mono:wght@400&display=swap');
    
    /* Typography */
    body, .content-wrapper, .main-sidebar {
      font-family: 'Exo 2', sans-serif;
      color: var(--brand-foreground);
      background-color: var(--brand-background);
    }
    
    h1, h2, h3, h4, h5, h6 {
      font-family: 'Exo 2', sans-serif;
      font-weight: 800;
      color: var(--brand-primary);
    }
    
    code, pre {
      font-family: 'Fira Mono', monospace;
    }
    
    /* Modern Layout */
    .content-wrapper {
      background: linear-gradient(135deg, var(--brand-background) 0%, #fafcfa 100%);
      min-height: 100vh;
    }
    
    /* Header styling */
    .main-header {
      background: var(--brand-primary) !important;
      border-bottom: none;
      box-shadow: 0 2px 10px rgba(31, 58, 61, 0.1);
    }
    
    .main-header .navbar {
      background: transparent !important;
    }
    
    .main-header .logo {
      background: var(--brand-primary) !important;
      color: var(--brand-background) !important;
      font-weight: 800;
      border-right: 1px solid rgba(240, 245, 239, 0.2);
    }
    
    /* Sidebar styling */
    .main-sidebar {
      background: var(--brand-primary) !important;
    }
    
    .sidebar-menu > li > a {
      color: var(--brand-background) !important;
      font-weight: 400;
      transition: all 0.3s ease;
    }
    
    .sidebar-menu > li > a:hover,
    .sidebar-menu > li.active > a {
      background: var(--brand-secondary) !important;
      color: var(--brand-background) !important;
      border-left: 4px solid var(--brand-light);
    }
    
    /* Cards and boxes */
    .box {
      border: none;
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(20, 32, 33, 0.08);
      background: white;
      transition: transform 0.2s ease;
    }
    
    .box:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 30px rgba(20, 32, 33, 0.12);
    }
    
    .box-header {
      background: linear-gradient(135deg, var(--brand-primary) 0%, var(--brand-secondary) 100%);
      color: var(--brand-background);
      border-radius: 12px 12px 0 0;
      border-bottom: none;
    }
    
    .box-title {
      font-weight: 600;
      font-size: 1.1em;
    }
    
    /* Metrics boxes */
    .metric-box {
      background: linear-gradient(135deg, var(--brand-secondary) 0%, var(--brand-info) 100%);
      border-radius: 12px;
      padding: 24px;
      color: var(--brand-background);
      text-align: center;
      margin: 12px 0;
      box-shadow: 0 4px 15px rgba(44, 94, 104, 0.2);
      transition: all 0.3s ease;
    }
    
    .metric-box:hover {
      transform: translateY(-3px);
      box-shadow: 0 8px 25px rgba(44, 94, 104, 0.3);
    }
    
    .metric-value {
      font-size: 2.2em;
      font-weight: 800;
      margin: 12px 0;
      text-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    .metric-label {
      font-size: 0.9em;
      opacity: 0.95;
      font-weight: 400;
    }
    
    /* Buttons */
    .btn-primary {
      background: var(--brand-primary);
      border-color: var(--brand-primary);
      font-weight: 600;
    }
    
    .btn-primary:hover {
      background: var(--brand-secondary);
      border-color: var(--brand-secondary);
    }
    
    /* Map container */
    .leaflet-container {
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(20, 32, 33, 0.1);
    }
    
    /* Value boxes */
    .small-box {
      border-radius: 12px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    
    .small-box.bg-red {
      background: var(--brand-danger) !important;
    }
    
    .small-box.bg-blue {
      background: var(--brand-info) !important;
    }
    
    .small-box.bg-green {
      background: var(--brand-success) !important;
    }
    
    /* Responsive adjustments */
    @media (max-width: 768px) {
      .metric-box {
        margin: 8px 0;
        padding: 16px;
      }
      
      .metric-value {
        font-size: 1.8em;
      }
    }
    
    /* Custom scrollbar */
    ::-webkit-scrollbar {
      width: 8px;
    }
    
    ::-webkit-scrollbar-track {
      background: var(--brand-light);
    }
    
    ::-webkit-scrollbar-thumb {
      background: var(--brand-secondary);
      border-radius: 4px;
    }
    
    ::-webkit-scrollbar-thumb:hover {
      background: var(--brand-primary);
    }
  ")
  
  return(css)
}