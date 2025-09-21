# MODERN UI UTILITIES
# Additional UI components inspired by Montreal Curbcut

# Modern value box with gradient background
modern_value_box <- function(value, subtitle, icon_name, color_class = "primary") {
  brand_colors <- get_brand_colors()
  
  color_map <- list(
    primary = brand_colors$`blue-green`,
    secondary = brand_colors$green,
    info = brand_colors$blue,
    success = brand_colors$`light-green`,
    warning = brand_colors$orange,
    danger = brand_colors$red
  )
  
  bg_color <- color_map[[color_class]] %||% brand_colors$`blue-green`
  
  div(
    class = "modern-value-box",
    style = paste0(
      "background: linear-gradient(135deg, ", bg_color, " 0%, ", 
      adjustcolor(bg_color, alpha.f = 0.8), " 100%);",
      "border-radius: 12px; padding: 20px; color: white; text-align: center;",
      "box-shadow: 0 4px 15px rgba(0,0,0,0.1); margin: 10px 0;",
      "transition: transform 0.2s ease;"
    ),
    onmouseover = "this.style.transform='translateY(-3px)'",
    onmouseout = "this.style.transform='translateY(0)'",
    
    div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
      tags$i(class = paste0("fa fa-", icon_name), style = "font-size: 1.5em; margin-right: 10px;"),
      h4(subtitle, style = "margin: 0; font-weight: 600;")
    ),
    div(
      style = "font-size: 2.2em; font-weight: 800; margin: 10px 0;",
      value
    )
  )
}

# Modern progress indicator
progress_indicator <- function(value, max_value = 100, label = "", color = "#2c5e68") {
  percentage <- round((value / max_value) * 100, 1)
  
  div(
    class = "progress-indicator",
    style = "margin: 15px 0;",
    
    # Label
    if(nchar(label) > 0) {
      div(style = "margin-bottom: 5px; font-weight: 500; color: #142021;", label)
    },
    
    # Progress bar container
    div(
      style = paste0(
        "background: #f0f5ef; border-radius: 20px; height: 8px;",
        "overflow: hidden; box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);"
      ),
      
      # Progress bar fill
      div(
        style = paste0(
          "background: linear-gradient(90deg, ", color, " 0%, ",
          adjustcolor(color, alpha.f = 0.7), " 100%);",
          "height: 100%; width: ", percentage, "%;",
          "border-radius: 20px; transition: width 0.3s ease;"
        )
      )
    ),
    
    # Percentage text
    div(
      style = "text-align: right; margin-top: 5px; font-size: 0.9em; color: #507c8b;",
      paste0(percentage, "%")
    )
  )
}

# Modern card component
modern_card <- function(title, content, icon_name = NULL, color = "primary") {
  brand_colors <- get_brand_colors()
  
  header_style <- if(!is.null(icon_name)) {
    paste0(
      "background: linear-gradient(135deg, ", brand_colors$`blue-green`, " 0%, ",
      brand_colors$green, " 100%); color: white; padding: 15px;",
      "border-radius: 12px 12px 0 0; display: flex; align-items: center;"
    )
  } else {
    "padding: 15px; border-bottom: 2px solid #b8d6c6;"
  }
  
  div(
    class = "modern-card",
    style = paste0(
      "background: white; border-radius: 12px;",
      "box-shadow: 0 4px 20px rgba(20, 32, 33, 0.08);",
      "transition: transform 0.2s ease, box-shadow 0.2s ease;",
      "margin: 15px 0;"
    ),
    onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 8px 30px rgba(20, 32, 33, 0.12)';",
    onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 20px rgba(20, 32, 33, 0.08)';",
    
    # Header
    div(
      style = header_style,
      if(!is.null(icon_name)) {
        tags$i(class = paste0("fa fa-", icon_name), style = "margin-right: 10px; font-size: 1.2em;")
      },
      h4(title, style = "margin: 0; font-weight: 600;")
    ),
    
    # Content
    div(
      style = "padding: 20px;",
      content
    )
  )
}

# Responsive grid helper
responsive_grid <- function(..., breakpoints = list(xs = 12, sm = 6, md = 4, lg = 3)) {
  columns <- list(...)
  
  fluidRow(
    lapply(seq_along(columns), function(i) {
      column(
        width = breakpoints$lg,
        class = paste0(
          "col-xs-", breakpoints$xs, " ",
          "col-sm-", breakpoints$sm, " ", 
          "col-md-", breakpoints$md
        ),
        columns[[i]]
      )
    })
  )
}