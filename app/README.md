# Madrid Air Quality Dashboard

Modern air quality monitoring dashboard for Madrid, inspired by [Montreal Curbcut](https://montreal.curbcut.ca/) and built with Kinelytics branding guidelines.

## 🚀 Quick Start

```r
# From project root
setwd("app")
source("app.R")
ejecutar_dashboard()

# Or use the wrapper function
source("R/08_dashboard_shiny.R")
ejecutar_dashboard_modular()
```

## 🏗️ Architecture

This Shiny application follows **Mastering Shiny** best practices with a modular structure:

```
app/
├── app.R                    # Main entry point
├── global.R                 # Global configuration and dependencies
├── ui.R                     # User interface
├── server.R                 # Server logic
├── config/
│   └── brand.yml           # Kinelytics brand configuration
├── R/
│   ├── brand_utils.R       # Brand color and typography utilities
│   ├── data_utils.R        # Data loading functions
│   ├── modern_ui_utils.R   # Modern UI components
│   └── visualization_utils.R # Chart and map utilities
└── www/
    ├── custom.css          # Additional styling
    └── logos/              # Brand assets (placeholder)
```

## 🎨 Design Features

### Montreal Curbcut Inspired
- **Clean, modern interface** with data-driven urban analytics focus
- **Interactive maps** with dynamic visualization layers
- **Responsive design** optimized for all screen sizes
- **Intuitive navigation** with clear visual hierarchy

### Kinelytics Branding
- **Color Palette**: Blue-green primary (#1f3a3d), complementary secondary colors
- **Typography**: Exo 2 font family for headings and body text
- **Visual Elements**: Gradient backgrounds, modern card layouts, enhanced shadows

### Technical Enhancements
- **CSS Variables** for consistent theming
- **Responsive Grid System** with mobile-first approach
- **Accessibility Features** (ARIA labels, high contrast support, reduced motion)
- **Performance Optimizations** (lazy loading, efficient rendering)

## 📊 Data Sources

- **Air Quality**: Madrid Open Data Portal (real-time API + fallback system)
- **Weather**: AEMET (Spanish Meteorological Agency)
- **Predictions**: CARET Random Forest models with R² > 0.96

## 🛠️ Dependencies

Core packages loaded in `global.R`:
- `shiny`, `shinydashboard` - Framework
- `leaflet`, `plotly` - Interactive visualizations  
- `DT` - Enhanced data tables
- `sf`, `dplyr` - Spatial and data manipulation
- `yaml` - Brand configuration

## 🔧 Configuration

### Brand Settings

Edit `config/brand.yml` to customize:
- Color palette
- Typography (Google Fonts)
- Logo paths
- Semantic color mapping

### Environment Variables

Required for full functionality:
```bash
DB_HOST=your_host
DB_PORT=5432
DB_NAME=air_quality_db
DB_USER=air_quality_user
DB_PASSWORD=your_password
AEMET_API_KEY=your_aemet_key
```

## 📱 Responsive Design

The dashboard adapts to different screen sizes:
- **Desktop** (>1200px): Full sidebar, 4-column metrics
- **Tablet** (768-1200px): Collapsible sidebar, 2-column layout
- **Mobile** (<768px): Overlay sidebar, single-column stacked layout

## 🎯 Key Features

1. **Interactive Map Tab**
   - Real-time air quality predictions across Madrid
   - Color-coded station markers with pollution levels
   - Dynamic legend and popup information

2. **Temporal Analysis Tab**  
   - 40-hour forecast timeline
   - Comparative pollutant analysis
   - Statistical summaries and trends

3. **Weather Conditions Tab**
   - Meteorological data integration
   - Temperature, humidity, wind visualizations
   - Weather impact on air quality

4. **Data & Statistics Tab**
   - Downloadable datasets (CSV, JSON)
   - System performance metrics
   - Real-time data validation

5. **About Tab**
   - Technical documentation
   - Model performance details
   - Kinelytics branding showcase

## 🔄 Updates

The dashboard automatically refreshes data from:
- Real-time prediction outputs (`output/predicciones_40h_latest.rds`)
- Weather forecasts (`output/meteo_40h_latest.rds`)
- GitHub Actions pipeline (hourly updates)

## 🎨 Customization

### Adding New Colors
```r
# In config/brand.yml
color:
  palette:
    new-color: "#your-hex-code"
  
# Then reference in semantic mapping
  new-role: new-color
```

### Creating Custom Components
```r
# In R/modern_ui_utils.R
my_custom_component <- function(data) {
  brand_colors <- get_brand_colors()
  # Use brand_colors$primary, etc.
}
```

## 📈 Performance

- **Reactive Programming**: Efficient data flow with Shiny reactives
- **Caching**: Automatic data file caching for faster loads  
- **Code Splitting**: Modular architecture for maintainability
- **CSS Optimization**: Minified styles with variable-based theming

## 🌐 Deployment

The dashboard is designed for:
- **Local Development**: RStudio or command line
- **Shiny Server**: Production deployment
- **Docker**: Containerized deployment (via parent project)
- **Cloud Platforms**: AWS, Azure, GCP compatible

---

**Powered by Kinelytics** - Advanced analytics for urban environmental monitoring