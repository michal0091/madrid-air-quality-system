# Sistema Automatizado de Análisis y Predicción de la Calidad del Aire de Madrid

![R Version](https://img.shields.io/badge/R-4.5.1-blue.svg) ![PostgreSQL](https://img.shields.io/badge/PostgreSQL-15-blue.svg) ![Shiny](https://img.shields.io/badge/Shiny-1.8.0-blue.svg) ![GitHub Actions](https://img.shields.io/badge/CI/CD-GitHub%20Actions-green.svg)

## 1. Objetivo del Proyecto

Este proyecto implementa un sistema completo y automatizado para la recolección, análisis, predicción y visualización de la calidad del aire en Madrid. El sistema opera sobre una Raspberry Pi 5, utilizando un pipeline de datos robusto con PostgreSQL/PostGIS y se despliega de forma continua mediante GitHub Actions.

El objetivo principal es servir como una pieza de portfolio que demuestra habilidades integrales en **Data Engineering**, **Modelado Geoestadístico (ML)**, **MLOps** y **Visualización Interactiva**.

## 2. Arquitectura y Tecnologías

El sistema se compone de un backend para la ingesta y almacenamiento de datos, un motor analítico para el modelado y una aplicación web para la interacción del usuario.

* **Hardware:** Raspberry Pi 5 (Raspberry Pi OS 64-bit)
* **Base de Datos:** PostgreSQL con extensión PostGIS
* **Backend & Análisis:** R (`httr2`, `DBI`, `sf`, `gstat`, `tidymodels`, `xgboost`)
* **Visualización:** `shiny`, `leaflet`, `plotly`, `ggplot2`
* **Automatización:** `cron` (recolección de datos), GitHub Actions (CI/CD)
* **Acceso Remoto:** Cloudflare Tunnel

![Diagrama de Arquitectura](docs/architecture.png) ## 3. Estructura del Repositorio

```
├── R/
│   ├── 01_collect_data.R       # Script de recolección de datos (API -> DB)
│   └── 02_train_model.R        # Script de entrenamiento del modelo
├── shiny_app/
│   ├── app.R                   # Lógica de la aplicación Shiny
│   └── data/                   # Datos estáticos para la app (ej. geojson de distritos)
├── output/
│   ├── models/                 # Modelos entrenados (.rds) - No versionado
│   └── predictions/            # Rejillas de predicción - No versionado
├── .github/
│   └── workflows/
│       └── deploy.yml          # Workflow de CI/CD para la app Shiny
├── .gitignore
└── README.md

```


## 4. Puesta en Marcha

### Prerrequisitos

* Raspberry Pi 5 configurada según la guía.
* R y las librerías necesarias instaladas.
* Base de datos PostgreSQL/PostGIS creada y accesible.

### Pasos

1.  **Clonar el repositorio:**
    ```bash
    git clone [https://github.com/tu_usuario/tu_repositorio.git](https://github.com/tu_usuario/tu_repositorio.git)
    cd tu_repositorio
    ```

2.  **Configurar credenciales:** Crear un fichero `.Renviron` en la raíz del proyecto para almacenar las credenciales de la base de datos de forma segura.
    ```
    DB_USER="air_quality_user"
    DB_PASSWORD="tu_contraseña_muy_segura"
    DB_HOST="localhost"
    DB_PORT="5432"
    DB_NAME="air_quality_db"
    ```

3.  **Ejecutar el pipeline:**
    * Para la recolección inicial de datos: `Rscript R/01_collect_data.R`
    * Para entrenar el modelo: `Rscript R/02_train_model.R`

4.  **Lanzar la aplicación Shiny:**
    Abrir R y ejecutar:
    ```R
    shiny::runApp('shiny_app/')
    ```
