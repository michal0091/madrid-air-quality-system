# Dockerfile para Madrid Air Quality System
# Imagen optimizada con R 4.5.1 y todas las dependencias pre-instaladas

FROM rocker/r-ver:4.5.1

# Metadatos
LABEL maintainer="Madrid Air Quality System"
LABEL description="Docker image con R 4.5.1 y XGBoost para predicciones de calidad del aire"
LABEL version="2.0.0"

# Variables de entorno
ENV DEBIAN_FRONTEND=noninteractive
ENV RENV_CONFIG_AUTOLOADER_ENABLED=FALSE

# Instalar dependencias del sistema (igual que en workflow)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libarchive-dev \
    cmake \
    pandoc \
    gdal-bin \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Crear directorio de trabajo
WORKDIR /app

# Copiar solo los archivos necesarios para instalar paquetes primero (cache layer)
COPY install_packages.R /tmp/install_packages.R

# Instalar paquetes R (esto se cachea si no cambian)
RUN Rscript /tmp/install_packages.R

# Verificar instalación de paquetes críticos (XGBoost version)
RUN Rscript -e "if (!requireNamespace('xgboost', quietly = TRUE)) stop('xgboost no instalado'); if (!requireNamespace('data.table', quietly = TRUE)) stop('data.table no instalado'); if (!requireNamespace('sf', quietly = TRUE)) stop('sf no instalado'); if (!requireNamespace('logger', quietly = TRUE)) stop('logger no instalado'); cat('✅ Paquetes críticos verificados (XGBoost)\n')"

# Mensaje de éxito
RUN echo "✅ Docker image construida exitosamente con R 4.5.1 y todas las dependencias"

# Comando por defecto (puede ser sobrescrito)
CMD ["R"]
