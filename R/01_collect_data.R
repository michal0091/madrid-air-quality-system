# FASE 1: SCRIPT DE RECOLECCIÓN Y ALMACENAMIENTO DE DATOS
# Objetivo: Obtener datos en formato ANCHO de la API, transformarlos a formato LARGO y guardarlos en PostGIS.

# 1. CARGA DE LIBRERÍAS ----
renv::load()

library(httr2)
library(jsonlite)
library(data.table)
library(lubridate)   
library(sf)
library(DBI)
library(RPostgres)
library(logger)