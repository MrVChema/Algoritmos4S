rm(list = ls())

# ---- Instalar y cargar paquetes necesarios ----
library(sf)
library(writexl)

# ---- Establecer directorio de trabajo ----
setwd('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[00] POLÍGONOS')
getwd()

# ---- Leer el archivo KML ----
kml_file <- '/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[00] POLÍGONOS/Epicentros ZMM.kml'
data <- st_read(kml_file)

# ---- Extraer el nombre, latitud y longitud ----
locations <- data.frame(
  Nombre = data$Name,  # Cambia este campo si el nombre tiene otro identificador
  Latitud = st_coordinates(data)[,2],  # Coordenadas Y (Latitud)
  Longitud = st_coordinates(data)[,1]  # Coordenadas X (Longitud)
)

# ---- Guardar el resultado en Excel ----
write_xlsx(locations, 'Epicentros ZMM.xlsx')

