rm(list = ls())

# ---- Instalar y cargar paquetes necesarios ----
library(sf)
library(lwgeom)
library(readxl)
library(writexl)
library(leaflet)

# ---- Establecer directorio de trabajo ----
setwd('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[02] EQUIPAMIENTO')
getwd()

# ---- Cargar y convertir información ----
data <- read_xlsx('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[02] EQUIPAMIENTO/Equipamiento_Integrado.xlsx')
zonas <- st_read('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[02] KML_KMZ/[03] ZONAS REDI/REDI - Zonas 2024-06-25.kml')

# Convertir data a objetos espaciales (invertir el orden de las coordenadas)
data_sf <- st_as_sf(data, coords = c("longitud", "latitud"), crs = 4326)

# ---- Corrección de Geometrías ----
# Remover geometrías vacías
zonas <- zonas[!st_is_empty(zonas), ]

# Remover Coordenadas Z y M
zonas <- st_zm(zonas, drop = TRUE, what = "ZM")

# Validar y corregir geometrías
zonas <- st_make_valid(zonas)

# Desactivar S2
sf_use_s2(FALSE)

# ---- Intersección de la información ----

# Definir el bounding box para Monterrey
monterrey_bbox <- st_as_sfc(st_bbox(c(xmin = -100.6, xmax = -99.8, ymin = 25.5, ymax = 26), crs = st_crs(4326)))

# Filtrar zonas dentro del bounding box
zonas <- st_intersection(zonas, monterrey_bbox)

# Realizar la intersección espacial en CRS geográfico
data_zonas <- st_join(data_sf, zonas)

# Eliminar columna "Description"
data_zonas <- data_zonas %>%
  dplyr::select(-Description)

# Ajustar nombre de variables
data_zonas <- data_zonas %>%
  rename("Variable" = "Fuente",
         "Zona_Redi" = "Name")

# Eliminar filas con NA en Zona_Redi
data_zonas <- data_zonas %>%
  filter(!is.na(Zona_Redi))

# ---- Visualización de información ----
# Define un vector con los colores que quieres usar
colores_personalizados <- c("Areas_Verdes" = "green", 
                            "Transporte_Publico" = "red" 
                            )

# Crea la paleta de colores con los colores personalizados
paleta_colores <- colorFactor(palette = colores_personalizados, 
                              domain = data_zonas$Fuente)

# Visualizar en Leaflet
mapa <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = zonas,
              color = "blue",
              weight = 1,
              fillOpacity = 0.2,
              popup = ~Name) %>%
  addCircleMarkers(data = data_zonas,
                   radius = 5,
                   color =  ~paleta_colores(Variable),
                   fillOpacity = 0.7,
                   popup = ~nombre)

# Mostrar el mapa
mapa

# ---- Preparación para exportar a Excel ---- 
# Extraer coordenadas
coords <- st_coordinates(data_zonas)

# Agregar las coordenadas como nuevas columnas
data_zonas$longitud <- coords[, 1]
data_zonas$latitud <- coords[, 2]

# Eliminar la columna 'geometry'
data_zonas$geometry <- NULL

# Exportar a Excel
write_xlsx(data_zonas, "Equipamiento_Google X Zonas - ZMM.xlsx")
write.csv(data_zonas, "Equipamiento_Google X Zonas - ZMM.csv")
