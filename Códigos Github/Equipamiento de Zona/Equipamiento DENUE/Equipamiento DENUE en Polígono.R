# Instalar Paqueterías
# install.packages("readr")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("writexl")

# Limpiar Workspace
rm(list = ls())

# Establecer directorio de trabajo
setwd('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[03] RESULTADOS')
getwd()

# Cargar librería necesaria
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(writexl)

# Cargar polígono de Zona de Influencia KML
polygon <- st_read('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[00] POLÍGONOS/3kms_Estanzuela.kml')

# Cargar los datos del DENUE
datos_municipio <- read.csv('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[02] BASE DE DATOS/Escuelas_Monterrey.csv')

# Convertir coordenadas en objetos espaciales
datos_municipio_sf <- st_as_sf(datos_municipio, coords = c("longitud", "latitud"), crs = 4326)

# Verificar el CRS del polígono
st_crs(polygon)

# Verificar el CRS de los datos de puntos
st_crs(datos_municipio_sf)

# Visualización de los datos y el polígono
ggplot() +
  geom_sf(data = polygon, fill = NA, color = "red") +
  geom_sf(data = datos_municipio_sf, color = "blue") +
  theme_minimal()

# Centro del radio para convertir ruta circular en polígono
centro <- st_centroid(polygon)

# Obtener las coordenadas del centro
coords_centro <- st_coordinates(centro)
print(coords_centro)

# Crear el punto central como objeto 'sf'
centro <- st_sfc(st_point(c(coords_centro[1], coords_centro[2])), crs = 4326)

# Transformar a un CRS proyectado adecuado (por ejemplo, UTM zona 14N)
crs_proyectado <- 32614  # EPSG para UTM zona 14N

# Transformar el centro al CRS proyectado
centro_proj <- st_transform(centro, crs = crs_proyectado)

# Crear el polígono circular con un radio de 3000 metros (3 km)
poligono_circular_proj <- st_buffer(centro_proj, dist = 3000)

# Volver al CRS geográfico
poligono_circular <- st_transform(poligono_circular_proj, crs = 4326)

# Realizar la intersección
datos_poligono <- st_intersection(datos_municipio_sf, poligono_circular)

# Verificar el número de puntos dentro del polígono
cat("Número de puntos dentro del polígono:", nrow(datos_poligono), "\n")

# **Visualización con leaflet**
# Verificar que todos los objetos están en CRS 4326
st_crs(poligono_circular)
st_crs(datos_municipio_sf)
st_crs(datos_poligono)

# Crear el mapa
mapa <- leaflet() %>%
  # Centrar el mapa en el centro del polígono
  setView(lng = coords_centro[1], lat = coords_centro[2], zoom = 13) %>%
  
  # Añadir un mapa base
  addTiles(group = "Mapa Base") %>%
  
  # Añadir capas base adicionales si lo deseas
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  
  # Añadir el polígono circular
  addPolygons(
    data = poligono_circular,
    fillColor = "red",
    fillOpacity = 0.2,
    color = "red",
    weight = 2,
    group = "Área de Influencia"
  ) %>%
  
  # Añadir los puntos dentro del polígono
  addCircleMarkers(
    data = datos_poligono,
    radius = 5,
    color = "blue",
    fillColor = "blue",
    fillOpacity = 0.8,
    popup = ~nom_estab,  # Reemplaza 'nombre' con el nombre del campo que quieras mostrar
    group = "Puntos Dentro"
  ) %>%
  
  # Añadir controles para capas
  addLayersControl(
    baseGroups = c("Mapa Base", "Satélite"),
    overlayGroups = c("Área de Influencia", "Puntos Dentro"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Añadir una leyenda
  addLegend(
    position = "bottomright",
    colors = c("blue"),
    labels = c("Puntos Dentro"),
    title = "Leyenda",
    opacity = 1
  ) %>%
  
  # Añadir escala
  addScaleBar(position = "bottomleft")

# Mostrar el mapa
mapa

# Extraer coordenadas de latitud y longitud
coords <- st_coordinates(datos_poligono)
datos_poligono$latitud <- coords[, 2]
datos_poligono$longitud <- coords[, 1]

# Eliminar la columna geometry
datos_poligono_df <- st_drop_geometry(datos_poligono)

# Exportar a CSV
write.csv(datos_poligono_df, "Escuelas.csv", row.names = FALSE)

# Exportar a Excel
write_xlsx(datos_poligono_df, "Escuelas.xlsx")

# Exportar a KML
datos_poligono$Name <- ""
st_write(datos_poligono, "Escuelas.kml", delete_dsn = TRUE)


# Fin del código
