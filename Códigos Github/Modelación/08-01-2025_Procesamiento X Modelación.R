# --- Limpiar entorno ----
rm(list = ls())

# --- Cargar Librerías ----
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggmap)
library(car)
library(sf)
library(readxl)
library(geosphere)
library(randomForest)
library(spdep)
library(spatialreg)
library(corrplot)
library(mclust)
library(MASS)
library(interactions)
library(RColorBrewer)
library(viridis)
library(lmtest)
library(sandwich)
library(nnet)
library(gbm)
library(e1071)
library(pdp)
library(iml)
library(caret)

# --- Cargar Datos ----
# Datos Mac
proyectos <- read_csv('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[01] PROYECTOS/[01] VIVIENDA VERTICAL/Proyectos Integrados_ZMM.csv')
areas_verdes <- read_xlsx('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[01] AREAS VERDES/AreasVerdes_Zona_V2.xlsx')

# Datos Windows
#proyectos <- read_xlsx(r"(C:\Users\yalta\iCloudDrive\4S Real Estate\2024\[01] PROYECTOS\[00] ALGORITMO\[02] AVANCES\[01] ÁREAS VERDES\VIVARIA\BASES DE DATOS\FINAL\Proyectos.xlsx)")
#areas_verdes <- read_xlsx(r"(C:\Users\yalta\iCloudDrive\4S Real Estate\2024\[01] PROYECTOS\[00] ALGORITMO\[02] AVANCES\[01] ÁREAS VERDES\VIVARIA\BASES DE DATOS\FINAL\AreasVerdes_Zona.xlsx)")

## Polígonos NSE
# 1. Listar capas (carpetas) en el KML
kml_layers <- st_layers('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[02] KML_KMZ/[01] NSE/NSE-Monterrey.kml')
print(kml_layers)

# 2. Leer cada capa y guardarlas en una lista
kml_data_list <- lapply(kml_layers$name, function(x) {
  st_read('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[02] KML_KMZ/[01] NSE/NSE-Monterrey.kml', layer = x)
})

# 3. Combinar todas las capas en un único objeto sf
kml_data_all <- do.call(rbind, kml_data_list)

# --- Procesamiento de la Información ----
# Realizar la unión por la columna "Proyecto"
datos <- areas_verdes %>%
  left_join(proyectos, by = "Proyecto")

# Validación de Datos
## Verificar valores faltantes
colSums(is.na(datos))
## Eliminar filas con valores faltantes
datos <- datos %>% drop_na()

# Estandarización de la información
## Convertir variables categóricas a factores
datos <- datos %>%
  mutate(
    loc_tipo = as.factor(loc_tipo),
    Segmento = as.factor(Segmento),
    Municipio = as.factor(Municipio),
    nombre_zona = as.factor(nombre_zona)
  )

## Convertir variables a numéricas si es necesario
datos <- datos %>%
  mutate(
    distancia = as.numeric(distancia),
    `p_m2` = as.numeric(`p_m2`),
    `Absorcion por proyecto` = as.numeric(`Absorcion por proyecto`),
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  )

## Cambiar nombre de Variable $M2 promedio inventario
datos <- datos %>%
  rename(PM2_Promedio = `p_m2`, 
         Absorcion = `Absorcion por proyecto`)

# Creación de Variables Dummy
## Crear variables dummy
datos <- datos %>%
  mutate(
    es_parque = if_else(loc_tipo == "Parque", 1, 0),
    es_jardin = if_else(loc_tipo == "Jardín", 1, 0),
    es_ecologico = if_else(loc_tipo == "Parques ecológicos y reservas", 1, 0),
    es_perros = if_else(loc_tipo == "Parque para perros", 1, 0),
    es_senderismo = if_else(loc_tipo == "Zona de senderismo", 1, 0),
    es_nacional = if_else(loc_tipo == "Parque nacional", 1, 0)
  )

## Interacción entre distancia y tipo de área verde
datos <- datos %>%
  mutate(
    distancia_parque = distancia * es_parque,
    distancia_jardin = distancia * es_jardin,
    distancia_ecologico = distancia * es_ecologico,
    distancia_perros = distancia * es_perros,
    distancia_senderismo = distancia * es_senderismo,
    distancia_nacional = distancia * es_nacional
  )

# Aplicar logaritmo al precio por m2
datos <- datos %>%
  mutate(
    log_precio_m2 = log(PM2_Promedio)
  )

# Creación de variable "Unidades Vendidas"
datos$unidades_vendidas <- datos$unidades_totales - datos$unidades_inv

# Procesamiento de información geográfica
## Convertir datos a objeto sf (geom: POINT)
datos <- st_as_sf(
  datos,
  coords = c("longitud", "latitud"),
  crs = 4326   # 4326 = WGS84
)

## Separar información de la Variable Description del archivo kml
## Definir el patrón de regex
regex_pat <- paste0(
  "NSE:\\s*([^ ]+)",
  "\\s+Personas:\\s*(\\d+)",
  "\\s+Hogares:\\s*(\\d+)",
  "\\s+Precio Promedio de Vivienda:\\s*\\$([\\d,\\.]+)"
)

## Extraer la info
match_info <- str_match(kml_data_all$Description, regex_pat)

## Crear columnas nuevas
kml_data_all <- kml_data_all %>%
  mutate(
    NSE       = match_info[, 2],
    Personas  = as.numeric(match_info[, 3]),
    Hogares   = as.numeric(match_info[, 4]),
    Precio_Prom_Vivienda_txt = match_info[, 5]
  ) %>%
  ## Limpiar el precio y convertir a número
  mutate(
    Precio_Prom_Vivienda = as.numeric(str_replace_all(Precio_Prom_Vivienda_txt, ",", ""))
  )

## Convertir NSE a factor (opcional)
kml_data_all$NSE <- factor(
  kml_data_all$NSE,
  levels = c("A+", "A", "B", "C+", "C", "D+", "D/E", "TNH")
)

# Realizar la unión espacial
datos <- st_join(
  datos, 
  kml_data_all,
  join = st_intersects
)

# --- Modelación ----
## Modelo de Regresión Lineal Simple
modelo_regsim <- lm(log_precio_m2 ~ distancia_parque + Absorcion + m2_inv + Segmento + unidades_vendidas + NSE + Precio_Prom_Vivienda, data = datos)
summary(modelo_regsim)

## Gradient Boost Machines (GBM)
### Ajustar el modelo GBM
modelo_gbm <- gbm(log_precio_m2 ~ distancia_parque + Absorcion + m2_inv + Segmento + unidades_vendidas + NSE + Precio_Prom_Vivienda, data = datos, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)
### Importancia de variables
resumen_importancia <- summary(modelo_gbm)
print(resumen_importancia)
predicciones_gbm <- predict(modelo_gbm, datos, n.trees = 1000)
mse_gbm <- mean((datos$log_precio_m2 - predicciones_gbm)^2)
rmse_gbm <- sqrt(mse_gbm)
cat("RMSE del modelo GBM:", rmse_gbm, "\n")

# --- Predicciones ----
## Crear un nuevo data frame con una observación
nueva_obs <- data.frame(
  distancia_parque   = 350,          # metros a un parque
  Absorcion          = 15,           # unidades vendidas por mes
  m2_inv             = 800,          # m² de inventario
  Segmento           = "Premium Plus",  # Segmento de clase
  unidades_vendidas  = 80,           # unidades vendidas
  NSE                = "A+",          # Nivel socioeconómico
  Precio_Prom_Vivienda = 55500000     # Precio promedio de vivienda en la zona
)

## Convertir Segmento a factor con los mismos niveles que en los datos de entrenamiento
nueva_obs$Segmento <- factor(nueva_obs$Segmento, levels = levels(datos$Segmento))

## Predicción en logaritmo del precio por m2
prediccion_log <- predict(modelo_gbm, newdata = nueva_obs, n.trees = 1000)
prediccion_log

## Convertir la predicción logarítmica a precio por m2
prediccion_m2 <- exp(prediccion_log)
prediccion_m2

## Predicción en logaritmo con el modelo de regresión lineal
prediccion_log_rl <- predict(modelo_regsim, newdata = nueva_obs)

## Convertir a escala real
prediccion_m2_rl <- exp(prediccion_log_rl)
prediccion_m2_rl

# --- Termómetro Calificador Original ----
## 1. Obtener predicciones
predicciones_gbm_termometro <- predict(modelo_gbm, datos, n.trees = 1000)

## 2. Cálculo de residuales
datos$residuals_gbm_termometro <- datos$log_precio_m2 - predicciones_gbm_termometro

## 3. Cálculo de percentiles en cada observación
datos <- datos %>%
  group_by(nombre_zona) %>%
  mutate(
    percentil_gbm_termometro = percent_rank(residuals_gbm_termometro)
  ) %>%
  ungroup()

## 4. Selección de proyectos a comparar
nombres_proyectos_clientes <- c("El Árbol", "Ventum Torre 1")

## 5. Filtrar el data frame y calcular un promedio del percentil
percentiles_clientes_gbm_termometro <- datos %>%
  filter(Proyecto %in% nombres_proyectos_clientes) %>%
  group_by(Proyecto) %>%
  summarise(percentil = mean(percentil_gbm_termometro))

## 6. Creación de un data frame con los rangos a graficar
termometro_data <- data.frame(
  rango = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
  valor = c(0.2, 0.2, 0.2, 0.2, 0.2),
  ymin = c(0, 0.2, 0.4, 0.6, 0.8),
  ymax = c(0.2, 0.4, 0.6, 0.8, 1.0)
)

## 7. Asignar colores al termómetro
termometro_data$color <- c("red", "orange", "yellow", "lightgreen", "green")

## 8. Definir colores para los proyectos seleccionados
colores_proyectos <- setNames(
  c("mediumblue", "grey13"),  # tantos colores como proyectos
  nombres_proyectos_clientes
)

## 9. Graficar el termómetro original
ggplot() +
  ### Rectángulos de colores para el termómetro
  geom_rect(
    data = termometro_data,
    aes(
      xmin = 0.8,
      xmax = 1.2,
      ymin = ymin,
      ymax = ymax,
      fill = factor(rango, levels = rango)
    ),
    color = "black"
  ) +
  scale_fill_manual(values = termometro_data$color) +
  ### Eje “horizontal” girado
  coord_flip() +
  ### Agregar la etiqueta al interior de cada rectángulo
  geom_text(
    data = termometro_data,
    aes(x = 1, y = (ymin + ymax) / 2, label = rango)
  ) +
  ### Agregar las líneas para cada proyecto
  geom_hline(
    data = percentiles_clientes_gbm_termometro,
    aes(yintercept = percentil, color = Proyecto),
    linetype = "dashed",
    linewidth = 1
  ) +
  ### Agregar el texto (nombre del proyecto) cerca de la línea
  geom_text(
    data = percentiles_clientes_gbm_termometro,
    aes(x = 1.1, y = percentil, label = Proyecto, color = Proyecto),
    hjust = 0,
    vjust = -0.5,
    size = 4
  ) +
  ### Escala de color para proyectos
  scale_color_manual(values = colores_proyectos) +
  labs(
    title = paste(
      "Valoración de Proyectos:",
      nombres_proyectos_clientes[1],
      "vs",
      nombres_proyectos_clientes[2]
    ),
    x = "",
    y = "Percentil (modelo GBM)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

# --- Termómetro Calificador con Nueva Observación ----
## 1. Agregar la nueva observación al conjunto de datos
nueva_obs <- nueva_obs %>%
  mutate(
    log_precio_m2 = prediccion_log,  # Agregar la predicción logarítmica
    PM2_Promedio = prediccion_m2,    # Agregar la predicción en escala real
    Proyecto = "Nuevo Proyecto",     # Asignar un nombre a la nueva observación
    residuals_gbm_termometro = 0,    # Asumimos residual = 0 (no hay valor real)
    nombre_zona = "Valle" # Asignar un valor de nombre_zona (ajusta según tus datos)
  )

# Combinar la nueva observación con el conjunto de datos original
datos_con_nueva_obs <- bind_rows(datos, nueva_obs)

# Calcular percentiles para todas las observaciones (incluyendo la nueva)
datos_con_nueva_obs <- datos_con_nueva_obs %>%
  group_by(nombre_zona) %>%
  mutate(
    percentil_gbm_termometro = percent_rank(residuals_gbm_termometro)
  ) %>%
  ungroup()

## 2. Selección de proyectos a comparar (incluyendo la nueva observación)
nombres_proyectos_clientes_nuevo <- c("El Árbol", "Ventum Torre 1", "Nuevo Proyecto")

## 3. Filtrar el data frame y calcular un promedio del percentil
percentiles_clientes_gbm_termometro_nuevo <- datos_con_nueva_obs %>%
  filter(Proyecto %in% nombres_proyectos_clientes_nuevo) %>%
  group_by(Proyecto) %>%
  summarise(percentil = mean(percentil_gbm_termometro))

## 4. Creación de un data frame con los rangos a graficar
termometro_data_nuevo <- data.frame(
  rango = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
  valor = c(0.2, 0.2, 0.2, 0.2, 0.2),
  ymin = c(0, 0.2, 0.4, 0.6, 0.8),
  ymax = c(0.2, 0.4, 0.6, 0.8, 1.0)
)

## 5. Asignar colores al termómetro
termometro_data_nuevo$color <- c("red", "orange", "yellow", "lightgreen", "green")

## 6. Definir colores para los proyectos seleccionados
colores_proyectos_nuevo <- setNames(
  c("mediumblue", "grey13", "purple"),  # Colores para los proyectos
  nombres_proyectos_clientes_nuevo
)

## 7. Graficar el nuevo termómetro
ggplot() +
  ### Rectángulos de colores para el termómetro
  geom_rect(
    data = termometro_data_nuevo,
    aes(
      xmin = 0.8,
      xmax = 1.2,
      ymin = ymin,
      ymax = ymax,
      fill = factor(rango, levels = rango)
    ),
    color = "black"
  ) +
  scale_fill_manual(values = termometro_data_nuevo$color) +
  ### Eje “horizontal” girado
  coord_flip() +
  ### Agregar la etiqueta al interior de cada rectángulo
  geom_text(
    data = termometro_data_nuevo,
    aes(x = 1, y = (ymin + ymax) / 2, label = rango)
  ) +
  ### Agregar las líneas para cada proyecto
  geom_hline(
    data = percentiles_clientes_gbm_termometro_nuevo,
    aes(yintercept = percentil, color = Proyecto),
    linetype = "dashed",
    linewidth = 1
  ) +
  ### Agregar el texto (nombre del proyecto) cerca de la línea
  geom_text(
    data = percentiles_clientes_gbm_termometro_nuevo,
    aes(x = 1.1, y = percentil, label = Proyecto, color = Proyecto),
    hjust = 0,
    vjust = -0.5,
    size = 4
  ) +
  ### Escala de color para proyectos
  scale_color_manual(values = colores_proyectos_nuevo) +
  labs(
    title = paste(
      "Valoración de Proyectos:",
      paste(nombres_proyectos_clientes_nuevo, collapse = " vs ")
    ),
    x = "",
    y = "Percentil (modelo GBM)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill = "none", color = "none")
