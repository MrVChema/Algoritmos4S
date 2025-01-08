# Instalar Paqueterías
# install.packages("dplyr")
# install.packages("openxlsx")

# Limpiar Workspace
rm(list = ls())

# Establecer directorio de trabajo
setwd('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[02] BASE DE DATOS')
getwd()

# Cargar librería necesaria
library(dplyr)
library(openxlsx)

# Cargar la base de datos
df <- read.csv('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[02] BASE DE DATOS/denue_19_csv/conjunto_de_datos/denue_inegi_19_.csv', encoding = "latin1")

# Imprimir los nombres de las columnas para verificarlos
print(names(df))

# Ver las opciones únicas en la columna de personal ocupado (per_ocu)
personal_ocupado_unico <- unique(df$per_ocu)
print(personal_ocupado_unico)

# Remover espacios extra de los valores de la columna 'per_ocu' y verificar nuevamente
df$PER_OCU <- trimws(df$per_ocu)
personal_ocupado_unico <- unique(df$per_ocu)
print(personal_ocupado_unico)

# Definir las palabras clave para buscar actividades económicas relacionadas
palabras_clave <- c("escuelas")

# Buscar actividades económicas que contengan alguna de las palabras clave
actividades_relacionadas <- df %>%
  filter(grepl(paste(palabras_clave, collapse = "|"), nombre_act, ignore.case = TRUE)) %>%
  select(nombre_act) %>%
  distinct()

# Mostrar las actividades económicas encontradas
print(actividades_relacionadas)

# Ejecutar la función de ejemplo (ajusta el nombre del municipio y las actividades)
municipios_seleccionados <- c("Monterrey")
actividades_seleccionadas <- c("Escuelas de educación primaria del sector privado", 
                               "Escuelas del sector privado que combinan diversos niveles de educación", 
                               "Escuelas de educación media superior del sector privado", 
                               "Escuelas de educación secundaria general del sector privado", 
                               "Escuelas de educación media técnica terminal del sector privado", 
                               "Escuelas de educación primaria del sector público", 
                               "Escuelas de educación preescolar del sector privado", 
                               "Escuelas de educación preescolar del sector público", 
                               "Escuelas de educación media superior del sector público", 
                               "Escuelas de educación secundaria general del sector público", 
                               "Escuelas del sector público que combinan diversos niveles de educación", 
                               "Escuelas de educación secundaria técnica del sector privado", 
                               "Escuelas del sector privado de educación para necesidades especiales", 
                               "Escuelas del sector público de educación para necesidades especiales", 
                               "Escuelas de educación media técnica terminal del sector público", 
                               "Escuelas de educación secundaria técnica del sector público") # Coloca las actividades que desees filtrar

# Función para filtrar por municipios, actividades y opcionalmente por uno o más rangos de personal ocupado
filtrar_datos <- function(municipios_seleccionados, actividades_seleccionadas, filtrar_personal = FALSE, personal_ocupado = NULL) {
  
  # Filtrar por municipios y actividades
  df_filtrado <- df %>%
    filter(municipio %in% municipios_seleccionados & nombre_act %in% actividades_seleccionadas)
  
  # Aplicar el filtro por personal ocupado solo si filtrar_personal es TRUE
  if (filtrar_personal) {
    df_filtrado <- df_filtrado %>%
      filter(per_ocu %in% personal_ocupado)  # Filtrar por múltiples rangos
  }
  
  # Exportar a CSV
  write.csv(df_filtrado, "Escuelas_Monterrey.csv", row.names = FALSE)
  
  # Exportar a Excel
  write.xlsx(df_filtrado, "Escuelas_Monterrey.xlsx", rowNames = FALSE)
}

# Uso:

# Ejemplo de uso:
# Filtrar solo por municipio y actividades
filtrar_datos(municipios_seleccionados, actividades_seleccionadas)
