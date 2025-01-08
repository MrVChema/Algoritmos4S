rm(list = ls())

# ---- Instalar y cargar paquetes necesarios ----
library(readxl)
library(dplyr)
library(writexl)

# ---- Establecer directorio de trabajo ----
setwd('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[02] BASE DE DATOS')

# ---- Definir la carpeta donde están los archivos ----
carpeta <- '/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[02] EQUIPAMIENTO EDUCATIVO/[02] BASE DE DATOS/PROYECTOS REDI'

# Listar todos los archivos de Excel en la carpeta
archivos <- list.files(carpeta, pattern = "\\.xlsx$", full.names = TRUE)

# Función para cargar cada archivo, estandarizar columnas e incluir el segmento de mercado
cargar_archivo <- function(archivo) {
  # Leer el archivo
  datos <- read_excel(archivo)
  
  # Convertir todas las columnas a carácter (opcional: puedes ajustar esto si prefieres otro tipo)
  datos <- datos %>%
    mutate(across(everything(), as.character))
  
  # Extraer el segmento de mercado del nombre del archivo
  segmento <- strsplit(basename(archivo), "_")[[1]][1]
  
  # Agregar una columna con el segmento
  datos <- datos %>%
    mutate(Segmento = segmento)
  
  return(datos)
}

# Aplicar la función a todos los archivos y combinarlos en una sola tabla
tabla_combinada <- archivos %>%
  lapply(cargar_archivo) %>%
  bind_rows()

# Convertir columnas específicas al tipo numérico donde aplica
tabla_combinada <- tabla_combinada %>%
  mutate(
    `Meses de Inventario` = as.numeric(`Meses de Inventario`),
    `Meses en el Mercado` = as.numeric(`Meses en el Mercado`),
    `Unidades Totales` = as.numeric(`Unidades Totales`),
    `Unidades Inventario` = as.numeric(`Unidades Inventario`),
    `Precio Promedio Inventario` = as.numeric(`Precio Promedio Inventario`),
    `$M2 promedio inventario` = as.numeric(`$M2 promedio inventario`),
    `M2 Prom Inv` = as.numeric(`M2 Prom Inv`),
    Latitud = as.numeric(Latitud),
    Longitud = as.numeric(Longitud)
  )

# Verificar la estructura de los datos para confirmar cambios
str(tabla_combinada)

# Renombrar variables
tabla_combinada <- tabla_combinada %>%
  rename(meses_inv = `Meses de Inventario`,
         meses_mercado = `Meses en el Mercado`,
         unidades_totales = `Unidades Totales`,
         unidades_inv = `Unidades Inventario`,
         ppromedio_inv = `Precio Promedio Inventario`,
         p_m2 = `$M2 promedio inventario`,
         m2_inv = `M2 Prom Inv`)

# Eliminar filas con NA
tabla_combinada <- tabla_combinada %>%
  filter(!is.na(p_m2))

# Ver los primeros registros de la tabla combinada
head(tabla_combinada)

# Exportar a Excel
write_xlsx(tabla_combinada, "Proyectos Integrados_ZMM.xlsx")
