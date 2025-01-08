rm(list = ls())

# ---- Instalar y cargar paquetes necesarios ----
library(readxl)
library(writexl)

# ---- Establecer directorio de trabajo ----
setwd('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[03] HOSPITALES/[02] BASE DE DATOS')
getwd()

# ---- Cargar base de datos ----
data <- read_excel('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[03] HOSPITALES/[02] BASE DE DATOS/Hospitales_ZMM.xlsx')

# ---- Procesar Data ----
  # Ajustar nombres
  colnames(data)
  
  data <- data %>%
    rename(loc_direccion = `loc-direccion`,
           loc_tipo = `loc-tipo`,
           sub_tipos = `sub-tipos`,
           latitud = lat,
           longitud = lng)
  
  # Limpiar base
  unique(data$loc_tipo)
  
  deseadas <- c("Hospital general", "Hospital", "Hospital especializado", 
                "Centro médico", "Hospital privado", "Hospital psiquiátrico",
                "Hospital infantil", "Servicio de emergencias", "Hospital de maternidad",
                "Hospital universitario", "Hospital cardiovascular", "Hospital militar",
                "Clínica ambulatoria", "Hospital gubernamental", "Unidad hospitalaria")
  
  data <- data %>%
    filter(loc_tipo %in% deseadas) %>%
    distinct(business_id, .keep_all = TRUE) %>%
    dplyr::select(-distancia)

# ---- Exportar a XLSX ----
write_xlsx(data, '/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[03] HOSPITALES/[02] BASE DE DATOS/Hospitales_ZMM_Clean.xlsx')
