rm(list = ls())

# ---- Instalar y cargar paquetes necesarios ----
library(readxl)
library(writexl)

# ---- Establecer directorio de trabajo ----
setwd('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[02] EQUIPAMIENTO/ACCESO TRANSPORTE')
getwd()

# ---- Cargar base de datos ----
data <- read_excel('/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[01] AREAS VERDES/AreasVerdes_ZMM.xlsx')

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
write_xlsx(data, '/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[01] AREAS VERDES/AreasVerdes_ZMM_Clean.xlsx')
