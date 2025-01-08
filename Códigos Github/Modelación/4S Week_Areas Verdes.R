rm(list = ls())

library(dplyr)
library(tidyverse)
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


# Datos Mac
proyectos <- read_xlsx('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[01] ÁREAS VERDES/VIVARIA/BASES DE DATOS/FINAL/Proyectos.xlsx')
areas_verdes <- read_xlsx('/Users/yalta/Library/Mobile Documents/com~apple~CloudDocs/4S Real Estate/2024/[01] PROYECTOS/[00] ALGORITMO/[02] AVANCES/[01] ÁREAS VERDES/VIVARIA/BASES DE DATOS/FINAL/AreasVerdes_Zona_V2.xlsx')

# Datos Windows
#proyectos <- read_xlsx(r"(C:\Users\yalta\iCloudDrive\4S Real Estate\2024\[01] PROYECTOS\[00] ALGORITMO\[02] AVANCES\[01] ÁREAS VERDES\VIVARIA\BASES DE DATOS\FINAL\Proyectos.xlsx)")
#areas_verdes <- read_xlsx(r"(C:\Users\yalta\iCloudDrive\4S Real Estate\2024\[01] PROYECTOS\[00] ALGORITMO\[02] AVANCES\[01] ÁREAS VERDES\VIVARIA\BASES DE DATOS\FINAL\AreasVerdes_Zona.xlsx)")

# Realizar la unión por la columna "Proyecto"
datos <- areas_verdes %>%
  left_join(proyectos, by = "Proyecto")

# Validación de Datos
## Verificar valores faltantes
colSums(is.na(datos))
# Eliminar filas con valores faltantes
datos <- datos %>% drop_na()

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
    `$M2 promedio inventario` = as.numeric(`$M2 promedio inventario`),
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  )

## Cambiar nombre de Variable $M2 promedio inventario
datos <- datos %>%
  rename(PM2_Promedio = `$M2 promedio inventario`, 
         Absorcion = `Absorcion por proyecto`)

# Análisis Exploratorio
## Histograma del precio por m2
ggplot(datos, aes(x = PM2_Promedio)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Distribución del Precio por m2", x = "Precio por m2", y = "Frecuencia")

## Gráfico de dispersión
ggplot(datos, aes(x = distancia, y = PM2_Promedio)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre Distancia y Precio por m2", x = "Distancia (km)", y = "Precio por m2")

## Boxplot del precio por m2 según el tipo de área verde
ggplot(datos, aes(x = loc_tipo, y = PM2_Promedio)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Precio por m2 según el Tipo de Área Verde", x = "Tipo de Área Verde", y = "Precio por m2")

## Boxplot del precio por m2 según el segmento
ggplot(datos, aes(x = Segmento, y = PM2_Promedio)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Precio por m2 según Segmento", x = "Segmento", y = "Precio por m2")

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

## Aplicar logaritmo al precio por m2
datos <- datos %>%
  mutate(
    log_precio_m2 = log(PM2_Promedio)
  )

## Listado de Variables y Datos
str(datos)

## Resumen Estadístico de los datos
summary(datos)

## Desviación Estándar de las variables numéricas
sapply(datos[, sapply(datos, is.numeric)], sd, na.rm = TRUE)

## Información General de la Base de Datos
dim(datos)
nrow(datos)
ncol(datos)

## Matriz de correlación entre variables numéricas
datos_num <- datos[, sapply(datos, is.numeric)]
matriz_corr <- cor(datos_num, use = "complete.obs")
print(matriz_corr)
corrplot(matriz_corr, method = "circle")

## Muestreo de datos
head(datos)
tail(datos)

### Muestreo Aleatorio de Datos
set.seed(123)
muestra <- datos[sample(1:nrow(datos), 10), ]
print(muestra)

# Histograma de Precio M2
hist(datos$log_precio_m2, main = "Distribución de log_precio_m2", xlab = "log_precio_m2")

# Histograma de Precio por Segmento
ggplot(datos, aes(x = log_precio_m2, fill = Segmento)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  labs(title = "Distribución de log_precio_m2 por Segmento",
       x = "log_precio_m2",
       y = "Frecuencia") +
  theme_minimal()

# Filtrar datos del segmento Premium
datos_premium <- subset(datos, Segmento == "Premium")

# Generar histograma sin plotear
hist_premium <- hist(datos_premium$log_precio_m2, breaks = 30, plot = FALSE)

# Crear data frame con los datos del histograma
hist_data <- data.frame(
  lower_bound = hist_premium$breaks[-length(hist_premium$breaks)],
  upper_bound = hist_premium$breaks[-1],
  mids = hist_premium$mids,
  counts = hist_premium$counts,
  density = hist_premium$density
)

# Mostrar los datos del histograma
print(hist_data)

# Calcular estadísticos descriptivos
library(e1071) # Si no tiene esta librería, instálela con install.packages("e1071")
mean_value <- mean(datos_premium$log_precio_m2)
median_value <- median(datos_premium$log_precio_m2)
sd_value <- sd(datos_premium$log_precio_m2)
skewness_value <- skewness(datos_premium$log_precio_m2)
kurtosis_value <- kurtosis(datos_premium$log_precio_m2)

cat("Media:", mean_value, "\n")
cat("Mediana:", median_value, "\n")
cat("Desviación Estándar:", sd_value, "\n")
cat("Asimetría:", skewness_value, "\n")
cat("Curtosis:", kurtosis_value, "\n")

# Prueba de Shapiro-Wilk
shapiro_result <- shapiro.test(datos_premium$log_precio_m2)
print(shapiro_result)

# Análisis de mixturas gaussianas
mix_model <- Mclust(datos_premium$log_precio_m2)
summary(mix_model)
plot(mix_model, what = "classification")

# Boxplot de log_precio_m2 por zona
ggplot(datos_premium, aes(x = nombre_zona, y = log_precio_m2)) +
  geom_boxplot() +
  labs(title = "Distribución de log_precio_m2 por Zona en el Segmento Premium",
       x = "Zona",
       y = "log_precio_m2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualización con Densidad Kernel
ggplot(datos_premium, aes(x = log_precio_m2, color = nombre_zona)) +
  geom_density() +
  labs(title = "Densidad de log_precio_m2 por Zona en el Segmento Premium",
       x = "log_precio_m2",
       y = "Densidad") +
  theme_minimal()

# Histograma Facetado por Zona
ggplot(datos_premium, aes(x = log_precio_m2)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  facet_wrap(~ nombre_zona, scales = "free_y") +
  labs(title = "Histograma de log_precio_m2 por Zona en el Segmento Premium",
       x = "log_precio_m2",
       y = "Frecuencia") +
  theme_minimal()

# Interacción de Dos Vías Segmento X Distancia
# Modelo con interacciones de dos vías
modelo_interaccion_dosvias_segdis <- lm(log_precio_m2 ~ distancia_parque * Segmento, data = datos)
summary(modelo_interaccion_dosvias_segdis)
vif(modelo_interaccion_dosvias_segdis, type = "predictor")


# Construcción de Calificación 
# Usando el modelo de interacción de dos vías
datos$predicted_log_precio_m2 <- predict(modelo_interaccion_dosvias_segdis, newdata = datos)

#Calcular diferencia entre valor real y predicho
datos$residuals <- datos$log_precio_m2 - datos$predicted_log_precio_m2

# Estandarizar Residuales
# Calcular el residual estandarizado por segmento
datos <- datos %>%
  group_by(Segmento) %>%
  mutate(
    residuals_z = scale(residuals)
  ) %>%
  ungroup()

# Asignar calificaciones basadas en los Z scores
datos <- datos %>%
  mutate(
    calificacion = case_when(
      residuals_z > 1 ~ "Sobrevalorado",
      residuals_z > 0 & residuals_z <= 1 ~ "Ligeramente Sobrevalorado",
      residuals_z > -1 & residuals_z <= 0 ~ "Ligeramente Subvalorado",
      residuals_z <= -1 ~ "Subvalorado"
    )
  )

# Gráfico de Distribución de Residuales
# Nombre del proyecto del cliente
nombre_proyecto_cliente <- "Parc 302"

# Obtener el segmento del proyecto del cliente
segmento_cliente <- datos$Segmento[datos$Proyecto == nombre_proyecto_cliente][1]

# Extraer el residual estandarizado del proyecto del cliente
residuals_z_cliente <- datos$residuals_z[datos$Proyecto == nombre_proyecto_cliente][1]


ggplot(datos %>% filter(Segmento == segmento_cliente), aes(x = residuals_z)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = residuals_z_cliente, color = "red", linetype = "dashed") +
  labs(title = paste("Posicionamiento del Proyecto en el Segmento", segmento_cliente),
       x = "Residual Estandarizado (Z-Score)",
       y = "Densidad") +
  theme_minimal()


# Calcular el percentil del proyecto dentro de su segmento
datos <- datos %>%
  group_by(Segmento) %>%
  mutate(
    percentil = percent_rank(residuals)
  ) %>%
  ungroup()

# Obtener el percentil del proyecto del cliente
percentil_cliente <- datos %>%
  filter(Proyecto == nombre_proyecto_cliente) %>%
  dplyr::select(percentil) %>%
  pull()

# Crear un data frame para el termómetro
termometro_data <- data.frame(
  rango = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
  valor = c(0.2, 0.2, 0.2, 0.2, 0.2)
)

# Asignar colores
termometro_data$color <- c("red", "orange", "yellow", "lightgreen", "green")

# Percentil único por proyecto
percentil_cliente <- datos %>%
  filter(Proyecto == nombre_proyecto_cliente) %>%
  summarise(percentil = mean(percentil)) %>%
  pull()


# Gráfico del termómetro
ggplot(termometro_data, aes(x = 1, y = valor, fill = factor(rango, levels = rev(rango)))) +
  geom_bar(stat = "identity", width = 0.2, color = "black") +
  coord_flip() +
  scale_fill_manual(values = rev(termometro_data$color)) +
  geom_text(aes(label = rango), position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = percentil_cliente, color = "blue", linetype = "dashed", linewidth = 1.5) +
  labs(title = paste("Posicionamiento del Proyecto", nombre_proyecto_cliente),
       x = "",
       y = "Percentil") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")
# Evaluación del modelo
# Residuales vs. Valores Ajustados
plot(modelo_interaccion_dosvias_segdis$fitted.values, modelo_interaccion_dosvias_segdis$residuals)
abline(h = 0, col = "red")

# Histograma de Residuales
hist(modelo_interaccion_dosvias_segdis$residuals, main = "Histograma de Residuales", xlab = "Residuales")

# Q-Q Plot de Residuales
qqnorm(modelo_interaccion_dosvias_segdis$residuals)
qqline(modelo_interaccion_dosvias_segdis$residuals, col = "red")

# Pruebas Estadísticas
# Normalidad de residuales
# shapiro.test(modelo_interaccion_dosvias_segdis$residuals)
# Breusch-Pagan (Homocedasticidad)
bptest(modelo_interaccion_dosvias_segdis)
# Multicolinealidad
vif(modelo_interaccion_dosvias_segdis)

# Modelo Robusto
  ## Modelo de regresión robusta con interacción de dos vías
  modelo_robusto <- rlm(log_precio_m2 ~ distancia_parque * Segmento, data = datos)
  # Resumen del modelo robusto
  summary(modelo_robusto)
  # Modelo clásico
  modelo_clasico <- lm(log_precio_m2 ~ distancia_parque * Segmento, data = datos)
  # Resumen del modelo clásico
  summary(modelo_clasico)
  
  
  # Residuales del modelo robusto
  residuos_robustos <- modelo_robusto$residuals
  # Histograma de los residuales
  hist(residuos_robustos, breaks = 50, main = "Histograma de Residuales (Modelo Robusto)", xlab = "Residuales")
  # Q-Q Plot de los residuales
  qqnorm(residuos_robustos)
  qqline(residuos_robustos, col = "red")
  
  
  # Valores ajustados
  ajustados_clasico <- modelo_clasico$fitted.values
  ajustados_robusto <- modelo_robusto$fitted.values
  # Gráfico de comparación
  plot(ajustados_clasico, ajustados_robusto, main = "Comparación de Valores Ajustados", xlab = "Modelo Clásico", ylab = "Modelo Robusto")
  abline(a = 0, b = 1, col = "red")
  
  
  # Predicciones del modelo robusto
  datos$predicted_log_precio_m2_robusto <- predict(modelo_robusto, newdata = datos)
  # Calcular los residuales robustos
  datos$residuals_robustos <- datos$log_precio_m2 - datos$predicted_log_precio_m2_robusto
  # Estandarizar los residuales por segmento
  datos <- datos %>%
    group_by(Segmento) %>%
    mutate(
      residuals_z_robustos = scale(residuals_robustos)
    ) %>%
    ungroup()
 
  
   # Asignar calificaciones basadas en los residuales robustos estandarizados
  datos <- datos %>%
    mutate(
      calificacion_robusta = case_when(
        residuals_z_robustos > 1 ~ "Sobrevalorado",
        residuals_z_robustos > 0 & residuals_z_robustos <= 1 ~ "Ligeramente Sobrevalorado",
        residuals_z_robustos > -1 & residuals_z_robustos <= 0 ~ "Ligeramente Subvalorado",
        residuals_z_robustos <= -1 ~ "Subvalorado"
      )
    )
  # Calcular el percentil del proyecto dentro de su segmento usando residuales robustos
  datos <- datos %>%
    group_by(Segmento) %>%
    mutate(
      percentil_robusto = percent_rank(residuals_robustos)
    ) %>%
    ungroup()
  
  # Obtener el percentil del proyecto del cliente
  percentil_cliente_robusto <- datos %>%
    filter(Proyecto == nombre_proyecto_cliente) %>%
    summarise(percentil = mean(percentil_robusto)) %>%
    pull()
  
  # Generar el gráfico del termómetro con los datos robustos
  ggplot(termometro_data, aes(x = 1, y = valor, fill = factor(rango, levels = rev(rango)))) +
    geom_bar(stat = "identity", width = 0.2, color = "black") +
    coord_flip() +
    scale_fill_manual(values = rev(termometro_data$color)) +
    geom_text(aes(label = rango), position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = percentil_cliente_robusto, color = "blue", linetype = "dashed", linewidth = 1.5) +
    annotate(
      "text",
      x = 1.1,
      y = percentil_cliente_robusto,
      label = nombre_proyecto_cliente,
      color = "blue",
      hjust = 0,
      vjust = -0.5,
      size = 4
    ) +
    labs(
      title = paste("Posicionamiento del Proyecto", nombre_proyecto_cliente, " (Modelo Robusto)"),
      x = "",
      y = "Percentil"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
  
  
  
  # Gráfico de residuos vs valores ajustados
  plot(modelo_robusto$fitted.values, modelo_robusto$residuals)
  abline(h = 0, col = "red")
  
  
  # Estimar la varianza de los residuos
  library(lmtest)
  library(sandwich)
  
  # Ajustar el modelo clásico
  modelo_clasico <- lm(log_precio_m2 ~ distancia_parque * Segmento, data = datos)
  
  # Estimar los pesos (inversos de los residuos al cuadrado)
  pesos <- 1 / (abs(modelo_clasico$residuals) + 1e-6)
  
  # Ajustar el modelo WLS
  modelo_wls <- lm(log_precio_m2 ~ distancia_parque * Segmento, data = datos, weights = pesos)
  
  # Resumen del modelo WLS
  summary(modelo_wls)



# Dividir los datos por segmento
  # Obtener segmentos únicos
  segmentos <- unique(datos$Segmento)
  
  # Inicializar una lista para almacenar los modelos
  modelos_por_segmento <- list()
  
  for (segmento in segmentos) {
    # Filtrar datos para el segmento
    datos_segmento <- datos %>% filter(Segmento == segmento)
    
    # Verificar tamaño de muestra
    if (nrow(datos_segmento) >= 30) {  # Ajusta el umbral según sea apropiado
      # Ajustar el modelo
      modelo_segmento <- lm(log_precio_m2 ~ distancia, data = datos_segmento)
      
      # Almacenar el modelo
      modelos_por_segmento[[segmento]] <- modelo_segmento
      
      # Imprimir resumen
      cat("Resumen del Modelo para el Segmento:", segmento, "\n")
      print(summary(modelo_segmento))
      cat("\n")
    } else {
      cat("El segmento", segmento, "tiene datos insuficientes para un modelado fiable.\n")
    }
  }


# Redes Neuronales
  # Ajustar el modelo de red neuronal
  modelo_nn <- nnet(log_precio_m2 ~ distancia_parque + distancia_jardin + distancia_ecologico + distancia_perros + distancia_senderismo + nombre_zona, data = datos, size = 5)
  # Predecir y evaluar
  predicciones <- predict(modelo_nn, datos, type = "raw")
  # Calcular el MSE y RMSE
  mse_nn <- mean((datos$log_precio_m2 - predicciones)^2)
  rmse_nn <- sqrt(mse_nn)
  cat("RMSE del modelo de Red Neuronal:", rmse_nn, "\n")
  
  
# Random Forest
  # Ajustar el modelo de Random Forest
  modelo_rf <- randomForest(log_precio_m2 ~ distancia_parque + distancia_jardin + distancia_ecologico + distancia_perros + distancia_senderismo + nombre_zona, data = datos)
  # Ver variables importantes
  importancia <- importance(modelo_rf)
  varImpPlot(modelo_rf)
  pdp_parque <- partial(modelo_rf, pred.var = "distancia_parque")
  plot(pdp_parque)
  print(importancia)
  head(pdp_parque)
  predicciones_rf <- predict(modelo_rf, datos)
  mse_rf <- mean((datos$log_precio_m2 - predicciones_rf)^2)
  rmse_rf <- sqrt(mse_rf)
  cat("RMSE del modelo Random Forest:", rmse_rf, "\n")
  
# Gradient Boost Machines (GBM)
  # Ajustar el modelo GBM
  modelo_gbm <- gbm(log_precio_m2 ~ distancia_parque + distancia_jardin + distancia_ecologico + distancia_perros + distancia_senderismo + nombre_zona, data = datos, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)
  # Importancia de variables
  resumen_importancia <- summary(modelo_gbm)
  print(resumen_importancia)
  predicciones_gbm <- predict(modelo_gbm, datos, n.trees = 1000)
  mse_gbm <- mean((datos$log_precio_m2 - predicciones_gbm)^2)
  rmse_gbm <- sqrt(mse_gbm)
  cat("RMSE del modelo GBM:", rmse_gbm, "\n")
  
  
# Support Vector Machines
  # Ajustar el modelo SVM
  modelo_svm <- svm(log_precio_m2 ~ distancia_parque + distancia_jardin + distancia_ecologico + distancia_perros + distancia_senderismo + nombre_zona, data = datos)
  # Predecir y evaluar
  predicciones <- predict(modelo_svm, datos)
  mse_svm <- mean((datos$log_precio_m2 - predicciones)^2)
  rmse_svm <- sqrt(mse_svm)
  cat("RMSE del modelo SVM:", rmse_svm, "\n")
  
# Comparación de desempeño de modelos de ML
  rmse_table <- data.frame(
    Modelo = c("Red Neuronal", "Random Forest", "GBM", "SVM"),
    RMSE = c(rmse_nn, rmse_rf, rmse_gbm, rmse_svm)
  )
  print(rmse_table)
  
# SHAP Values
  predictor_rf <- Predictor$new(modelo_rf, data = datos, y = "log_precio_m2")
  shapley_rf <- Shapley$new(predictor_rf, x.interest = datos[1, ])
  plot(shapley_rf)
  
# Generar Predicciones y Residuales por GBM
  # Realizar predicciones con el modelo GBM
  predicciones_gbm <- predict(modelo_gbm, datos, n.trees = 1000)
  # Calcular los residuales
  datos$residuals_gbm <- datos$log_precio_m2 - predicciones_gbm
  # Estandarizar los residuales por zona o segmento (si corresponde)
  datos <- datos %>%
    group_by(nombre_zona) %>%
    mutate(
      residuals_z_gbm = scale(residuals_gbm)
    ) %>%
    ungroup()
  
# Generación de Calificación  
  # Asignar calificaciones basadas en los residuales
  datos <- datos %>%
    mutate(
      calificacion_gbm = case_when(
        residuals_z_gbm > 1 ~ "Sobrevalorado",
        residuals_z_gbm > 0 & residuals_z_gbm <= 1 ~ "Ligeramente Sobrevalorado",
        residuals_z_gbm > -1 & residuals_z_gbm <= 0 ~ "Ligeramente Subvalorado",
        residuals_z_gbm <= -1 ~ "Subvalorado"
      )
    )
  
  # Nombre del proyecto del cliente
  nombre_proyecto_cliente <- "El Árbol"
  
  # Calcular el percentil dentro de su zona
  datos <- datos %>%
    group_by(nombre_zona) %>%
    mutate(
      percentil_gbm = percent_rank(residuals_gbm)
    ) %>%
    ungroup()
  
  # Obtener el percentil del proyecto del cliente
  percentil_cliente_gbm <- datos %>%
    filter(Proyecto == nombre_proyecto_cliente) %>%
    summarise(percentil = mean(percentil_gbm)) %>%
    pull()
  
  # Crear un data frame para el termómetro
  termometro_data <- data.frame(
    rango = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
    valor = c(0.2, 0.2, 0.2, 0.2, 0.2)
  )
  
  # Asignar colores
  termometro_data$color <- c("red", "orange", "yellow", "lightgreen", "green")
  
  # Generar el gráfico del termómetro con los datos GBM
  ggplot(termometro_data, aes(x = 1, y = valor, fill = factor(rango, levels = rev(rango)))) +
    geom_bar(stat = "identity", width = 0.2, color = "black") +
    coord_flip() +
    scale_fill_manual(values = rev(termometro_data$color)) +
    geom_text(aes(label = rango), position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = percentil_cliente_gbm, color = "blue", linetype = "dashed", linewidth = 1.5) +
    annotate(
      "text",
      x = 1.1,
      y = percentil_cliente_gbm,
      label = nombre_proyecto_cliente,
      color = "blue",
      hjust = 0,
      vjust = -0.5,
      size = 4
    ) +
    labs(
      title = paste("Posicionamiento del Proyecto", nombre_proyecto_cliente, " (Modelo GBM)"),
      x = "",
      y = "Percentil"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  
# Termómetro comparativo
  # Nombres de los proyectos a comparar
  nombres_proyectos_clientes <- c("El Árbol", "Ventum Torre 1")
  
  # Calcular el percentil dentro de su zona
  datos <- datos %>%
    group_by(nombre_zona) %>%
    mutate(
      percentil_gbm = percent_rank(residuals_gbm)
    ) %>%
    ungroup()
  
  # Obtener los percentiles de los proyectos seleccionados
  percentiles_clientes_gbm <- datos %>%
    filter(Proyecto %in% nombres_proyectos_clientes) %>%
    group_by(Proyecto) %>%
    summarise(percentil = mean(percentil_gbm))
  
  # Crear un data frame para el termómetro
  termometro_data <- data.frame(
    rango = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
    valor = c(0.2, 0.2, 0.2, 0.2, 0.2),
    ymin = c(0, 0.2, 0.4, 0.6, 0.8),
    ymax = c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  
  # Asignar colores
  termometro_data$color <- c("red", "orange", "yellow", "lightgreen", "green")
  colores_proyectos <- setNames(
    c("mediumblue", "grey13"),
    nombres_proyectos_clientes
  )
  
  # Generar el gráfico del termómetro con los datos GBM
   ggplot() +
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
    scale_color_manual(values = colores_proyectos) +
    coord_flip() +
    geom_text(
      data = termometro_data,
      aes(x = 1, y = (ymin + ymax) / 2, label = rango)
    ) +
    # Agregar líneas horizontales y etiquetas para cada proyecto
    geom_hline(
      data = percentiles_clientes_gbm,
      aes(yintercept = percentil, color = Proyecto),
      linetype = "dashed",
      size = 1
    ) +
    geom_text(
      data = percentiles_clientes_gbm,
      aes(x = 1.1, y = percentil, label = Proyecto, color = Proyecto),
      hjust = 0,
      vjust = -0.5,
      size = 4
    ) +
    labs(
      title = paste("Valoración de Proyectos: ", nombres_proyectos_clientes[1], "vs", nombres_proyectos_clientes[2]),
      x = "",
      y = "Percentil"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    guides(fill = "none", color = "none")
  