#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec  2 14:52:59 2024

@author: yalta
"""

import os
import requests
import pandas as pd
import time
from math import radians, cos, sin, asin, sqrt
import re  # Para sanitizar el project_name

# 1. Lectura del archivo Excel con las coordenadas
coordinates_file = '/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[02] EQUIPAMIENTO/Epicentros ZMM.xlsx' # Reemplaza con la ruta de tu archivo Excel
coordinates_df = pd.read_excel(coordinates_file)

# 2. Configuración inicial
url = "https://local-business-data.p.rapidapi.com/search-nearby"
output_file = '/Users/yalta/Library/CloudStorage/GoogleDrive-yaltalielt@gmail.com/Mi unidad/4S Real Estate/2025/[02] ALGORITMO/[02] DATOS/[03] VARIABLES/[01] AREAS VERDES/AreasVerdes_ZMM.xlsx'  # Ruta y nombre del archivo Excel de salida
giros = ['park', 'garden', 'community garden']
region = 'mx'

# Asegurarse de que la carpeta de salida existe
output_folder = os.path.dirname(output_file)
if not os.path.exists(output_folder):
    os.makedirs(output_folder)

# 3. Inicializar una lista para almacenar los DataFrames de cada zona
all_projects_data = []

# 4. Iteración sobre cada zona en el archivo Excel
for idx, row in coordinates_df.iterrows():
    latPredio = row['Latitud']
    lngPredio = row['Longitud']
    project_name = row['Nombre']  # Usamos 'Nombre' como el nombre de la zona

    # Sanitizar el project_name para evitar problemas
    project_name_clean = re.sub(r'[\\/*?:"<>|]', '', project_name)

    # Inicialización de estructuras de datos para cada zona
    bizz_data = {
        'business_id': [],
        'nombre': [],
        'loc-direccion': [],
        'URL': [],
        'rating': [],
        'loc-tipo': [],
        'sub-tipos': [],
        'lat': [],
        'lng': [],
        'project_name': []  # Agregamos el nombre de la zona aquí
    }

    # Coordenadas del punto central
    lat = latPredio
    lng = lngPredio

    # Iteración sobre cada giro
    for giro in giros:
        querystring = {
            "query": giro,
            "limit": "100",
            "lat": f"{lat}",
            "lng": f"{lng}",
            "radius": "3000",  # Radio en metros (3 km)
            "language": "es",
            "region": f"{region}",
            "extract_emails_and_contacts": "false"
        }

        headers = {
            "x-rapidapi-key": "9298958da1msh6cb856993710795p1bdbb2jsnc49127d97639",  # Reemplaza con tu clave de API
            "x-rapidapi-host": "local-business-data.p.rapidapi.com"
        }

        # Realiza la solicitud a la API
        response = requests.get(url, headers=headers, params=querystring)
        dataset = response.json()
        data_list = dataset.get('data', [])

        # Procesa los datos recibidos
        for data in data_list:
            bizz_data['business_id'].append(data.get('business_id', 'N/A'))
            bizz_data['nombre'].append(data.get('name', 'N/A'))
            bizz_data['loc-direccion'].append(data.get('address', 'N/A'))
            bizz_data['URL'].append(data.get('website', 'N/A'))
            bizz_data['rating'].append(data.get('rating', 'N/A'))
            bizz_data['loc-tipo'].append(data.get('type', 'N/A'))
            bizz_data['sub-tipos'].append(data.get('subtypes', 'N/A'))
            bizz_data['lat'].append(data.get('latitude', 'N/A'))
            bizz_data['lng'].append(data.get('longitude', 'N/A'))
            bizz_data['project_name'].append(project_name_clean)  # Agregamos el nombre de la zona

        print(f"Búsqueda para '{giro}' completada para la zona '{project_name}'.")
        time.sleep(1)  # Pausa para no exceder el límite de solicitudes

    # Creación del DataFrame para la zona actual
    df = pd.DataFrame(bizz_data)
    df = df.drop_duplicates(subset=['business_id'], keep='first')

    # Limpieza de datos
    df = df[df['lat'] != 'N/A']
    df = df[df['lng'] != 'N/A']
    df['lat'] = df['lat'].astype(float)
    df['lng'] = df['lng'].astype(float)

    # Cálculo de distancias usando Haversine
    def haversine(lat1, lon1, lat2, lon2):
        # Convierte grados a radianes
        lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
        # Fórmula de Haversine
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
        c = 2 * asin(sqrt(a))
        r = 6371  # Radio de la Tierra en kilómetros
        return c * r

    df['distancia'] = df.apply(lambda row: haversine(latPredio, lngPredio, row['lat'], row['lng']), axis=1)

    # Filtrado por distancia (3 km)
    df = df[df['distancia'] <= 3].reset_index(drop=True)

    # Agregar el DataFrame de la zona actual a la lista
    if not df.empty:
        all_projects_data.append(df)
        print(f"Datos agregados para la zona '{project_name}'.")
    else:
        print(f"No se encontraron datos para la zona '{project_name}' después del filtrado.")

# 5. Concatenar todos los DataFrames en uno solo
if all_projects_data:
    final_df = pd.concat(all_projects_data, ignore_index=True)

    # Exportar el DataFrame final a un archivo Excel
    final_df.to_excel(output_file, index=False, sheet_name='Áreas Verdes')
    print(f"Todos los datos han sido exportados a Excel en {output_file}")
else:
    print("No hay datos para exportar.")

print("Todos los procesos han sido completados exitosamente.")
