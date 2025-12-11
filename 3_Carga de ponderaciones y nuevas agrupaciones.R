

              ## CARGA DE PONDERACIONES Y MODIFICACIÓN DE GRUPOS PARA UNIFICAR CON IPC ##
              ## CARGA DE PONDERACIONES Y MODIFICACIÓN DE GRUPOS PARA UNIFICAR CON IPC ##
              ## CARGA DE PONDERACIONES Y MODIFICACIÓN DE GRUPOS PARA UNIFICAR CON IPC ##
              ## CARGA DE PONDERACIONES Y MODIFICACIÓN DE GRUPOS PARA UNIFICAR CON IPC ##


# En este script, se cargan los datos de las ponderaciones para terminar de prepararlos. Dado que a lo largo de los años analizados
# cambian ligeramente las agrupaciones de subgrupos, es necesario uniformarlas todas y hacer que coincidan con los subgrupos del IPC 
# tal y como los proporciona actualmente el INE. En concreto, en los años en los que aparecen, "081 Servicios postales" lo uno a "083 Servicios de telefonía y fax", 
# y "092 Otros grandes bienes duraderos para ocio y cultura", lo sumo a "093 Otros artículos y equipos para ocio, jardinería y animales domésticos". Hago comprobaciones al final
# para comprobar que todo suma 1000. Guardo de nuevo las ponderaciones con el "apellido" de "definitivas".


# CARGA LIBRERIAS
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(writexl)

# AÑOS A OBTENER (SOLO TOCAR ESTO)
anios <- 2019:2025

anios_ponderaciones <- (min(anios) - 2):(max(anios) - 1) # Para los años que quiero obtener, necesito años previos de ponderaciones
anios_ponderaciones_IPC <- (min(anios) - 1):(max(anios)) # Para los años que quiero obtener, necesito años previos de ponderaciones


# DIRECTORIO
# Directorio base
base_dir <- getwd()

# Resto rutas
ruta_ponderaciones_nuevas <- file.path(base_dir, "DATOS/PONDERACIONES/IPC_INQUILINOS")
ruta_ponderaciones_nuevas_CCAA <- file.path(base_dir, "DATOS/PONDERACIONES/IPC_INQUILINOS_CCAA")
ruta_ponderaciones_definitivas <- file.path(base_dir, "DATOS/PONDERACIONES/PONDERACIONES_DEFINITIVAS")

# Crear carpeta si no existe (en las que se guardarán los nuevos pesos del alquiler)
if (!dir.exists(ruta_ponderaciones_definitivas)) {
  dir.create(ruta_ponderaciones_definitivas, recursive = TRUE)
}





###### ESTATAL #####
# Una vez reponderados todos los subgrupos, y guardados en un archivo por cada año, cargo todos juntos en una unica tabla
ponderaciones_IPC <- file.path(
  ruta_ponderaciones_nuevas,
  paste0("alquiler_ajustado_ponderaciones_IPC_", anios_ponderaciones_IPC, ".xlsx")
)

# Funcion para leer y unir mis excels de ponderaciones
unir_ponderaciones <- function(){
  # Crear un dataframe vacío para almacenar los resultados
  datos_combinados <- data.frame()
  # Bucle para leer y combinar cada archivo
  for (archivo in ponderaciones_IPC) {
    # Extraer el año del nombre del archivo (ej: "2007" de "sin_alquiler_ponderaciones_IPC_2007.xlsx")
    anio <- gsub(".*_(\\d{4})\\.xlsx", "\\1", archivo)  # Usamos expresiones regulares
    # Leer el archivo y seleccionar columnas relevantes
    datos <- read_xlsx(archivo) %>%
      select(Componente = `...1`, Ponderacion_nueva) %>%  # Asumimos que `...1` es el nombre de componente
      mutate(anio = anio)  # Añadir columna con el año
    # Combinar con el dataframe principal
    datos_combinados <- rbind(datos_combinados, datos)
  }
  # Pivotar para tener años como columnas
  datos_finales <- datos_combinados %>%
    pivot_wider(
      names_from = anio,
      values_from = Ponderacion_nueva,
      names_prefix = "ponderacion_"
    )
  
  return(datos_finales)
}

# Llamar a la función
df_ponderaciones_unificado <- unir_ponderaciones()

# Ahora tengo que corregir disonancias entre los componentes de las ponderaciones y los datos que tengo. En concreto, hay dos componentes de ponderaciones
# ...que no aparecen en los datos: 081 (servicios postales) y 092 (Otros grandes bienes duraderos para ocio y cultura). 
# Como son componentes que me dan igual, meros componentes para el calculo, los voy a agregar sus ponderaciones a las de otros componentes similares

# Corrijo 081
df_ponderaciones_unificado <- df_ponderaciones_unificado %>%
  mutate(across( # Combino mutate y across para seleccionar multiples columnas
    starts_with("ponderacion_"), # En este caso todas las que empiezan con "ponderacion_"
    ~ ifelse(Componente == "083 Servicios de telefonía y fax", # Si la observacion tiene el valor "083..." en la columna "Componente!"
             . + replace_na(.[Componente == "081 Servicios postales"], 0),# Al valor de la columna actual se le suma el valor en la misma columna de la observacion "081..."
             # Y en caso de que sea NA, lo considera "0"
             .) # Si no, se mantiene igual
  )) %>%
  # Filtrar para eliminar la fila 081
  filter(Componente != "081 Servicios postales") %>%
  # Reordenar las filas si es necesario
  arrange(Componente)

# Corrijo 092
df_ponderaciones_unificado <- df_ponderaciones_unificado %>%
  mutate(across( # Combino mutate y across para seleccionar multiples columnas
    starts_with("ponderacion_"), # En este caso todas las que empiezan con "ponderacion_"
    ~ ifelse(Componente == "093 Otros artículos y equipos para ocio, jardinería y animales domésticos", # Si la observacion tiene el valor "093..." en la columna "Componente!"
             . + replace_na(.[Componente == "092 Otros grandes bienes duraderos para ocio y cultura"], 0), # Al valor de la columna actual se le suma el valor en la misma columna de la observacion "092..."
             .) # Si no, se mantiene igual
  )) %>%
  # Filtrar para eliminar la fila 092
  filter(Componente != "092 Otros grandes bienes duraderos para ocio y cultura") %>%
  # Reordenar las filas si es necesario
  arrange(Componente)

# Comprobación de que las ponderaciones estan bien (todas suman 1000)

# Paso a formato largo, que ademas luego me vendra bien
ponderaciones_largo <- df_ponderaciones_unificado %>%
  pivot_longer(cols = starts_with("ponderacion_"), 
               names_to = "anio_prov", 
               values_to = "ponderacion")

# Limpio el nombre del año 
ponderaciones_largo <- ponderaciones_largo %>%
  mutate(anio = as.numeric(str_remove(anio_prov, "ponderacion_"))) %>% # Limpio de string
  select(-anio_prov) # elimino columna sobrante

# Verificar que las sumas son correctas
ipc_comprobacion <- ponderaciones_largo %>%
  mutate(año_num = anio) %>%
  group_by(año_num) %>%
  summarise(
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar la comprobación
print(ipc_comprobacion)

# Guardar archivos
saveRDS(ponderaciones_largo,
        file = file.path(ruta_ponderaciones_definitivas, "ponderaciones_estatales_definitivas.rds"))



###### CCAA ######

# Cargar de nuevo las ponderaciones y unirlas en un único DF
# Hacer lista con archivos
# Lo que cargo aquí son ya las ponderaciones que voy a utilizar, en este caso parten de los datos oficiales de IPC,
# reponderados con el peso del alquiler a partir de la EPF
ponderaciones_IPC_CCAA <- file.path(
  ruta_ponderaciones_nuevas_CCAA,
  paste0("alquiler_ajustado_ponderaciones_IPC_CCAA_", anios_ponderaciones_IPC, ".xlsx")
)


# Funcion para leer y unir mis excels de ponderaciones
unir_ponderaciones_CCAA <- function(archivos){
  datos_combinados <- data.frame()
  
  for (archivo in archivos) {
    # Extraer el año del nombre del archivo
    anio <- gsub(".*_(\\d{4})\\.xlsx", "\\1", archivo)
    
    # Leer el excel
    datos <- read_xlsx(archivo)
    
    # Pasar de formato ancho (CCAA en columnas) a largo
    datos_largo <- datos %>%
      rename(Componente = `...1`) %>%   # renombrar primera columna
      pivot_longer(
        cols = -Componente,
        names_to = "CCAA",
        values_to = "Ponderacion"
      ) %>%
      mutate(anio = as.integer(anio))
    
    # Acumular resultados
    datos_combinados <- bind_rows(datos_combinados, datos_largo)
  }
  
  return(datos_combinados)
}

# Llamada a la función:
df_ponderaciones_CCAA <- unir_ponderaciones_CCAA(ponderaciones_IPC_CCAA)

# Ahora tengo que corregir disonancias entre los componentes de las ponderaciones y los datos que tengo. En concreto, hay dos componentes de ponderaciones
# ...que no aparecen en los datos: 081 (servicios postales) y 092 (Otros grandes bienes duraderos para ocio y cultura). 
# Como son componentes que me dan igual, meros componentes para el calculo, los voy a agregar sus ponderaciones a las de otros componentes similares

# Corrijo 081

df_ponderaciones_CCAA <- df_ponderaciones_CCAA %>%
  # Para años hasta 2021
  filter(anio <= 2021) %>%
  group_by(CCAA, anio) %>%
  mutate(
    # Obtener valor de 081 (0 si no existe o es NA)
    valor_081 = ifelse(any(Componente == "081 Servicios postales" & !is.na(Ponderacion)),
                       Ponderacion[Componente == "081 Servicios postales"],
                       0)
  ) %>%
  # Sumar a 083 y eliminar 081
  mutate(
    Ponderacion = ifelse(Componente == "083 Servicios de telefonía y fax",
                         Ponderacion + valor_081,
                         Ponderacion)
  ) %>%
  filter(Componente != "081 Servicios postales") %>%
  select(-valor_081) %>%
  ungroup() %>%
  # Añadir años posteriores sin cambios
  bind_rows(df_ponderaciones_CCAA %>% filter(anio > 2021)) %>%
  arrange(anio, CCAA, Componente)

# Verificación
sumas_correctas <- df_ponderaciones_CCAA %>%
  group_by(anio, CCAA) %>%
  summarise(suma = sum(Ponderacion, na.rm = TRUE), .groups = 'drop') %>%
  pull(suma) %>%
  all(. > 999.9 & . < 1000.1)  # Tolerancia de ±0.1

if (sumas_correctas) {
  message(" Todas las sumas son aproximadamente 1000")
} else {
  message(" Algunas sumas no son 1000 - Revisar:")
  # Mostrar grupos problemáticos
  df_ponderaciones_CCAA  %>%
    group_by(anio, CCAA) %>%
    summarise(suma = sum(Ponderacion, na.rm = TRUE), .groups = 'drop') %>%
    filter(suma < 999.9 | suma > 1000.1) %>%
    print()
}


# Corrijo 092 

df_ponderaciones_CCAA <- df_ponderaciones_CCAA %>%
  # Para años hasta 2021
  filter(anio <= 2021) %>%
  group_by(CCAA, anio) %>%
  mutate(
    # Obtener valor de 092 (0 si no existe o es NA)
    valor_092 = ifelse(any(Componente == "092 Otros grandes bienes duraderos para ocio y cultura" & !is.na(Ponderacion)),
                       Ponderacion[Componente == "092 Otros grandes bienes duraderos para ocio y cultura"],
                       0)
  ) %>%
  # Sumar a 093 y eliminar 092
  mutate(
    Ponderacion = ifelse(Componente == "093 Otros artículos y equipos para ocio, jardinería y animales domésticos",
                         Ponderacion + valor_092,
                         Ponderacion)
  ) %>%
  filter(Componente != "092 Otros grandes bienes duraderos para ocio y cultura") %>%
  select(-valor_092) %>%
  ungroup() %>%
  # Añadir años posteriores sin cambios
  bind_rows(df_ponderaciones_CCAA %>% filter(anio > 2021)) %>%
  arrange(anio, CCAA, Componente)

# Verificación
sumas_correctas <- df_ponderaciones_CCAA %>%
  group_by(anio, CCAA) %>%
  summarise(suma = sum(Ponderacion, na.rm = TRUE), .groups = 'drop') %>%
  pull(suma) %>%
  all(. > 999.9 & . < 1000.1)  # Tolerancia de ±0.1

if (sumas_correctas) {
  message(" Todas las sumas son aproximadamente 1000")
} else {
  message(" Algunas sumas no son 1000 - Revisar:")
  # Mostrar grupos problemáticos
  df_ponderaciones_CCAA  %>%
    group_by(anio, CCAA) %>%
    summarise(suma = sum(Ponderacion, na.rm = TRUE), .groups = 'drop') %>%
    filter(suma < 999.9 | suma > 1000.1) %>%
    print()
}

# Comprobación de que las ponderaciones están bien (todas suman 1000 por CCAA y año)
# Ya tenemos los datos en formato largo, así que no necesitamos pivot_longer

# Verificar que las sumas son correctas por CCAA y año
ipc_comprobacion <- df_ponderaciones_CCAA %>%
  group_by(CCAA, anio) %>%
  summarise(
    total = sum(Ponderacion, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar la comprobación
print(ipc_comprobacion)

# Guardar archivos
saveRDS(df_ponderaciones_CCAA,
        file = file.path(ruta_ponderaciones_definitivas, "ponderaciones_ccaa_definitivas.rds"))

