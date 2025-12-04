


                ## CALCULO DE NUEVAS PONDERACIONES DE SUBGRUPOS A PARTIR DE NUEVOS PESOS DEL ALQUILER ##
                ## CALCULO DE NUEVAS PONDERACIONES DE SUBGRUPOS A PARTIR DE NUEVOS PESOS DEL ALQUILER ##
                ## CALCULO DE NUEVAS PONDERACIONES DE SUBGRUPOS A PARTIR DE NUEVOS PESOS DEL ALQUILER ##
                ## CALCULO DE NUEVAS PONDERACIONES DE SUBGRUPOS A PARTIR DE NUEVOS PESOS DEL ALQUILER ##


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
ruta_nuevos_pesos_alquiler <- file.path(base_dir, "DATOS/PONDERACIONES/PESOS_ALQUILER_EPF")
ruta_ponderaciones_originales <- file.path(base_dir, "DATOS/PONDERACIONES/SUBGRUPOS")
ruta_ponderaciones_originales_CCAA <- file.path(base_dir, "DATOS/PONDERACIONES/SUBGRUPOS_CCAA")
ruta_ponderaciones_nuevas <- file.path(base_dir, "DATOS/PONDERACIONES/IPC_INQUILINOS")
ruta_ponderaciones_nuevas_CCAA <- file.path(base_dir, "DATOS/PONDERACIONES/IPC_INQUILINOS_CCAA")

# Crear carpetas que no existan (solo las "nuevas", en las que se guardarán las nuevas ponderaciones)
if (!dir.exists(ruta_ponderaciones_nuevas)) {
  dir.create(ruta_ponderaciones_nuevas, recursive = TRUE)
}

if (!dir.exists(ruta_ponderaciones_nuevas_CCAA)) {
  dir.create(ruta_ponderaciones_nuevas_CCAA, recursive = TRUE)
}



####### REPONDERAR SUBGRUPOS IPC ESTATAL #######

# Cargo los datos de peso del alquiler del anterior script
nuevo_peso_alquiler <- fread(
  file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_estatal.csv")
)

# Funcion para reponderar
reponderar_con_alquiler_ajustado <- function(archivo_entrada, archivo_salida, nuevo_peso_alquiler) {
  # Leer archivo
  datos <- read_excel(archivo_entrada)
  
  # Identificar fila de alquiler
  fila_alquiler <- which(datos$`...1` %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  if (length(fila_alquiler) == 0) {
    warning("No se encontró el componente 'Alquiler de vivienda' en ", basename(archivo_entrada))
    return()
  }
  
  # Separar alquiler del resto
  datos_sin_alquiler <- datos %>%
    filter(!`...1` %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  datos_alquiler <- datos %>%
    filter(`...1` %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  # Calcular peso disponible para el resto (1000 - nuevo_peso_alquiler)
  peso_disponible_resto <- 1000 - nuevo_peso_alquiler
  
  # Calcular total original del resto (sin alquiler)
  total_resto_original <- sum(datos_sin_alquiler[[2]], na.rm = TRUE)
  
  # Reponderar el resto proporcionalmente
  datos_resto_reponderados <- datos_sin_alquiler %>%
    mutate(
      Ponderacion_nueva = (.[[2]] / total_resto_original) * peso_disponible_resto
    )
  
  # Actualizar peso del alquiler
  datos_alquiler_reponderados <- datos_alquiler %>%
    mutate(
      Ponderacion_nueva = nuevo_peso_alquiler
    )
  
  # Combinar todo
  datos_reponderados <- bind_rows(datos_resto_reponderados, datos_alquiler_reponderados)
  
  # Verificar que suma 1000
  suma_total <- sum(datos_reponderados$Ponderacion_nueva, na.rm = TRUE)
  message("Suma total para ", basename(archivo_entrada), ": ", round(suma_total, 2))
  
  # Guardar
  write_xlsx(datos_reponderados, archivo_salida)
}

# Lista de archivos a reponderar

archivos_originales <- file.path(
  ruta_ponderaciones_originales,
  paste0("ponderaciones_IPC_", anios_ponderaciones_IPC, ".xlsx")
)

# Función para obtener año del nombre del archivo
extraer_año <- function(nombre_archivo) {
  # Extraer año del nombre del archivo (ej: "ponderaciones_IPC_2015.xlsx" -> 2015)
  year_match <- regmatches(nombre_archivo, regexpr("\\d{4}", nombre_archivo))
  if (length(year_match) > 0) {
    return(as.numeric(year_match[1]))
  } else {
    return(NA)
  }
}

# Procesar cada archivo
for (archivo in archivos_originales) {
  if (file.exists(archivo)) {
    # Extraer año del archivo
    año <- extraer_año(basename(archivo))
    
    if (is.na(año)) {
      message("No se pudo extraer el año de: ", basename(archivo))
      next
    }
    
    # Buscar ponderación correspondiente al año
    peso_alquiler <- nuevo_peso_alquiler %>%
      filter(AÑO_PONDERACION == !!año) %>%
      pull(ESFUERZO) * 1000 # Columna que posee las ponderaciones. Multiplico para que esté en el mismo rango que el resto de podneraciones
    
    if (length(peso_alquiler) == 0) {
      message("No se encontró ponderación para el año ", año, " en ", basename(archivo))
      next
    }
    
    message("Procesando año ", año, " con peso de alquiler: ", peso_alquiler)
    
    nombre_salida <- file.path(ruta_ponderaciones_nuevas, paste0("alquiler_ajustado_", basename(archivo)))
    reponderar_con_alquiler_ajustado(archivo, nombre_salida, peso_alquiler)
    message("Procesado: ", basename(archivo))
  } else {
    message("Archivo no encontrado: ", basename(archivo))
  }
}



###################### Repetir procesos para CCAA #####

# Funcion para reponderar
peso_alquiler_ccaa <- fread(
  file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_ccaa.csv"))


# Función para reponderar por cada CCAA con pesos específicos por CCAA
reponderar_con_alquiler_ajustado_ccaa <- function(archivo_entrada, archivo_salida, año_ponderacion, peso_alquiler_ccaa) {
  # Leer archivo
  datos <- read_excel(archivo_entrada)
  
  # Identificar fila de alquiler
  fila_alquiler <- which(datos$...1 %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  if (length(fila_alquiler) == 0) {
    warning("No se encontró el componente 'Alquiler de vivienda' en ", basename(archivo_entrada))
    return()
  }
  
  # Separar alquiler del resto
  datos_sin_alquiler <- datos %>% 
    filter(!...1 %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  datos_alquiler <- datos %>% 
    filter(...1 %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  # Obtener nombres de columnas de CCAA (todas excepto la primera que es ...1)
  columnas_ccaa <- names(datos)[names(datos) != "...1"]
  
  # Crear mapeo entre nombres de columnas y códigos de CCAA
  # Basado en los nombres que veo en tu estructura
  mapeo_ccaa <- c(
    "01 Andalucía" = "Andalucía",
    "02 Aragón" = "Aragón", 
    "03 Asturias, Principado de" = "Asturias, Principado de",
    "04 Balears, Illes" = "Balears, Illes",
    "05 Canarias" = "Canarias",
    "06 Cantabria" = "Cantabria",
    "07 Castilla y León" = "Castilla y León",
    "08 Castilla - La Mancha" = "Castilla-La Mancha",  # Corregido: sin espacios alrededor del guión
    "09 Cataluña" = "Cataluña",
    "10 Comunitat Valenciana" = "Comunitat Valenciana",
    "11 Extremadura" = "Extremadura",
    "12 Galicia" = "Galicia",
    "13 Madrid, Comunidad de" = "Madrid, Comunidad de",
    "14 Murcia, Región de" = "Murcia, Región de",
    "15 Navarra, Comunidad Foral de" = "Navarra, Comunidad Foral de",
    "16 País Vasco" = "País Vasco",
    "17 Rioja, La" = "Rioja, La",
    "18 Ceuta" = "Ceuta",
    "19 Melilla" = "Melilla"
  )
  
  # Procesar cada CCAA
  for (columna in columnas_ccaa) {
    
    # Obtener nombre de CCAA para buscar en peso_alquiler_ccaa
    nombre_ccaa <- mapeo_ccaa[columna]
    if (is.na(nombre_ccaa)) {
      nombre_ccaa <- columna  # Si no está en el mapeo, usar el nombre tal como está
    }
    
    # Buscar peso específico para esta CCAA y año
    peso_alquiler_especifico <- peso_alquiler_ccaa %>%
      filter(AÑO_PONDERACION == año_ponderacion, nombre == nombre_ccaa) %>%
      pull(ESFUERZO)
    
    if (length(peso_alquiler_especifico) == 0) {
      stop("ERROR: No se encontró peso de alquiler para ", nombre_ccaa, " en año ", año_ponderacion, 
           ". Verifica que los datos existan antes de continuar.")
    }
    
    # Convertir de proporción a peso sobre 1000 si es necesario
    nuevo_peso_alquiler <- if (peso_alquiler_especifico <= 1) peso_alquiler_especifico * 1000 else peso_alquiler_especifico
    
    # Calcular peso disponible para el resto
    peso_disponible_resto <- 1000 - nuevo_peso_alquiler
    
    # Calcular total original del resto (sin alquiler) para esta CCAA
    total_resto_original <- sum(datos_sin_alquiler[[columna]], na.rm = TRUE)
    
    # Reponderar el resto proporcionalmente para esta CCAA
    datos_sin_alquiler[[paste0(columna, "_nueva")]] <- 
      (datos_sin_alquiler[[columna]] / total_resto_original) * peso_disponible_resto
    
    # Actualizar peso del alquiler para esta CCAA
    datos_alquiler[[paste0(columna, "_nueva")]] <- nuevo_peso_alquiler
    
    message("CCAA: ", nombre_ccaa, " - Nuevo peso alquiler: ", round(nuevo_peso_alquiler, 2))
  }
  
  # Crear dataframe final con estructura original
  datos_reponderados <- datos_sin_alquiler[, c("...1", paste0(columnas_ccaa, "_nueva"))]
  names(datos_reponderados) <- c("...1", columnas_ccaa)
  
  # Agregar la fila de alquiler
  fila_alquiler_nueva <- datos_alquiler[, c("...1", paste0(columnas_ccaa, "_nueva"))]
  names(fila_alquiler_nueva) <- c("...1", columnas_ccaa)
  
  # Combinar todo manteniendo el orden original
  datos_reponderados <- bind_rows(datos_reponderados, fila_alquiler_nueva)
  
  # Reordenar según el orden original de las filas
  orden_original <- datos$...1
  datos_reponderados <- datos_reponderados %>%
    arrange(match(...1, orden_original))
  
  # Verificar que suma 1000 para cada CCAA
  for (columna in columnas_ccaa) {
    suma_total <- sum(datos_reponderados[[columna]], na.rm = TRUE)
    message("Suma total para ", columna, ": ", round(suma_total, 2))
  }
  
  # Guardar
  write_xlsx(datos_reponderados, archivo_salida)
  
  return(datos_reponderados)
}

# Lista de archivos a reponderar (ajustar nombre del archivo según tu estructura)
archivos_originales <- file.path(ruta_ponderaciones_originales_CCAA, 
                                 paste0("ponderaciones_IPC_CCAA_", anios_ponderaciones_IPC, ".xlsx"))

# Función para obtener año del nombre del archivo (ajustada para formato CCAA)
extraer_año <- function(nombre_archivo) {
  # Para archivos como "ponderaciones_IPC_CCAA_2017.xlsx"
  year_match <- regmatches(nombre_archivo, regexpr("\\d{4}", nombre_archivo))
  if (length(year_match) > 0) {
    return(as.numeric(year_match[1]))
  } else {
    return(NA)
  }
}

# Procesar cada archivo
for (archivo in archivos_originales) {
  if (file.exists(archivo)) {
    # Extraer año del archivo
    año <- extraer_año(basename(archivo))
    
    if (is.na(año)) {
      message("No se pudo extraer el año de: ", basename(archivo))
      next
    }
    
    message("Procesando año ", año)
    
    nombre_salida <- file.path(ruta_ponderaciones_nuevas_CCAA, 
                               paste0("alquiler_ajustado_", basename(archivo)))
    
    # Llamar a la función con los datos de peso por CCAA
    reponderar_con_alquiler_ajustado_ccaa(archivo, nombre_salida, año, peso_alquiler_ccaa)
    
    message("Procesado: ", basename(archivo))
    
  } else {
    message("Archivo no encontrado: ", basename(archivo))
  }
}

##### Funciones para verificar
# Función adicional para verificar resultados
verificar_reponderacion_ccaa <- function(archivo_reponderado) {
  datos <- read_excel(archivo_reponderado)
  columnas_ccaa <- names(datos)[names(datos) != "...1"]
  
  cat("Verificación de sumas por CCAA:\n")
  for (columna in columnas_ccaa) {
    suma <- sum(datos[[columna]], na.rm = TRUE)
    cat(columna, ": ", round(suma, 2), "\n")
  }
  
  # Verificar que el alquiler tiene el peso esperado
  fila_alquiler <- datos %>% 
    filter(...1 %in% c("Alquiler de vivienda", "041 Alquiler de vivienda"))
  
  if (nrow(fila_alquiler) > 0) {
    cat("\nPesos de alquiler por CCAA:\n")
    for (columna in columnas_ccaa) {
      peso <- fila_alquiler[[columna]]
      cat(columna, ": ", round(peso, 2), "\n")
    }
  }
}

# Función para ver qué CCAA y años tenemos disponibles en peso_alquiler_ccaa
revisar_datos_disponibles <- function() {
  cat("Años disponibles:\n")
  print(sort(unique(peso_alquiler_ccaa$AÑO_PONDERACION)))
  
  cat("\nCCAA disponibles:\n")
  print(unique(peso_alquiler_ccaa$nombre))
  
  cat("\nResumen por año:\n")
  resumen <- peso_alquiler_ccaa %>%
    group_by(AÑO_PONDERACION) %>%
    summarise(n_ccaa = n_distinct(nombre), .groups = 'drop')
  print(resumen)
}

# Verificar todos los archivos procesados
cat("\n=== VERIFICACIÓN DE TODOS LOS ARCHIVOS ===\n")
archivos_procesados <- list.files(ruta_ponderaciones_nuevas_CCAA, 
                                  pattern = "alquiler_ajustado_.*\\.xlsx", 
                                  full.names = TRUE)

for (archivo in archivos_procesados) {
  cat("\n--- ", basename(archivo), " ---\n")
  verificar_reponderacion_ccaa(archivo)
}

# Revisar qué datos están disponibles
cat("\n=== DATOS DISPONIBLES ===\n")
revisar_datos_disponibles()

