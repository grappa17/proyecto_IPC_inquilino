

                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##

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
ruta <- file.path(base_dir, "DATOS/EPF")
ruta_nuevos_pesos_alquiler <- file.path(base_dir, "DATOS/PONDERACIONES/PESOS_ALQUILER_EPF")

# Crear carpeta si no existe (en las que se guardarán los nuevos pesos del alquiler)
if (!dir.exists(ruta_nuevos_pesos_alquiler)) {
  dir.create(ruta_nuevos_pesos_alquiler, recursive = TRUE)
}



####################### Carga de datos y preparacion de dfs ####
# Definir todas las columnas que quiero utilizar, incluyendo las variantes flag
columnas_hogares <- c('ANOENC', 'NUMERO', 'CCAA', 'NUTS1', 'FACTOR', 'REGTEN',
                      'GASTOT', 'GASTNOM4')

columnas_gastos <- c('ANOENC', 'NUMERO', 'CODIGO', 'GASTO', 'FACTOR')

cargar_datos <- function(tipo, columnas) {
  archivos_2023 <- file.path(ruta, paste0("EPF", tipo, anios_ponderaciones, ".tab")) # defino la ruta de todos los archivos que quiero. La funcion filepath sirve para crar la ruta uniendo los distintos componentes.
  archivos_2022 <- file.path(ruta, paste0("EPF", tipo, anios_ponderaciones, ".csv")) 
  archivos <- c(archivos_2022, archivos_2023)
  
  dt_list <- lapply(archivos, function(archivo) {
    # Verificar si el archivo existe antes de leerlo
    if(file.exists(archivo)) {
      fread(archivo, select = columnas, na.strings = c("", "NA"))
    } else {
      warning(paste("Archivo no encontrado:", archivo))
      NULL
    }
  })
  
  # Filtrar elementos nulos (archivos que no existen)
  dt_list <- dt_list[!sapply(dt_list, is.null)]
  
  return(rbindlist(dt_list, fill = TRUE))
}


df_hogares <- cargar_datos("hogar_", columnas_hogares) # ejecuto las funciones
df_gastos <- cargar_datos("gastos_", columnas_gastos)

# En el df de gastos filtro solo las observaciones de alquiler $CODIGO == 04110
df_gastos <- df_gastos %>% 
  filter(CODIGO == "04110") %>%
  rename(GASTO_ALQUILER = GASTO) %>%
  rename(FACTOR_ALQUILER = FACTOR) %>%
  select(-CODIGO)

# Uno dfs
df_completo <- df_hogares %>%
  left_join(df_gastos, by = c("ANOENC", "NUMERO")) %>%
  filter(REGTEN == 3)

###################### Calculo #####

df_completo <- df_completo %>% 
  mutate(GASTO_SIN_IMP = GASTOT) # Tomo solo el gasto monetario, que no incluye alquiler imputado ni autoconsumo

peso_alquiler_anual <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
           GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  group_by(ANOENC) %>%
  summarise(
    n_observaciones = n(),
    # Ratio de totales: gasto agregado en alquiler / gasto agregado total
    gasto_total_alquiler_ponderado = sum(GASTO_ALQUILER * FACTOR, na.rm = TRUE),
    gasto_total_sin_imp_ponderado = sum(GASTO_SIN_IMP * FACTOR, na.rm = TRUE),
    peso_alquiler_medio = gasto_total_alquiler_ponderado / gasto_total_sin_imp_ponderado
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 5),
  ) %>%
  arrange(ANOENC)


###################### Repetir con CCAA ########
peso_alquiler_anual_ccaa <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
           GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  group_by(ANOENC, CCAA) %>%
  summarise(
    n_observaciones = n(),
    # Ratio de totales por CCAA
    gasto_total_alquiler_ponderado = sum(GASTO_ALQUILER * FACTOR, na.rm = TRUE),
    gasto_total_sin_imp_ponderado = sum(GASTO_SIN_IMP * FACTOR, na.rm = TRUE),
    peso_alquiler_medio = gasto_total_alquiler_ponderado / gasto_total_sin_imp_ponderado,
    .groups = 'drop'
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 5)
  ) %>%
  arrange(CCAA, ANOENC)

ccaa <- tribble(
  ~CCAA, ~nombre,
  "01", "Andalucía",
  "02", "Aragón",
  "03", "Asturias, Principado de",
  "04", "Balears, Illes",
  "05", "Canarias",
  "06", "Cantabria",
  "07", "Castilla y León",
  "08", "Castilla-La Mancha",
  "09", "Cataluña",
  "10", "Comunitat Valenciana",
  "11", "Extremadura",
  "12", "Galicia",
  "13", "Madrid, Comunidad de",
  "14", "Murcia, Región de",
  "15", "Navarra, Comunidad Foral de",
  "16", "País Vasco",
  "17", "Rioja, La",
  "18", "Ceuta",
  "19", "Melilla"
)

ccaa <- ccaa %>%
  mutate(CCAA = as.integer(CCAA)) # Paso al mismo formato para poder unir

peso_alquiler_anual_ccaa <- peso_alquiler_anual_ccaa %>% # Union de columnas
  left_join(ccaa, by = "CCAA")

# Preparar datos finales del peso del alquiler y guardar

# Alquiler estatal
peso_alquiler_anual <- peso_alquiler_anual %>%
  select(-n_observaciones, -gasto_total_alquiler_ponderado, -gasto_total_sin_imp_ponderado) %>%
  mutate(AÑO_PONDERACION = ANOENC + 1)  %>%
  rename(AÑO_DATOS = ANOENC,
         ESFUERZO = peso_alquiler_medio)

write_xlsx(peso_alquiler_anual,
           file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_estatal.xlsx"),
           col_names = TRUE)


# Alquiler CCAA
peso_alquiler_anual_ccaa <- peso_alquiler_anual_ccaa %>%
  select(-n_observaciones, -gasto_total_alquiler_ponderado, -gasto_total_sin_imp_ponderado) %>%
  mutate(AÑO_PONDERACION = ANOENC + 1)  %>%
  rename(AÑO_DATOS = ANOENC,
         ESFUERZO = peso_alquiler_medio)

write_xlsx(peso_alquiler_anual_ccaa,
           file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_ccaa.xlsx"),
           col_names = TRUE)




