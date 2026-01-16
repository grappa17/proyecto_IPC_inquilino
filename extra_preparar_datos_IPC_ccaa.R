

# El objetivo de este script es actualizar los datos de IPC de comunidades autonomas y estructurarlos
# Lo que he hecho es descargar los nuevos datos ("IPC_2002_2025_CCAA_act.xlsx") y ponerlos manualmente en la misma
# estructura que los anteriores.

# CARGA LIBRERIAS
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(writexl)


# DIRECTORIO
# Directorio base
base_dir <- getwd()

# Resto rutas
ruta_ipc_original <- file.path(base_dir, "DATOS/IPC ORIGINAL")
ruta_precios_alquiler <- file.path(base_dir, "DATOS/PRECIOS ALQUILER")
ruta_ponderaciones_definitivas <- file.path(base_dir, "DATOS/PONDERACIONES/PONDERACIONES_DEFINITIVAS")
ruta_resultados <- file.path(base_dir, "DATOS/RESULTADOS")

# Cargo datos de IPC con subgrupos

df_base <- read_xlsx(
  path = file.path(ruta_ipc_original, "IPC_2002_2025_CCAA.xlsx"),
  col_names = TRUE)

df_act <- read_xlsx(
  path = file.path(ruta_ipc_original, "IPC_2002_2025_CCAA_act.xlsx"),
  col_names = TRUE)

# Renombrar la columna ...1 a "mes" en ambos dataframes
df_base <- df_base %>%
  rename(mes = ...1)

df_act <- df_act %>%
  rename(mes = ...1)

# Combinar ambos dataframes y ordenar
df_completo <- bind_rows(df_base, df_act) %>%
  arrange(CCAA, mes)

# Guardo tabla
write_xlsx(df_completo,
           path = file.path(ruta_ipc_original, "IPC_2002_2025_CCAA_2.xlsx"))
