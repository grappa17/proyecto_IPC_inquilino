

                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##
                    ## CALCULO DE PONDERACION DEL ALQUILER A PARTIR DE LOS MICRODATOS DE LA EPF ##

library(magrittr)
library(data.table)
library(survey)
library(dplyr)

# globals
ratio_alquiler <- ratio_ccaa <- list()

anios <- 2019:2025
years <- (min(anios) - 2):(max(anios) - 1)

# DIRECTORIO
# Directorio base
base_dir <- getwd()

# Resto rutas
ruta_datos_epf_actualizados <- file.path(base_dir, "DATOS/EPF_NUEVO")
ruta_nuevos_pesos_alquiler <- file.path(base_dir, "DATOS/PONDERACIONES/PESOS_ALQUILER_EPF")

# Crear carpeta si no existe (en las que se guardarán los nuevos pesos del alquiler)
if (!dir.exists(ruta_nuevos_pesos_alquiler)) {
  dir.create(ruta_nuevos_pesos_alquiler, recursive = TRUE)
}

# CARGA DATOS
# iteramos por años para obtener los resultados de cada wave
for (i in years) {
  
  # importamos nuevos ficheros optimizados
  dt1 <- fread(file.path(ruta_datos_epf_actualizados, paste0("/gastos_join_", i, ".tsv.gz")))
  dt2 <- fread(file.path(ruta_datos_epf_actualizados, paste0("/hogar_join_", i, ".tsv.gz")))
  
  # CALCULO PONDERACIONES
  # calculamos el gasto en alquiler por hogar
  rent_agg <- dt1[grep("^0411", CODIGO), .(GALQ = sum(GASTO, na.rm = TRUE)), by = "NUMERO"]
  
  # We use dt2 as the base because it contains the correct annual GASTMON (1 row per family)
  dt <- merge(dt2, rent_agg, by = "NUMERO", all.x = TRUE)[is.na(GALQ), GALQ := 0]
  
  # definimos la encuesta y acotamos la muestra para crear objeto survey con pesos
  sv_dt <- svydesign(id = ~1, weights = ~FACTOR, data =  subset(dt, ANOENC == i))
  
  # para el total simplemente syytotal
  alquiler <- svytotal(~GALQ, subset(sv_dt, REGTEN == 3))
  gasto <- svytotal(~GASTMON, subset(sv_dt, REGTEN == 3))
  
  # para poder cruzar por CCAA como segunda categoria se usa el objeto svyby
  alquiler_ccaa <- svyby(~GALQ, ~ CCAA, subset(sv_dt, REGTEN == 3), svytotal, na.rm = TRUE)
  gasto_ccaa <- svyby(~GASTMON, ~ CCAA, subset(sv_dt, REGTEN == 3), svytotal, na.rm = TRUE)
  
  # finalmente obtenemos los nuevos pesos
  ratio_ccaa[[as.character(i)]] <- merge(alquiler_ccaa, gasto_ccaa, by = "CCAA")
  ratio_alquiler[[as.character(i)]] <- alquiler / gasto
  
  rm(dt, dt1, dt2, sv_dt) # Clean up large objects to manage memory
}

# GUARDAR RESULTADOS

# Asignamos los resultados a objetos para visualizar en RStudio
resultado_ratio_alquiler <- data.table(year = names(ratio_alquiler), ratio = unlist(ratio_alquiler))
resultado_ratio_alquiler <- resultado_ratio_alquiler %>%
  rename(AÑO_DATOS = year,
         ESFUERZO = ratio) %>%
  mutate(
    AÑO_DATOS = as.numeric(AÑO_DATOS),
    AÑO_PONDERACION = AÑO_DATOS + 1
  ) %>% fwrite(file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_estatal.csv"))


resultado_ratio_ccaa <- rbindlist(ratio_ccaa, idcol = "ANOENC")[, PORCENTAJE_ALQUILER := GALQ / GASTMON]

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

resultado_ratio_ccaa <- resultado_ratio_ccaa %>%
  select(
    -GALQ,
    -se.x,
    -se.y,
    -GASTMON
  ) %>%
  rename(AÑO_DATOS = ANOENC,
         ESFUERZO = PORCENTAJE_ALQUILER) %>%
  left_join(ccaa, by = "CCAA") %>%
  mutate(
    AÑO_DATOS = as.numeric(AÑO_DATOS),
    AÑO_PONDERACION = AÑO_DATOS + 1
  ) %>% fwrite(file.path(ruta_nuevos_pesos_alquiler, "pesos_alquiler_ccaa.csv"))




