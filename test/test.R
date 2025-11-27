# Reduce basefile size x 90% and sanitize NAs to 0

library(data.table)
library(survey)

# Convert .tab to .csv if needed (set to true if 2023 and 2024 are missing as csv)
pending <- TRUE

if (pending) {
  for (i in 2023:2024) {
    dt_gasto <- paste0("DATOS/EPF/EPFgastos_", i)
    dt_hogar <- paste0("DATOS/EPF/EPFhogar_", i)
    fwrite(fread(paste0(dt_gasto, ".tab"), sep = "\t"), paste0(dt_gasto, ".csv"))
    fwrite(fread(paste0(dt_hogar, ".tab"), sep = "\t"), paste0(dt_hogar, ".csv"))
  }
}

# Process files with NA sanitization
files <- list.files("DATOS/EPF/", pattern = ".csv", full.names = TRUE)

for (f in files) {
  dt <- fread(f) # leer iteración de wave
  dt[is.na(dt)] <- 0 # Sanitize all NAs to 0
  # Correctly check the loop variable 'f' to determine the output prefix
  pre <- ifelse(f %like% "gasto", "DATOS/REDUCED/gastos_join_", "DATOS/REDUCED/hogar_join_")
  fwrite(dt,paste0(pre, dt$ANOENC[1], ".tsv.gz"), sep = "\t", compress = "gzip")

}

# globals
ratio_alquiler <- ratio_ccaa <- list() 
keys <- c("NUMERO", "ANOENC", "FACTOR")
years <- c(2017:2024)

# iteramos por años para obtener los resultados de cada wave
for (i in years) {

  # importamos nuevos ficheros optimizados
  dt1 <- fread(paste0("DATOS/REDUCED/gastos_join_", i, ".tsv.gz"))
  dt2 <- fread(paste0("DATOS/REDUCED/hogar_join_", i, ".tsv.gz"))

  # calculamos el gasto en alquiler por hogar
rent_agg <- dt1[grep("^0411", CODIGO), .(GALQ = sum(GASTO, na.rm = TRUE)), by = "NUMERO"]

# We use dt2 as the base because it contains the correct annual GASTMON (1 row per family)
dt <- merge(dt2, rent_agg, by = "NUMERO", all.x = TRUE)[is.na(GALQ), GALQ := 0]

  # definimos la encuesta y acotamos la muestra para crear objeto survey con pesos
  sv_dt <- svydesign(id = ~1, weights = ~FACTOR, data = subset(dt, ANOENC == i))

  # para el total simplemente syytotal
  alquiler <- svytotal(~GALQ, subset(sv_dt, REGTEN == 3))
  gasto <- svytotal(~GASTMON, subset(sv_dt, REGTEN == 3))

  # para poder cruzar por CCAA como segunda categoria se usa el objeto svyby
  alquiler_ccaa <- svyby(~GALQ, ~ CCAA, subset(sv_dt, REGTEN == 3), svytotal)
  gasto_ccaa <- svyby(~GASTMON, ~ CCAA, subset(sv_dt, REGTEN == 3), svytotal)

  # finalmente obtenemos los nuevos pesos
  ratio_ccaa[[as.character(i)]] <- cbind(alquiler_ccaa, gasto_ccaa)
  ratio_alquiler[[as.character(i)]] <- alquiler / gasto

  rm(dt, dt1, dt2, sv_dt) # Clean up large objects to manage memory
}

# exportamos los resultados tras normalizarlos con rbindlist & rbind 
rbindlist(ratio_ccaa)[,PORCENTAJE_ALQUILER := GALQ / GASTMON] %>% fwrite("ratio_ccaa.csv")
data.table(year = names(ratio_alquiler), ratio = unlist(ratio_alquiler)) %>% fwrite("ratio_alquiler.csv")