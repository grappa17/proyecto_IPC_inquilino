# Reduce basefile size x 90% and sanitize NAs to 0

library(data.table)
library(survey)

# Convert .tab to .csv if needed (set to true if 2023 and 2024 are missing as csv)
for (i in 2023:2024) {
    dt_gasto <- paste0("DATOS/EPF/EPFgastos_", i)
    dt_hogar <- paste0("DATOS/EPF/EPFhogar_", i)
    fwrite(fread(paste0(dt_gasto, ".tab"), sep = "\t"), paste0(dt_gasto, ".csv"))
    fwrite(fread(paste0(dt_hogar, ".tab"), sep = "\t"), paste0(dt_hogar, ".csv"))
}

# Listamos los ficheros originales
files <- list.files("DATOS/EPF/", pattern = ".csv", full.names = TRUE)

# Optimizamos
for (f in files) {
  dt <- fread(f) # leer iteración de wave
  dt[is.na(dt)] <- 0 # Sanitize all NAs to 0
  pre <- ifelse(f %like% "gasto", "test/out/gastos_join_", "test/out/hogar_join_")
  fwrite(dt, paste0(pre, dt$ANOENC[1], ".tsv.gz"), sep = "\t", compress = "gzip")
}