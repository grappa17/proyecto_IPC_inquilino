


                  ### SCRIPT PARA OPTIMIZAR LOS DATOS MAS PESADOS ###
                  ### SCRIPT PARA OPTIMIZAR LOS DATOS MAS PESADOS ###
                  ### SCRIPT PARA OPTIMIZAR LOS DATOS MAS PESADOS ###

# Carga librerias
library(data.table)


# DIRECTORIO
# Directorio base
base_dir <- getwd()

# Resto rutas
ruta <- file.path(base_dir, "DATOS/EPF")
ruta_datos_epf_actualizados <- file.path(base_dir, "DATOS/EPF_NUEVO")

if (!dir.exists(ruta_datos_epf_actualizados)) {
  dir.create(ruta_datos_epf_actualizados, recursive = TRUE)
}


# Convert .tab to .csv if needed (set to true if 2023 and 2024 are missing as csv)
for (i in 2023:2024) {
    dt_gasto <- file.path(ruta, paste0("/EPFgastos_", i))
    dt_hogar <- file.path(ruta, paste0("/EPFhogar_", i))
    fwrite(fread(paste0(dt_gasto, ".tab"), sep = "\t"), paste0(dt_gasto, ".csv"))
    fwrite(fread(paste0(dt_hogar, ".tab"), sep = "\t"), paste0(dt_hogar, ".csv"))
}

# Listamos los ficheros originales
files <- list.files(ruta, pattern = ".csv", full.names = TRUE)

# Optimizamos
for (f in files) {
  dt <- fread(f)
  dt[is.na(dt)] <- 0
  pre <- ifelse(f %like% "gasto", "gastos_join_", "hogar_join_")
  fwrite(dt, file.path(ruta_datos_epf_actualizados, paste0(pre, dt$ANOENC[1], ".tsv.gz")), 
         sep = "\t", compress = "gzip")
}


