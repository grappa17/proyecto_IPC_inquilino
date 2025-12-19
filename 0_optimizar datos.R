


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

# Parentesis para estimar valores NA
# Inicializamos dataframe para almacenar resultados
resultados_na <- data.frame(
  archivo = character(),
  tipo = character(),
  year = integer(),
  variable = character(),
  num_na = integer(),
  stringsAsFactors = FALSE
)

# Iteramos sobre los archivos
for (f in files) {
  dt <- fread(f)
  
  # Determinamos tipo y año
  tipo <- ifelse(f %like% "gasto", "gastos", "hogar")
  year <- dt$ANOENC[1]
  
  # Seleccionamos variables segun el tipo
  if (tipo == "gastos") {
    vars <- c("CODIGO", "GASTO", "NUMERO")
  } else {
    vars <- c("GASTMON", "REGTEN")
  }
  
  # Contamos NAs para cada variable
  for (v in vars) {
    if (v %in% names(dt)) {
      num_na <- sum(is.na(dt[[v]]))
      resultados_na <- rbind(resultados_na, 
                             data.frame(archivo = basename(f),
                                        tipo = tipo,
                                        year = year,
                                        variable = v,
                                        num_na = num_na))
    }
  }
}

# Mostramos resultados
print(resultados_na)

# Optimizamos
for (f in files) {
  dt <- fread(f)
  # dt[is.na(dt)] <- 0 # Esta es la línea que coerciona a 0 los NAs. Prefiero no hacerlo y tratarlos excluyendolos, aunque no cambia el resultado.
  pre <- ifelse(f %like% "gasto", "gastos_join_", "hogar_join_")
  fwrite(dt, file.path(ruta_datos_epf_actualizados, paste0(pre, dt$ANOENC[1], ".tsv.gz")),
         sep = "\t", compress = "gzip")
}


