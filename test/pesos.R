library(magrittr)
library(data.table)
library(survey)

# globals
ratio_alquiler <- ratio_ccaa <- list()
years <- c(2017:2024)

# iteramos por años para obtener los resultados de cada wave
for (i in years) {

  # importamos nuevos ficheros optimizados
  dt1 <- fread(paste0("test/out/gastos_join_", i, ".tsv.gz"))
  dt2 <- fread(paste0("test/out/hogar_join_", i, ".tsv.gz"))

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

# exportamos los resultados tras normalizarlos con rbindlist & rbind 
rbindlist(ratio_ccaa, idcol = "ANOENC")[, PORCENTAJE_ALQUILER := GALQ / GASTMON] %>% fwrite("test/out/ratio_ccaa.csv")
data.table(year = names(ratio_alquiler), ratio = unlist(ratio_alquiler)) %>% fwrite("test/out/ratio_alquiler.csv")