# Comentarios segunda versión 27/11/3032

## General

Ya lo tengo revisado entero más o menos en detalle y lo primero comentarte que en términos generales los resultados tienen pinta de ser sólidos, aunque la parte "dura" estadística pinta guay (aplicaciones de pesos muestrales, ponderaciones…). Algún detalle que creo que habría que tener en cuenta para terminar de hilarlo :) (lo tienes todo en la carpeta test de la raiz)
Algún detalle que creo que habría que tener en cuenta para terminar de hilarlo :) (lo tienes todo en la [carpeta `test` de la raíz](./test))

## Dudas y potenciales mejoras

1. **¿Por qué has incluido a los usuarios con vivienda en cesión?** No es un tipo de alquiler de mercado y disminuye el peso obtenido al final de los cálculos. Al incluir el código 4 (`REGTEN == 4`, que suele ser alquiler inferior a mercado o cesión), estás diluyendo el esfuerzo de los inquilinos de mercado libre.

```r
# Líneas 81-83 del script
df_completo <- df_hogares %>%
  left_join(df_gastos, by = c("ANOENC", "NUMERO")) %>%
  filter(REGTEN == 3 | REGTEN == 4) # <--- Aquí incluyes alquiler de mercado (3) y cesión/inferior (4)
```

2.  **Tampoco creo que la manera que tienes de aislar los gastos no monetarios sea la más correcta.** Estás intentando reconstruir el gasto monetario restando manualmente una partida (`GASTNOM4`) al gasto total (`GASTOT`). Esto es arriesgado porque `GASTOT` puede tener otras imputaciones (autoconsumo, salario en especie, etc.) que no estás limpiando.

<!-- end list -->

```r
# Línea 88 del script
mutate(GASTO_SIN_IMP = GASTOT - coalesce(GASTNOM4, 0)) # Tomo los valores de GASTNOM4 como "0" antes de la operación
```

Has mencionado `GASTMON` (o similar en las variables del INE) y es un punto excelente. En la EPF, `GASTOT` suele incluir el "Alquiler Imputado" (lo que pagaría un propietario si viviera de alquiler). Si usas `GASTOT` en el denominador para inquilinos, el error es menor (porque ellos no tienen alquiler imputado), pero pueden tener autoconsumo u otras imputaciones. Usar `GASTMON` (Gasto Monetario) es lo metodológicamente "puro" para un IPC, ya que el IPC mide precios de transacción monetaria real.

Al corregir esto, es muy probable que el peso del alquiler salga ligeramente más alto que si se usara el gasto total bruto, lo cual refuerza la tesis de la pérdida de poder adquisitivo.

3.  **Por último te recomiendo encarecidamente que cuando trabajes con encuestas con pesos poblacionales (todas :P) vayas siempre por defecto a usar la librería `survey`**, tanto por la facilidad de obtener los estimadores / implementar pesos / filtrar si no, sobre todo, para poder obtener las desv. std. e & intervalos de confianza de los mismos.

El método que usas usando `weighted mean` con `dplyr` te da estimaciones puntuales correctas (si se hace como ratio de sumas) pero sin error típico y el resto de parámetros de control de encuesta / estimación que `survey` te da.

```r
# Líneas 97-99 del script: Cálculo manual con dplyr (sin gestión de varianza muestral)
gasto_total_alquiler_ponderado = sum(GASTO_ALQUILER * FACTOR, na.rm = TRUE),
gasto_total_sin_imp_ponderado = sum(GASTO_SIN_IMP * FACTOR, na.rm = TRUE),
peso_alquiler_medio = gasto_total_alquiler_ponderado / gasto_total_sin_imp_ponderado
```

---

---

## Propuesta de optimización inicial

El resto ya son consejos de uso general que creo te pueden facilitar la vida con R. Te paso una propuesta más o menos completa de los pasos iniciales para que veas cómo quedaría usando `data.table` para eficiencia y `survey` para rigor estadístico:

Lo primero, para poder optimizar el tamaño y subirlos a github sin llegar al limite lo comprimimos en gz que data.table sigue leyendolo muy rapido y reduce un 90% el tamaño:

```r
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
```

### Calculo con errores estandar y objetos de encuesta 

Y aqui va una implementacion de tu script 1 incluyendo los comentarios que te hacia al principio incluidos:

```r
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
```
