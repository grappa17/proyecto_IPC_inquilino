

                      ## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (CCAA) ##
                      ## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (CCAA) ##
                      ## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (CCAA) ##
                      ## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (CCAA) ##


# Ahora se replica el proceso anterior pero con los datos de CCAAs.

# CARGA LIBRERIAS
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(writexl)

# AÑOS A OBTENER

anios_ponderaciones <- (min(anios) - 2):(max(anios) - 1) # Para los años que quiero obtener, necesito años previos de ponderaciones
anios_ponderaciones_IPC <- (min(anios) - 1):(max(anios)) # Para los años que quiero obtener, necesito años previos de ponderaciones


# DIRECTORIO
# Directorio base
base_dir <- getwd()

# Resto rutas
ruta_ipc_original <- file.path(base_dir, "DATOS/IPC ORIGINAL")
ruta_precios_alquiler <- file.path(base_dir, "DATOS/PRECIOS ALQUILER")
ruta_ponderaciones_definitivas <- file.path(base_dir, "DATOS/PONDERACIONES/PONDERACIONES_DEFINITIVAS")
ruta_resultados <- file.path(base_dir, "DATOS/RESULTADOS")

#### CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (CCAA) #####

# Cargo datos de IPC con subgrupos
# Esto es el IPC normal
ipc_original_ccaa <- read_xlsx(
  path = file.path(ruta_ipc_original, "IPC_2002_2025_CCAA.xlsx"),
  col_names = TRUE)

# Lo transformo para trabajar mejor con el, aunque anes ya lo he preparado en excel
ipc_largo_ccaa <- ipc_original_ccaa %>%
  pivot_longer(
    cols = -c(`...1`, CCAA),   # todas menos fecha y CCAA
    names_to = "Componente",   # nombre del subgrupo
    values_to = "Valor"        # valor correspondiente
  )

# Selecciono años que me interesan
# Hay que seleccionar siempre un año menos del que se quiere para luego poder hacer los calculos

ipc_filtrado_ccaa <- ipc_original_ccaa %>%
  filter(substr(`...1`, 1, 4) %in% as.character(anios_ponderaciones_IPC)) # Selecciono los 4 primeros caracteres (el año) en la columna que me interesa










###################### Desencadenar IPC Original CCAA #####


# Crear columnas de año y mes a partir de la columna ...1, para facilitar trabajo
ipc_filtrado_ccaa <- ipc_filtrado_ccaa %>%
  mutate(
    anio = as.numeric(str_sub(...1, 1, 4)),
    mes = as.numeric(str_sub(...1, 6, 7))
  ) %>%
  select(anio, mes, CCAA, everything(), -...1)  # Reorganizar columnas



ipc_desencadenado_ccaa <- ipc_filtrado_ccaa %>%
  # Asegurar que los datos están ordenados por año y mes
  arrange(anio, mes) %>%
  # Pivotar a formato largo para trabajar mejor con las CCAA
  pivot_longer(
    cols = -c(anio, mes, CCAA),
    names_to = "Componente",
    values_to = "valor"
  ) %>%
  # Agrupar por CCAA Y producto (cada producto tiene su propia serie por CCAA)
  group_by(CCAA, Componente) %>%
  arrange(anio, mes) %>%
  # Calcular el valor de referencia (diciembre del año anterior)
  mutate(
    diciembre_anterior = ifelse(
      mes == 1, 
      lag(valor, 1), # Tomar diciembre del año anterior
      NA
    )
  ) %>%
  # Llenar el valor de referencia para todos los meses
  fill(diciembre_anterior, .direction = "down") %>%
  # Calcular el índice (base 100 en diciembre anterior)
  mutate(
    indice_base_100 = ifelse(
      !is.na(diciembre_anterior),
      (valor / diciembre_anterior) * 100,
      NA
    )
  ) %>%
  # Reordenar columnas
  select(anio, mes, CCAA, Componente, valor, diciembre_anterior, indice_base_100) %>%
  ungroup()


###################### Unir valores alquiler ######
# Uso los datos de idealista, a sabiendas de las limitaciones que tienen.

# En el script general había desencadenado el índice manualemente, pero aquí voy a desencadenarlo aquí que es más sencillo

# Cargo datos
df_vivienda_idealista_ccaa_sin_desencadenar <- read_xlsx(
  path = file.path(ruta_precios_alquiler, "Precio_alquiler_CCAA.xlsx"),
  col_names = TRUE)

# Selecciono años
df_vivienda_idealista_ccaa_sin_desencadenar <- df_vivienda_idealista_ccaa_sin_desencadenar %>%
  filter(anio %in% anios_ponderaciones_IPC)

df_vivienda_idealista_ccaa_desencadenado <- df_vivienda_idealista_ccaa_sin_desencadenar %>%
  # Asegurar que los datos están ordenados por año y mes
  arrange(anio, mes) %>%
  # Pivotar a formato largo para trabajar mejor con las CCAA
  pivot_longer(
    cols = -c(anio, mes),
    names_to = "CCAA",
    values_to = "Precio_m2"
  ) %>%
  # Agrupar por CCAA
  group_by(CCAA) %>%
  # Calcular el valor de referencia (diciembre del año anterior)
  mutate(
    diciembre_anterior = ifelse(
      mes == 1, 
      lag(Precio_m2, 1), # Tomar diciembre del año anterior
      NA
    )
  ) %>%
  # Llenar el valor de referencia para todos los meses
  fill(diciembre_anterior, .direction = "down") %>%
  # Calcular el índice (base 100 en diciembre anterior)
  mutate(
    indice_base_100 = ifelse(
      !is.na(diciembre_anterior),
      (Precio_m2 / diciembre_anterior) * 100,
      NA
    )
  ) %>%
  # Seleccionar columnas relevantes
  select(anio, mes, CCAA, Precio_m2, diciembre_anterior, indice_base_100)

# Los preparos para unir al df del IPC original
ipc_desencadenado_ccaa <- ipc_desencadenado_ccaa %>%
  filter(Componente != "041 Alquiler de vivienda") # Borro datos originales del INE

df_vivienda_idealista_ccaa_desencadenado <- df_vivienda_idealista_ccaa_desencadenado %>%
  mutate(
    Componente = "041 Alquiler de vivienda"
  )

# Los uno
ipc_desencadenado_ccaa <- ipc_desencadenado_ccaa %>%
  bind_rows(df_vivienda_idealista_ccaa_desencadenado) %>% # Uno datos de idealista
  arrange(anio, mes, CCAA, Componente)







###################### Reponderar indice desencadenado ######
# Cargo ponderaciones
df_ponderaciones_CCAA <- readRDS(
  file.path(ruta_ponderaciones_definitivas, "ponderaciones_ccaa_definitivas.rds")
)

# Uno dfs
ipc_reponderado_ccaa <- ipc_desencadenado_ccaa %>%
  left_join(df_ponderaciones_CCAA, by = c("Componente", "anio", "CCAA")) %>% 
  mutate(
    valor_ponderado = indice_base_100 * Ponderacion
  )


# Calculo el nuevo índice
indice_general_ccaa <- ipc_reponderado_ccaa %>% 
  group_by(anio, mes, CCAA) %>% 
  summarise(
    Componente = "Índice general",
    valor = sum(valor_ponderado, na.rm = TRUE) / 1000) # Por qué salen valores tan raros en 2025?????




###################### Encadenamiento del nuevo indice #####

# Establecer punto base: enero 2021 = 100 para cada CCAA
indice_encadenado_ccaa <- indice_general_ccaa %>%
  ungroup() %>% # garantiza que no queden agrupaciones previas
  arrange(CCAA, anio, mes) %>% # ordena por CCAA primero, luego cronológicamente
  group_by(CCAA) %>% # agrupa por CCAA para procesar cada una independientemente
  mutate(
    # Identificar el punto base para cada CCAA
    es_base = (anio == anio_base & mes == mes_base),
    # Inicializar valores encadenados
    valor_encadenado = NA_real_,
    # Crear índice de fila dentro de cada CCAA
    fila_ccaa = row_number()
  ) %>%
  ungroup()

# Procesar cada CCAA por separado
ccaas <- unique(indice_encadenado_ccaa$CCAA)

for(ccaa in ccaas) {
  # Filtrar datos de la CCAA actual
  indices_ccaa <- which(indice_encadenado_ccaa$CCAA == ccaa)
  datos_ccaa <- indice_encadenado_ccaa[indices_ccaa, ]
  
  # Establecer valor base para esta CCAA
  punto_base_ccaa <- which(datos_ccaa$es_base)
  
  if(length(punto_base_ccaa) > 0) {
    # Asignar valor base en el dataframe principal
    indice_encadenado_ccaa$valor_encadenado[indices_ccaa[punto_base_ccaa]] <- 100
    
    # Encadenar hacia adelante desde el punto base
    if(punto_base_ccaa < nrow(datos_ccaa)) {
      for(i in (punto_base_ccaa + 1):nrow(datos_ccaa)) {
        idx_actual <- indices_ccaa[i]
        idx_anterior <- indices_ccaa[i-1]
        
        if(datos_ccaa$mes[i] == 1) {
          # Si el mes es enero, el índice se resetea a base 100
          factor_variacion <- datos_ccaa$valor[i] / 100
        } else {
          # Mes normal, comparar con mes anterior
          factor_variacion <- datos_ccaa$valor[i] / datos_ccaa$valor[i-1]
        }
        
        indice_encadenado_ccaa$valor_encadenado[idx_actual] <- 
          indice_encadenado_ccaa$valor_encadenado[idx_anterior] * factor_variacion
      }
    }
    
    # Encadenar hacia atrás desde el punto base (para datos anteriores a 2021)
    if(punto_base_ccaa > 1) {
      for(i in (punto_base_ccaa - 1):1) {
        idx_actual <- indices_ccaa[i]
        idx_siguiente <- indices_ccaa[i+1]
        
        if(datos_ccaa$mes[i+1] == 1) {
          # El siguiente es enero
          factor_variacion <- datos_ccaa$valor[i+1] / 100
          indice_encadenado_ccaa$valor_encadenado[idx_actual] <- 
            indice_encadenado_ccaa$valor_encadenado[idx_siguiente] / factor_variacion
        } else {
          # Mes normal
          factor_variacion <- datos_ccaa$valor[i+1] / datos_ccaa$valor[i]
          indice_encadenado_ccaa$valor_encadenado[idx_actual] <- 
            indice_encadenado_ccaa$valor_encadenado[idx_siguiente] / factor_variacion
        }
      }
    }
  }
}

# Limpiar columnas auxiliares
indice_encadenado_ccaa <- indice_encadenado_ccaa %>%
  select(-fila_ccaa) %>%
  arrange(anio, mes, CCAA)


###################### Verificación de coherencia por CCAA #####
indice_check <- indice_encadenado_ccaa %>%
  arrange(CCAA, anio, mes) %>%
  group_by(CCAA) %>%
  mutate(
    # 1. Valor encadenado de diciembre del año anterior (dentro de cada CCAA)
    valor_dic_ant = ifelse(mes == 1, lag(valor_encadenado), NA_real_),
    
    # 2. Fórmula estándar para eneros:
    valor_encadenado_teorico = ifelse(
      mes == 1 & !is.na(valor_dic_ant),
      valor * valor_dic_ant / 100,
      NA_real_
    )
  ) %>%
  ungroup()

# 3. Comparar tu cálculo vs el teórico en eneros
comparacion_eneros <- indice_check %>%
  filter(mes == 1 & !is.na(valor_encadenado_teorico)) %>%
  mutate(
    diff_abs = valor_encadenado - valor_encadenado_teorico,
    diff_rel = 100 * diff_abs / valor_encadenado_teorico
  )

# 4. Resumen general
resumen <- comparacion_eneros %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE),
    num_discrepancias = sum(abs(diff_abs) > 0.001, na.rm = TRUE), # Discrepancias > 0.001
    .groups = 'drop'
  )
print("=== RESUMEN GENERAL - VALIDACIÓN ENEROS ===")
print(resumen)

# 5. Resumen por CCAA (para identificar problemas específicos)
resumen_por_ccaa <- comparacion_eneros %>%
  group_by(CCAA) %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE),
    num_eneros = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(max_diff_abs))

print("=== RESUMEN POR CCAA - VALIDACIÓN ENEROS ===")
print(resumen_por_ccaa)

# Comprobación detallada del encadenado (todos los meses)
indice_check_full <- indice_encadenado_ccaa %>%
  arrange(CCAA, anio, mes) %>%
  group_by(CCAA) %>%
  mutate(
    # Valor encadenado teórico:
    valor_encadenado_teorico = case_when(
      # Primer punto de cada CCAA: NA
      row_number() == 1 ~ NA_real_,
      
      # Si es enero  usar diciembre del año anterior
      mes == 1 ~ valor * lag(valor_encadenado) / 100,
      
      # Si no es enero  usar el mes anterior
      TRUE ~ lag(valor_encadenado) * (valor / lag(valor))
    ),
    
    # Diferencias
    diff_abs = valor_encadenado - valor_encadenado_teorico,
    diff_rel = 100 * diff_abs / valor_encadenado_teorico
  ) %>%
  ungroup()

# Resumen de discrepancias - TODOS LOS MESES
resumen_full <- indice_check_full %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE),
    num_discrepancias = sum(abs(diff_abs) > 0.001, na.rm = TRUE),
    total_observaciones = sum(!is.na(diff_abs)),
    .groups = 'drop'
  )

print("=== RESUMEN GENERAL - VALIDACIÓN COMPLETA ===")
print(resumen_full)

# Resumen detallado por CCAA - TODOS LOS MESES
resumen_full_por_ccaa <- indice_check_full %>%
  group_by(CCAA) %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE),
    num_discrepancias = sum(abs(diff_abs) > 0.001, na.rm = TRUE),
    total_observaciones = sum(!is.na(diff_abs)),
    .groups = 'drop'
  ) %>%
  arrange(desc(max_diff_abs))

print("=== RESUMEN POR CCAA - VALIDACIÓN COMPLETA ===")
print(resumen_full_por_ccaa)

# Mostrar las mayores discrepancias (si las hay)
if(resumen_full$max_diff_abs > 0.001) {
  print("=== MAYORES DISCREPANCIAS DETECTADAS ===")
  mayores_discrepancias <- indice_check_full %>%
    filter(abs(diff_abs) > 0.001) %>%
    select(CCAA, anio, mes, valor, valor_encadenado, valor_encadenado_teorico, diff_abs, diff_rel) %>%
    arrange(desc(abs(diff_abs)))
  
  print(mayores_discrepancias)
}

###### Guardado de datos ######
# Obtener el IPC orginal para comparar (si me lo curro más creo que tendría que desencadenarlo y ponerlo exactamente en la misma base)
# Filtrar la fila "Índice general"
ipc_oficial_ccaa <- read_xlsx(
  path = file.path(ruta_ipc_original, "IPC_GENERAL_CCAA.xlsx"))

# Paso a formato largo
ipc_oficial_largo_ccaa <- ipc_oficial_ccaa %>%
  pivot_longer(
    cols = -`CCAA`,            # Mantener la primera columna para comunidades
    names_to = c("anio", "mes"), 
    names_sep = "M",           # Dependiendo de cómo estén nombradas las columnas (ej: 2021M01)
    values_to = "ipc_oficial"
  ) %>%
  mutate(
    anio = as.numeric(anio),
    mes = as.numeric(mes)
  ) %>%
  filter(anio %in% anios)

# Cambio de base
# Obtengo el valor de IPC del periodo base para cada CCAA
ipc_base_ccaa <- ipc_oficial_largo_ccaa %>%
  filter(anio == anio_base, mes == mes_base) %>%
  select(CCAA, ipc_oficial) %>%
  rename(ipc_base = ipc_oficial)

# Creo nueva columna con base seleccionada = 100 y elimino la original
ipc_oficial_largo_ccaa <- ipc_oficial_largo_ccaa %>%
  left_join(ipc_base_ccaa, by = "CCAA") %>%
  mutate(ipc_oficial = (ipc_oficial / ipc_base) * 100) %>%
  select(-ipc_base)

# Unir y limpiar
ipc_inquilinos_ccaa <- indice_encadenado_ccaa %>%
  select(
    -Componente, -valor, -es_base
  ) %>%
  rename(ipc_inquilinos = valor_encadenado) %>%
  filter(anio %in% anios) %>% # Filtrar años
  left_join(ipc_oficial_largo_ccaa, by = c("anio", "mes", "CCAA"))

# Elimino ceuta y melilla, añado fecha
ipc_inquilinos_ccaa <- ipc_inquilinos_ccaa %>%
  filter(!CCAA %in% c("18 Ceuta", "19 Melilla")) %>% # Elimino Ceuta y Melilla que tienen demasiadas pocas observaciones para que los datos sean validos
  mutate(
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  )

# guardar
saveRDS(ipc_inquilinos_ccaa,
        file = file.path(ruta_resultados, "ipc_inquilinos_ccaa.rds"))
write_xlsx(ipc_inquilinos_ccaa,
           path = file.path(ruta_resultados, "ipc_inquilinos_ccaa.xlsx"))
