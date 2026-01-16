


## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (ESTATAL) ##
## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (ESTATAL) ##
## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (ESTATAL) ##
## CALCULO DEL NUEVO IPC CON LAS NUEVAS PONDERACIONES (ESTATAL) ##



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


##### Carga de datos #####
# Cargo datos de IPC con subgrupos
# Esto es el IPC normal
ipc_original <- read_xlsx(
  path = file.path(ruta_ipc_original, "IPC_2002_2025.xlsx"),
                   col_names = TRUE)

# Selecciono años que me interesan
ipc_filtrado <- ipc_original %>%
  select(`...1`, contains(as.character(anios_ponderaciones_IPC))) # Mantengo primera columna (nombre categorias, y selecciono resto)
# Hay que seleccionar siempre un año menos del que se quiere para luego poder hacer los calculos





###################### Desencadenar IPC Original #####

# Lo primero que hay que hacer es desencadenarlos, dado que el INE los proporciona siguiendo la metodologia de Laspeyres encadenado. 
# Una vez desencadenados, es posible trabajar con los valores de los componentes.

# Para desencadenarlos, el valor de cada componente / subgrupo del año N se divide por el de diciembre del año N-1, y se multiplica por 100.


ipc_desencadenado <- ipc_filtrado %>%
# Convertir a formato largo para facilitar operaciones
pivot_longer(
    cols = -`...1`,
    names_to = c("anio", "mes"),
    names_sep = "M",
    values_to = "valor"
  ) %>%
  mutate(
    anio = as.numeric(anio),
    mes = as.numeric(mes)
  ) %>%
# Agrupar por componente
group_by(`...1`) %>%
# Ordenar cronológicamente
arrange(anio, mes, .by_group = TRUE) %>%
# Calcular valor de referencia (diciembre del año anterior)
mutate(
    valor_diciembre = ifelse(mes == 12, valor, NA_real_) # Creo una columna con el valor de diciembre de ese año
  ) %>%
  mutate(
    diciembre_anterior = lag(valor_diciembre) # Luego una con el año de diciembre del año anterior
  ) %>%
  fill(diciembre_anterior, .direction = "down") %>%
# Aplicar fórmula de desencadenamiento
mutate(
    valor_desencadenado = case_when(
      anio == min(anio) ~ NA_real_, # Mantener primer año
      is.na(diciembre_anterior) ~ NA_real_, # Si no hay referencia
      TRUE ~ 100 * (valor / diciembre_anterior)
    )
  )

# Modifico el nombre de la primera columna para trabajar mejor
ipc_desencadenado <- ipc_desencadenado %>%
  rename(Componente = `...1`)


###################### Unir valores alquiler ######
# Uso los datos de idealista, a sabiendas de las limitaciones que tienen. Idealista tiende a sobreestimar los incrementos de precios al reflejar solo los precios de las viviendas que están en el mercado.
# Pero por el momento, la unica alternativa que proporciona datos mensuales (ademas de otros portales inmobiliarios), son los propios datos del IPC de alquileres, 
# que parecen tener el efecto contrario. Cuando se comparan los datos de precios del alquiler del IPC con otros indicadores 
# especificos (como el IPVA, que desgraciadamente solo es anual), los aumentos que refleja el IPC son sustancialmente mas bajos.

# Previamente he convertido los datos de Idealista a numeros indice con base 100 en referencia a diciembre del año anterior, replicando el formato de los datos desencadenados del IPC.

# Cargo datos idealista
df_vivienda_idealista <- read_xlsx(
  path = file.path(ruta_precios_alquiler, "Precio_alquiler_estatal_desencadenado.xlsx"),
  col_names = TRUE)

# Borro dato de alquiler de los datos de IPC originales y uno los datos de idealista
ipc_desencadenado <- ipc_desencadenado %>%
  filter(Componente != "041 Alquiler de vivienda") %>% # Borro datos originales del INE
bind_rows(df_vivienda_idealista) # Uno datos de idealista



###################### Recalcular el indice desencadenado ######
# Una vez esta todo listo, los precios se recalculan con las nuevas ponderaciones. Para ello, cada valor desencadenado 
# se multiplica por su ponderacion.


# Cargo de nuevo las ponderaciones
ponderaciones_largo <- readRDS(
  file.path(ruta_ponderaciones_definitivas, "ponderaciones_estatales_definitivas.rds"))

# Uno dfs
ipc_reponderado <- ipc_desencadenado %>%
  left_join(ponderaciones_largo, by = c("Componente", "anio")) %>% # Y aqui no habría que unir también por mes?
mutate(
    valor_ponderado = valor_desencadenado * ponderacion
  )

ipc_reponderado <- ipc_reponderado %>%
  ungroup() %>%
  filter(Componente != "Índice general") # Elimino las observaciones de indice general que no son utiles

# Una vez calculados los componentes, se calcula el indice general sumando el valor de todos los componentes y dividiendolo por 1000.

# Calculo el nuevo índice
indice_general <- ipc_reponderado %>%
  group_by(anio, mes) %>%
  summarise(
    Componente = "Índice general",
    valor = sum(valor_ponderado, na.rm = TRUE) / 1000)





###################### Encadenamiento del nuevo indice #####

# Por ultimo, se vuelve a encadenar el índice general, para tenerlo en el mismo formato que el oficial. 
# En este caso utilizamos una base enero 2021 = 100. 

# Para volver a encadenar, el procedimiento es el siguiente: se establece el mes y año base. Para los meses posteriores, se distingue
# entre si es enero y si no es. Si es, se calcula el coficiente de variacion dividiendo entre 100 (porque el mes anterior actua como base), y si 
# no es enero, se divide entre el mes anterior. 

# Para los meses anteriores, se distingue entre si es diciembre, se divide el mes posterior (enero) entre 100 para obtener el cofieicente de variacion.
# Si no es diciembre, se divide el mes posterior entre el mes presente. 

# A partir del coeficiente de variacion, se encadena el indice desde el punto base.



# Establecer punto base:
indice_encadenado <- indice_general %>%
  ungroup() %>% # garantiza que no queden agrupaciones previas de dplyr que rompan el orden o el posterior indexado
arrange(anio, mes) %>% # ordena cronológicamente (fundamental para encadenar)
mutate(
# Identificar el punto base
    es_base = (anio == anio_base & mes == mes_base), # Marca la observacion que actua como base
# Inicializar valores encadenados
    valor_encadenado = NA_real_ # Crea una nueva columna inicialmente con valores NA, que luego cambiaran
  )

# Establecer valor base
punto_base <- which(indice_encadenado$es_base)
if (length(punto_base) > 0) {
  indice_encadenado$valor_encadenado[punto_base] <- 100 # Si la observacion es base, se asigna el valor 100

  # Encadenar hacia adelante desde el punto base
  for (i in (punto_base + 1):nrow(indice_encadenado)) {
    # Para todas las observaciones posteriores a la base
    if (indice_encadenado$mes[i] == 1) {
      # Si el mes es enero...
      factor_variacion <- indice_encadenado$valor[i] / 100 # ... el índice “desencadenado” se ha reseteado a base 100, por lo que no se puede comparar con el mes anterior, y se divide entre 100
    } else {
      # Si el mes es distinto a enero...
      factor_variacion <- indice_encadenado$valor[i] / indice_encadenado$valor[i - 1] # el factor es la variación mensual
    }
    indice_encadenado$valor_encadenado[i] <- indice_encadenado$valor_encadenado[i - 1] * factor_variacion # Se obtiene el valor encadenado multiplicando el valor del mes anterior por el factor de variación
  }

  # Encadenar hacia atrás desde el punto base (para datos anteriores a 2021)
  if (punto_base > 1) {
    # Lo mismo pero al revés
    for (i in (punto_base - 1):1) {
      if (indice_encadenado$mes[i + 1] == 1) {
        # Si es diciembre (el siguiente es enero)
        factor_variacion <- indice_encadenado$valor[i + 1] / 100
        indice_encadenado$valor_encadenado[i] <- indice_encadenado$valor_encadenado[i + 1] / factor_variacion
      } else {
        # Mes normal
        factor_variacion <- indice_encadenado$valor[i + 1] / indice_encadenado$valor[i]
        indice_encadenado$valor_encadenado[i] <- indice_encadenado$valor_encadenado[i + 1] / factor_variacion
      }
    }
  }
}

# Calcular tasas de variación del nuevo IPC (Cosas secundarias y adicionales)
indice_final <- indice_encadenado %>%
  arrange(anio, mes) %>%
  mutate(
# Variación mensual
    tasa_mensual = (valor_encadenado / lag(valor_encadenado, 1) - 1) * 100,
# Variación interanual
    tasa_interanual = (valor_encadenado / lag(valor_encadenado, 12) - 1) * 100,
# Crear fecha para gráficos
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  )



###################### Verificación de coherencia #####

indice_check <- indice_encadenado %>%
  arrange(anio, mes) %>%
  mutate(
# 1. Valor encadenado de diciembre del año anterior
    valor_dic_ant = ifelse(mes == 1, lag(valor_encadenado), NA_real_),

# 2. Fórmula estándar:
    valor_encadenado_teorico = ifelse(
      mes == 1 & !is.na(valor_dic_ant),
      valor * valor_dic_ant / 100,
      NA_real_
    )
  )

# 3. Comparar tu cálculo vs el teórico en eneros
comparacion_eneros <- indice_check %>%
  filter(mes == 1 & !is.na(valor_encadenado_teorico)) %>%
  mutate(
    diff_abs = valor_encadenado - valor_encadenado_teorico,
    diff_rel = 100 * diff_abs / valor_encadenado_teorico
  )

# 4. Resumen
resumen <- comparacion_eneros %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE)
  )

print(resumen)

# Comprobación detallada del encadenado
indice_check_full <- indice_encadenado %>%
  arrange(anio, mes) %>%
  mutate(
# Valor encadenado teórico:
    valor_encadenado_teorico = case_when(
# Primer punto base: NA
      row_number() == 1 ~ NA_real_,

# Si es enero usar diciembre del año anterior
      mes == 1 ~ valor * lag(valor_encadenado) / 100,

# Si no es enero  usar el mes anterior
      TRUE ~ lag(valor_encadenado) * (valor / lag(valor))
    ),

# Diferencias
    diff_abs = valor_encadenado - valor_encadenado_teorico,
    diff_rel = 100 * diff_abs / valor_encadenado_teorico
  )

# Resumen de discrepancias
resumen_full <- indice_check_full %>%
  summarise(
    max_diff_abs = max(abs(diff_abs), na.rm = TRUE),
    max_diff_rel = max(abs(diff_rel), na.rm = TRUE)
  )

print(resumen_full)


########### Guardar datos ####

# Obtener el IPC orginal para comparar
# Filtrar la fila "Índice general"
ipc_oficial <- ipc_original %>%
  filter(`...1` == "Índice general")

ipc_oficial_largo <- ipc_oficial %>%
  pivot_longer(
    cols = -`...1`, # Mantener la primera columna para nombres
    names_to = c("anio", "mes"),
    names_sep = "M", # Dependiendo de cómo estén nombradas las columnas (ej: 2021M01)
    values_to = "ipc_oficial"
  ) %>%
  mutate(
    anio = as.numeric(anio),
    mes = as.numeric(mes)
  ) %>%
  select(-`...1`) %>% # Eliminar columna de nombre
filter(anio > 2018 & anio < 2026) # Filtrar años

# Pasar a base que me interesa
ipc_base <- ipc_oficial_largo %>%
  filter(anio == anio_base, mes == mes_base) %>%
  pull(ipc_oficial)

# Creo nueva columna con base seleccionada = 100
ipc_oficial_largo <- ipc_oficial_largo %>%
  rename(
    ipc_oficial_prev = ipc_oficial
  ) %>%
  mutate(ipc_oficial = (ipc_oficial_prev / ipc_base) * 100) %>%
  select(
    - ipc_oficial_prev
  )


# Junto indices y limpio tabla
ipc_inquilinos <- indice_final %>%
  select(
    -Componente, - valor, - es_base, - tasa_mensual, - tasa_interanual
  ) %>%
  rename(ipc_inquilinos = valor_encadenado) %>%
  filter(anio > 2018 & anio < 2026) %>% # Filtrar años
left_join(ipc_oficial_largo, by = c("anio", "mes"))

# Guardo tabla
saveRDS(ipc_inquilinos,
        file = file.path(ruta_resultados, "ipc_inquilinos_estatal.rds"))
write_xlsx(ipc_inquilinos,
           path = file.path(ruta_resultados, "ipc_inquilinos_estatal.xlsx"))


##### CALCULO MEDIAS ANUALES #####
# Tengo que tener datos de 2018 para ambos indices

