                  
                  
                  # CALCULO DE SALARIOS EN BASE A IPC ALTERNATIVO PARA INQUILINOS #
                  # CALCULO DE SALARIOS EN BASE A IPC ALTERNATIVO PARA INQUILINOS #
                  # CALCULO DE SALARIOS EB BASE A IPC ALTERNATIVO PARA INQUILINOS #
                  # CALCULO DE SALARIOS EB BASE A IPC ALTERNATIVO PARA INQUILINOS #


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
ruta_resultados <- file.path(base_dir, "DATOS/RESULTADOS")
ruta_salarios <- file.path(base_dir, "DATOS/SALARIOS")
ruta_graficos <- file.path(base_dir, "GRAFICOS")

##### CALCULO SALARIOS REALES #####
# Cargo datos de salarios
salarios <- read_xlsx(
  path = file.path(ruta_salarios, "coste_laboral_por_hora_efectiva_ccaa.xlsx")
) %>%
  pivot_longer(
    cols = -1,
    names_to = "trimestre",
    values_to = "salario"
  ) %>%
  rename(CCAA = ...1) %>%
  separate(trimestre, into = c("anio", "trimestre"), sep = "T") %>%
  filter(anio %in% anios) %>%
  group_by(CCAA, anio) %>%
  summarise(
    salario = mean(salario, na.rm = TRUE),
    .groups = 'drop'
  )

# Distingo estatales y ccaa
salarios_ccaa <- salarios[salarios$CCAA != "Total Nacional", ]

salarios_estatal <- salarios[salarios$CCAA == "Total Nacional", ]


# Cargo datos de IPC
deflactores_ccaa <- readRDS(
  file = file.path(ruta_resultados, "ipc_inquilinos_ccaa.rds")
) 

deflactores_estatal <- readRDS(
  file = file.path(ruta_resultados, "ipc_inquilinos_estatal.rds")
) 

# Convierto los datos mensuales de IPC en datos anuales haciendo la media anual
deflactores_anual_ccaa <- deflactores_ccaa %>%
  group_by(anio, CCAA) %>%
  summarise(
    ipc_inquilinos = mean(ipc_inquilinos, na.rm = TRUE),
    ipc_oficial = mean(ipc_oficial, na.rm = TRUE),
    .groups = 'drop'
  )

deflactores_anual_estatal <- deflactores_estatal %>%
  group_by(anio) %>%
  summarise(
    ipc_inquilinos = mean(ipc_inquilinos, na.rm = TRUE),
    ipc_oficial = mean(ipc_oficial, na.rm = TRUE),
    .groups = 'drop'
  )

# Guardo para poder calcular a mano salarios en base a datos de estadisticas CCT
write_xlsx(deflactores_anual_estatal,
           path = file.path(ruta_resultados, "ipc_anual_estatal.xlsx"))

# Convierto anio a numerico en los data frames
salarios_ccaa <- salarios_ccaa %>%
  mutate(anio = as.numeric(anio))

salarios_estatal <- salarios_estatal %>%
  mutate(anio = as.numeric(anio))

deflactores_anual_ccaa <- deflactores_anual_ccaa %>%
  mutate(anio = as.numeric(anio))

deflactores_anual_estatal <- deflactores_anual_estatal %>%
  mutate(anio = as.numeric(anio))


# Uno dfs
salarios_estatal <- salarios_estatal %>%
  left_join(
    deflactores_anual_estatal, by = c("anio")
  )

salarios_ccaa <- salarios_ccaa %>%
  left_join(
    deflactores_anual_ccaa, by = c("anio", "CCAA")
  )


# Calculo salarios reales
salarios_estatal <- salarios_estatal %>%
  mutate(
    salario_real_oficial = salario / (ipc_oficial / 100),
    salario_real_inquilinos = salario / (ipc_inquilinos / 100)
  )

resultados_estatal <- salarios_estatal %>%
  select(
    -salario,
    -ipc_oficial,
    -ipc_inquilinos,
    -CCAA
  )

salarios_ccaa <- salarios_ccaa %>%
  mutate(
    salario_real_oficial = salario / (ipc_oficial / 100),
    salario_real_inquilinos = salario / (ipc_inquilinos / 100)
  )

resultados_ccaa <- salarios_ccaa %>%
  select(
    -salario,
    -ipc_oficial,
    -ipc_inquilinos
  )

# Pasar a numeros indice
# Estatal
# Obtengo los valores base
resultados_estatal <- resultados_estatal %>%
  mutate(
    salario_real_oficial_base = (salario_real_oficial / salario_real_oficial[anio == anio_base]) * 100,
    salario_real_inquilinos_base = (salario_real_inquilinos / salario_real_inquilinos[anio == anio_base]) * 100
  )

# CCAA
# Obtengo los valores base para cada CCAA
salarios_base_ccaa <- resultados_ccaa %>%
  filter(anio == anio_base) %>%
  select(CCAA, salario_real_oficial, salario_real_inquilinos) %>%
  rename(
    salario_oficial_base = salario_real_oficial,
    salario_inquilinos_base = salario_real_inquilinos
  )

# Creo indices con base seleccionada = 100 para cada CCAA
resultados_ccaa <- resultados_ccaa %>%
  left_join(salarios_base_ccaa, by = "CCAA") %>%
  mutate(
    salario_real_oficial_base = (salario_real_oficial / salario_oficial_base) * 100,
    salario_real_inquilinos_base = (salario_real_inquilinos / salario_inquilinos_base) * 100
  ) %>%
  select(
    -salario_oficial_base,
    -salario_inquilinos_base
  )

# guardo resultados
write_xlsx(
  resultados_estatal,
  path = file.path(ruta_resultados, "salario_real_inquilinos_y_oficial_estatal.xlsx")
)

resultados_ancho_inquilinos_ccaa <- resultados_ccaa %>% # Aquí estoy guardando solo los datos normales, sin indice
  select(
    -salario_real_oficial,
    -salario_real_oficial_base, 
    -salario_real_inquilinos_base 
  ) %>%
  pivot_wider(
    names_from = anio,
    values_from = c(salario_real_inquilinos)
  )

resultados_ancho_oficial_ccaa <- resultados_ccaa %>%
  select(
    -salario_real_inquilinos,
    -salario_real_inquilinos_base,
    -salario_real_oficial_base
  ) %>%
  pivot_wider(
    names_from = anio,
    values_from = c(salario_real_oficial)
  )
  
write_xlsx(
  resultados_ancho_inquilinos_ccaa,
  path = file.path(ruta_resultados, "salario_real_inquilinos_ccaa.xlsx")
)

write_xlsx(
  resultados_ancho_oficial_ccaa,
  path = file.path(ruta_resultados, "salario_real_oficial_ccaa.xlsx")
)




###### GRAFICAR ######

# ESTATAL
datos_plot_1 <- resultados_estatal %>%
  select(
    -salario_real_oficial,
    -salario_real_inquilinos
  ) %>%
  pivot_longer(
    cols = c(salario_real_oficial_base, salario_real_inquilinos_base),
    names_to = "tipo_salario",
    values_to = "salario"
  ) %>%
  mutate(
    tipo_salario = case_when(
      tipo_salario == "salario_real_oficial_base" ~ "Oficial",
      tipo_salario == "salario_real_inquilinos_base" ~ "Inquilinos"
    )
  )

# Grafico
grafico_salarios_estatal <- ggplot(datos_plot_1, aes(x = anio, y = salario, color = tipo_salario, group = tipo_salario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Oficial" = c_oficial, "Inquilinos" = c_inquilinos),
    name = "Tipo de IPC"
  ) +
  labs(
    title = "Salario por hora real deflactado con distintos índices de precios",
    subtitle = "Coste salarial por hora efectiva, Estado español",
    x = "Año",
    y = "Salario real por hora",
    color = "Serie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(caption = "Fuente: Elaboración propia con datos de la ETCL, el IPC, Idealista (precios del alquiler) \ny la Encuesta de Condiciones de Vida (ponderaciones del alquiler). Autor: Gabinete Socioeconómico CGT (@CGT_economia).")

grafico_salarios_estatal

# Guardar
ggsave(
  file.path(ruta_graficos, "comparacion_salarios_estatal.png"),
  plot = grafico_salarios_estatal,
  width = 8.27, 
  height = 11.69, 
  dpi = 300, 
  bg = "white"
)





# CCAA
# Filtrar Total Nacional y preparar datos
datos_plot_2 <- resultados_ccaa %>%
  filter(CCAA != "Total Nacional") %>%
  select(
    -salario_real_oficial,
    -salario_real_inquilinos
  ) %>%
  pivot_longer(
    cols = c(salario_real_oficial_base, salario_real_inquilinos_base),
    names_to = "tipo_salario",
    values_to = "salario"
  ) %>%
  mutate(
    tipo_salario = case_when(
      tipo_salario == "salario_real_oficial_base" ~ "Oficial",
      tipo_salario == "salario_real_inquilinos_base" ~ "Inquilinos"
    )
  )

# Crear gráfico con facetas
grafico_salarios_ccaa <- ggplot(datos_plot_2, aes(x = anio, y = salario, color = tipo_salario, group = tipo_salario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ CCAA, ncol = 3, scales = "free_y") +
  scale_color_manual(
    values = c("Oficial" = c_oficial, "Inquilinos" = c_inquilinos),
    name = "Tipo de salario"
  ) +
  labs(
    title = "Evolución del salario real por Comunidad Autónoma",
    subtitle = "Comparación entre salario oficial y salario de inquilinos (2019-2025)",
    x = "Año",
    y = "Salario real (€ por hora)",
    caption = "Fuente: Datos propios"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

grafico_salarios_ccaa


# Guardar
ggsave(
  file.path(ruta_graficos, "comparacion_salarios_ccaa.png"),
  plot = grafico_salarios_ccaa,
  width = 8.27, 
  height = 11.69, 
  dpi = 300, 
  bg = "white"
)









