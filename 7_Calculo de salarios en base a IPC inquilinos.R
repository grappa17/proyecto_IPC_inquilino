                  
                  
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

# AÑOS A OBTENER (SOLO TOCAR ESTO)
anios <- 2019:2025

anios_ponderaciones <- (min(anios) - 2):(max(anios) - 1) # Para los años que quiero obtener, necesito años previos de ponderaciones
anios_ponderaciones_IPC <- (min(anios) - 1):(max(anios)) # Para los años que quiero obtener, necesito años previos de ponderaciones


# DIRECTORIO

# Directorio base
base_dir <- getwd()

# Resto rutas
ruta_resultados <- file.path(base_dir, "DATOS/RESULTADOS")
ruta_salarios <- file.path(base_dir, "DATOS/SALARIOS")
ruta_graficos <- file.path(base_dir, "GRAFICOS")

##### CALCULO SALARIOS REALES 
# Cargo datos
salarios_ccaa <- read_xlsx(
  path = file.path(ruta_salarios, "coste_laboral_por_hora_efectiva_ccaa.xlsx")
) %>%
  pivot_longer(
    cols = -1,  # todas las columnas excepto ...1 (CCAA)
    names_to = "trimestre",
    values_to = "salario"
  ) %>%
  rename(CCAA = ...1) %>%
  separate(trimestre, into = c("anio", "trimestre"), sep = "T") %>%
  filter(anio %in% anios)
  

deflactores <- readRDS(
  file = file.path(ruta_resultados, "ipc_inquilinos_ccaa.rds")
) 

# Convierto los datos mensuales de IPC en datos trimestrales haciendo la media trimestral
deflactores_trimestral <- deflactores %>%
  mutate(trimestre = paste0(anio, "T", ceiling(mes / 3))) %>%
  group_by(trimestre, ) %>%
  summarise(
    ipc_inquilinos = mean(ipc_inquilinos, na.rm = TRUE),
    ipc_oficial = mean(ipc_oficial, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  separate(trimestre, into = c("anio", "trimestre"), sep = "T")

# Uno dfs
salarios_ccaa <- salarios_ccaa %>%
  left_join(
    deflactores_trimestral, by = c("anio", "trimestre")
  )

# Calculo salarios reales
salarios_ccaa <- salarios_ccaa %>%
  mutate(
    salario_real_oficial = salario / (ipc_oficial / 100),
    salario_real_inquilinos = salario / (ipc_inquilinos / 100)
  )

resultados <- salarios_ccaa %>%
  select(
    -salario,
    -ipc_oficial,
    -ipc_inquilinos
  )

# guardo resultados
resultados_ancho_inquilinos <- resultados %>%
  mutate(trimestre = paste0(anio, "T", trimestre)) %>%
  select(
    -anio,
    -salario_real_oficial
  ) %>%
  pivot_wider(
    names_from = trimestre,
    values_from = c(salario_real_inquilinos)
  )

resultados_ancho_oficial <- resultados %>%
  mutate(trimestre = paste0(anio, "T", trimestre)) %>%
  select(
    -anio,
    -salario_real_inquilinos
  ) %>%
  pivot_wider(
    names_from = trimestre,
    values_from = c(salario_real_oficial)
  )
  
write_xlsx(
  resultados_ancho_inquilinos,
  path = file.path(ruta_resultados, "salario_real_inquilinos.xlsx_ccaa")
)

write_xlsx(
  resultados_ancho_oficial,
  path = file.path(ruta_resultados, "salario_real_oficial_ccaa.xlsx")
)

# Graficar
# Filtrar Total Nacional y preparar datos
datos_plot <- resultados %>%
  filter(CCAA != "Total Nacional") %>%
  # Crear variable temporal continua
  mutate(
    periodo = as.numeric(anio) + (as.numeric(trimestre) - 1) / 4
  ) %>%
  # Convertir a formato largo para ggplot
  pivot_longer(
    cols = c(salario_real_oficial, salario_real_inquilinos),
    names_to = "tipo_salario",
    values_to = "salario"
  ) %>%
  mutate(
    tipo_salario = case_when(
      tipo_salario == "salario_real_oficial" ~ "Oficial",
      tipo_salario == "salario_real_inquilinos" ~ "Inquilinos"
    )
  )

# Crear gráfico con facetas
grafico_salarios <- ggplot(datos_plot, aes(x = periodo, y = salario, color = tipo_salario, group = tipo_salario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ CCAA, ncol = 3, scales = "free_y") +
  scale_color_manual(
    values = c("Oficial" = "#2E86AB", "Inquilinos" = "#A23B72"),
    name = "Tipo de salario"
  ) +
  labs(
    title = "Evolución del salario real por Comunidad Autónoma",
    subtitle = "Comparación entre salario oficial y salario de inquilinos (2019-2025)",
    x = "Año",
    y = "Salario real (miles €)",
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

# Guardar
ggsave(
  file.path(ruta_graficos, "comparacion_salarios_ccaa.png"),
  plot = grafico_salarios,
  width = 8.27, 
  height = 11.69, 
  dpi = 300, 
  bg = "white"
)









