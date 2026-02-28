

                                              ## GRAFICANDO RESULTADOS ##
                                              ## GRAFICANDO RESULTADOS ##
                                              ## GRAFICANDO RESULTADOS ##
                                              ## GRAFICANDO RESULTADOS ##


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
ruta_graficos <- file.path(base_dir, "GRAFICOS")

if (!dir.exists(ruta_graficos)) {
  dir.create(ruta_graficos, recursive = TRUE)
}


####### Graficos estatales #######
# IPC inquilinos estatal

# Carga datos
ipc_inquilinos <- readRDS(
  file = file.path(ruta_resultados, "ipc_inquilinos_estatal.rds")
)

# Pivotamos a formato largo
ipc_plot <- ipc_inquilinos %>%
  pivot_longer(
    cols = c(ipc_inquilinos, ipc_oficial),
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(tipo, 
                  ipc_inquilinos = "IPC Inquilinos",
                  ipc_oficial = "IPC Oficial"),
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  )

# 2 Gráfico
grafico_1 <- ggplot(ipc_plot, aes(x = fecha, y = valor, color = tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(c_inquilinos, c_oficial)) +
  scale_x_date(
    date_breaks = "1 year",   # marcas cada año
    date_labels = "%Y",       # etiquetas como 2021, 2022, ...
    expand = expansion(mult = c(0, 0.02)) # evita márgenes extra
  ) +
  scale_y_continuous(
    breaks = seq(80, 130, 5) # aquí ajusta el rango según tus datos
  ) +
  labs(
    title = "Comparación IPC Inquilinos vs IPC Oficial",
    x = "Fecha",
    y = "Índice de Precios (Base 2021)",
    color = "Serie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(caption = "Fuente: Elaboración propia con datos del IPC, Idealista (precios del alquiler) \ny la Encuesta de Condiciones de Vida (ponderaciones del alquiler). Autor: Gabinete Socioeconómico CGT (@CGT_economia).")

ggsave(
  filename = file.path(ruta_graficos, "grafico_IPC_inquilinos.jpeg"),
  plot = grafico_1,
  bg = "transparent",
  width = 18,
  height = 16,
  units = "cm",
  dpi = 300  # Calidad alta
)

###### GRAFICO CCAA #######

# Cargo datos y paso a formato largo

ipc_inquilinos_ccaa <- readRDS(
  file = file.path(ruta_resultados, "ipc_inquilinos_ccaa.rds")
)
  
ipc_plot_ccaa <- ipc_inquilinos_ccaa %>%
  pivot_longer(
    cols = c(ipc_inquilinos, ipc_oficial),
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(tipo, 
                  ipc_inquilinos = "IPC Inquilinos",
                  ipc_oficial = "IPC Oficial")
  )

# Crear el gráfico con facetas por CCAA
grafico_ipc_ccaa <- ggplot(ipc_plot_ccaa, aes(x = fecha, y = valor, color = tipo)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  facet_wrap(~CCAA, scales = "fixed", ncol = 3) +
  
  # Colores diferenciados
  scale_color_manual(
    values = c("IPC Inquilinos" = c_inquilinos, "IPC Oficial" = c_oficial),
    name = "Tipo de IPC"
  ) +
  
  # Escalas y formato
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x),
    expand = c(0.02, 0)
  ) +
  
  # Tema y estética
  theme_minimal() +
  theme(
    # Títulos y etiquetas
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    
    # Facetas
    strip.text = element_text(size = 10, face = "bold", margin = margin(5, 0, 5, 0)),
    strip.background = element_rect(fill = "gray95", color = NA),
    
    # Ejes
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(t = 15),
    
    # Panel y grilla
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.2),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
    
    # Espaciado general
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Títulos
  labs(
    title = "Comparación IPC Oficial vs IPC Inquilinos por Comunidad Autónoma",
    subtitle = "Evolución temporal del Índice de Precios al Consumo (Base: 2021 = 100)",
    x = "Fecha",
    y = "Índice (Base 100)",
    caption = "Fuente: Elaboración propia a partir de los datos del IPC, la Encuesta de Presupuestos Familiares y los precios de alquileres del portal Idealista.\nGabinete socioeconómico de CGT (@CGTsocioeconomico)"
  )

# Mostrar el gráfico
print(grafico_ipc_ccaa)

# Guardar en alta resolución
ggsave(
  file.path(ruta_graficos, "comparacion_ipc_ccaa.png"),
  plot = grafico_ipc_ccaa,
  width = 8.27, 
  height = 11.69, 
  dpi = 300, 
  bg = "white"
)
