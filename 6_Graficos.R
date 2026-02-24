

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
library(showtext)

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

# Fuente 
font_add("Gotham", 
         regular = "C:/Users/lorie/Desktop/TRABAJO/CGT/Recursos/Recursos/CGT/Gotham Font Family/Gotham-font-family/Gotham/Gotham-Book.otf")
showtext_auto()  # Activar showtext globalmente


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

# 2 Grafico
grafico_1 <- ggplot(ipc_plot, aes(x = fecha, y = valor, color = tipo)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(c_inquilinos, c_oficial)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(80, 130, 5)
  ) +
  labs(
    x = "Fecha",
    y = "Indice de Precios (Base 2021)",
    color = "Serie"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "Gotham", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Gotham", size = 12),
    axis.text.y = element_text(family = "Gotham", size = 12),
    axis.title = element_text(family = "Gotham", size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12, hjust = 0.5, family = "Gotham"),
    legend.text = element_text(size = 12, family = "Gotham")
  )

print(grafico_1)
showtext_opts(dpi = 300)
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
  geom_line(linewidth = 1.2) +
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
    
    # Facetas
    strip.text = element_text(family = "Gotham", size = 10, face = "bold", margin = margin(5, 0, 5, 0)),
    strip.background = element_rect(fill = "gray95", color = NA),
    
    # Ejes
    axis.title.x = element_text(family = "Gotham", size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Gotham", size = 10, margin = margin(r = 10)),
    axis.text.x = element_text(family = "Gotham", size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(family = "Gotham", size = 10),
    
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(family = "Gotham", size = 12, face = "bold"),
    legend.text = element_text(family = "Gotham", size = 12),
    legend.margin = margin(t = 15),
    
    # Panel y grilla
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.2),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
    
    # Espaciado general
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Títulos
  labs(
    x = "Fecha",
    y = "Índice (Base 100)"
  )

# Mostrar el gráfico
print(grafico_ipc_ccaa)

# Guardar en alta resolución
showtext_opts(dpi = 300)
ggsave(
  file.path(ruta_graficos, "comparacion_ipc_ccaa.png"),
  plot = grafico_ipc_ccaa,
  width = 8.27, 
  height = 11.69, 
  dpi = 300, 
  bg = "white"
)

