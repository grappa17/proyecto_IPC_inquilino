# install_requirements.R
# Script para instalar todas las dependencias necesarias para reproducir el IPC Inquilino

paquetes_necesarios <- c(
  "data.table", # Crucial para la manipulación ultra rápida y lectura de los microarchivos pesados de la EPF
  "survey",     # Esencial para el diseño muestral complejo (svydesign) y el cálculo riguroso de varianzas
  "dplyr",      # Para la limpieza y manipulación general de datos
  "magrittr",   # Para la utilización de pipes (%>%)
  "readxl",     # Para importar las tablas originales del IPC e Idealista provistas en Excel
  "writexl",    # Para la exportación de resultados intermedios a formato Excel
  "ggplot2"     # Para la generación de gráficos de series temporales (Script 6 y 7)
)

# Instalar los paquetes que no estén actualmente en el entorno
paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]

if(length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, dependencies = TRUE)
} else {
  message("Todos los paquetes necesarios ya están instalados.")
}
