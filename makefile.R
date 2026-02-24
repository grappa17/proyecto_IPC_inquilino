# ============================================================================== # 
# SCRIPT MAESTRO - Proyecto reponderacionIPC
# ============================================================================== #


# Este script ejecuta la cadena de scripts del proyecto y fija los parametros comunes

# Limpiar entorno
rm(list = ls())


# PARAMETROS GLOBALES DEL PROYECTO
# Seleccion de periodo base para indices
anio_base <- 2019
mes_base <- 1


# Rango de años a analizar
anios <- 2019:2025

# Colores de graficos:
c_inquilinos <- "#FF0031"
c_oficial <- "#1F78B4"



# ============================================================================== #
# EJECUCION DE SCRIPTS
# ============================================================================== #

# Lista de scripts
scripts <- c(
  "1_Calculo ponderaciones alquiler a partir de EPF.R",
  "2_Calculo ponderaciones subgrupos a partir de nuevo peso alquiler.R",
  "3_Carga de ponderaciones y nuevas agrupaciones.R",
  "4_Calculo IPC alternativo estatal.R",
  "5_Calculo IPC inquilinos ccaa.R",
  "6_Graficos.R"
)

# Ejecutar todos los scripts
print("Iniciando ejecucion de scripts...")
for (script in scripts) {
  print(paste("Ejecutando:", script))
  source(script)
  print(" Completado")
}
print("¡Todos los scripts ejecutados!")