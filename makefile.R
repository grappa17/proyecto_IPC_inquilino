# ==============================================================================
# SCRIPT MAESTRO - Proyecto reponderacionIPC
# ==============================================================================

# Este script simplemente corre el resto de scripts

# Limpiar entorno
rm(list = ls())

# Lista de scripts
scripts <- c(
    "1_Calculo ponderaciones alquiler a partir de EPF.R",
    "2_Calculo ponderaciones subgrupos a partir de nuevo peso alquiler.R",
    "3_Carga de ponderaciones y nuevas agrupaciones.R",
    "4_Calculo IPC alternativo estatal.R",
    "5_Calculo IPC inquilinos ccaa.R",
    "6_Graficos.R",
    "7_Calculo de salarios en base a IPC inquilinos.R"
)

# Ejecutar todos los scripts
print("Iniciando ejecución de scripts...\n\n")

for (script in scripts) {
    cat("Ejecutando:", script, "\n")
    source(script, encoding = "UTF-8")
    cat("Completado\n\n")
}

print("¡Todos los scripts ejecutados!\n")
