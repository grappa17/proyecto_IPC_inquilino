# Proyecto IPC Inquilino: Midiendo la inflación real de los hogares de alquiler

Este repositorio contiene el código y las bases de datos necesarias para reproducir el cálculo del "IPC Inquilino". Este índice alternativo mide la evolución real de los precios y la pérdida de poder adquisitivo de los hogares que viven de alquiler a precio de mercado. Este grupo representa el 17% de los hogares en el Estado español, sumando alrededor de 3,3 millones de hogares en los que viven 8,4 millones de personas. 

El proyecto y la investigación original han sido desarrollados por el Gabinete Socioeconómico Confederal de CGT (Autores: Lorién Cirera Sancho y Miguel García-Duch).

## Metodología y Fuentes de Datos

El índice se construye replicando la metodología del IPC oficial (Laspeyres encadenado), pero solucionando la infrarrepresentación del gasto en alquiler en el índice general del INE. Para ello, utiliza:
1. **Ponderaciones del Alquiler (EPF):** Se extraen de los microdatos de la Encuesta de Presupuestos Familiares del INE. A diferencia del IPC oficial, las ponderaciones se calculan filtrando estrictamente los hogares en régimen de alquiler a precio de mercado e indexando el peso del alquiler sobre el Gasto Monetario total del hogar.
2. **Evolución de Precios (Idealista):** Ante la subestimación de precios por parte del índice de alquileres del IPC oficial (que sólo recoge una subida acumulada del 5,2% entre 2019 y 2023), se utilizan los precios mensuales del portal Idealista para el componente de vivienda.
3. **Resto de la Cesta (INE):** Se utilizan los microdatos mensuales del IPC oficial para el resto de los subgrupos de consumo.

## Estructura de Datos

Todos los datos necesarios para reproducir el proyecto ya se encuentran incluidos en este repositorio. Los microdatos de la EPF (históricos y actualizados) se localizan en la carpeta `EPF_NUEVO/`. Han sido optimizados y comprimidos en formato `.gz` para sortear los límites de tamaño de GitHub, permitiendo a su vez una lectura ultra rápida con librerías como `data.table`. No es necesaria ninguna descarga externa.

## Flujo de Trabajo e Instrucciones de Ejecución

El proyecto está estructurado en una tubería (*pipeline*) de scripts de R. Puedes ejecutar el archivo `makefile.R` para correr todo el proceso secuencialmente, o bien ejecutar los scripts individuales en el siguiente orden:

* **`1_Calculo ponderaciones alquiler a partir de EPF.R`**: Lee los microarchivos comprimidos directamente desde la carpeta `EPF_NUEVO/` y calcula el peso real del alquiler respecto al gasto monetario para los inquilinos de libre mercado a nivel estatal y por CCAA. Se emplea la librería `survey` para respetar el diseño muestral complejo de la encuesta oficial y obtener estimaciones precisas.
* **`2_Calculo ponderaciones subgrupos a partir de nuevo peso alquiler.R`**: Recalcula el peso proporcional de los demás componentes de la cesta del IPC para que el total siga sumando 1000, integrando la nueva ponderación mayoritaria del alquiler.
* **`3_Carga de ponderaciones y nuevas agrupaciones.R`**: Unifica los datos y agrupa los subgrupos en categorías coherentes con los datos mensuales de precios publicados por el INE en la actualidad.
* **`4_Calculo IPC alternativo estatal.R`**: Desencadena el IPC base oficial, sustituye el componente 041 por los datos de Idealista, aplica las nuevas ponderaciones calculadas y vuelve a encadenar el índice final a nivel estatal.
* **`5_Calculo IPC inquilinos ccaa.R`**: Replica el proceso del paso 4 desagregado para cada Comunidad Autónoma.
* **`6_Graficos.R`**: Genera gráficos comparativos visuales entre el crecimiento del IPC General y el IPC Inquilino.
* **`7_Calculo de salarios en base a IPC inquilinos.R`**: Deflacta los salarios medios por hora utilizando el nuevo IPC Inquilino para evidenciar la pérdida real de poder adquisitivo, comparándola con los resultados que arroja el IPC oficial.

## Licencia y Contacto

**Licencia:** Este proyecto es de código abierto y se distribuye bajo la licencia **GNU General Public License v3.0 (GPL-3.0) Share Alike**. Eres libre de usar, modificar y distribuir este software, siempre y cuando cualquier obra derivada también se distribuya bajo esta misma licencia y mantenga su carácter abierto.

Para cualquier duda metodológica o consulta sobre la investigación, puedes escribir a: econlab@econlab.info.
