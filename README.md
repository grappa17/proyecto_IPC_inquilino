# GUÍA DEL SCRIPT PARA CONSTRUIR UN IPC DE INQUILINOS

## 0. Materiales y preparaciones previas

Subidos a github están todos los archivos necesarios para correr el script, menos los microdatos de la EPF, que pesan demasiado, y están subidos a drive en el siguiente link: https://drive.google.com/drive/folders/1BDpfJDn8N_Kk75EYfv6zBh-P8h1EUAs2?usp=drive_link. Para que funcione bien el script hay que meterlos en la carpeta `DATOS/EPF/` dentro de la carpeta del proyecto.

Hay 8 scripts, numerados por orden excepto "makefile". "makefile" corre el resto de scripts.

Los scripts empiezan definiendo los años que se quieren obtener, en un vector a partir del cual se derivan otros. También se muestra el directorio con las rutas que utiliza el script.

En general, el proceso del proyecto es el siguiente:

- Calcular, a partir de los microdatos de la EPF, el peso del gasto en alquiler de vivienda sobre el total de gasto para cada año, a nivel estatal y por CCAA.
- Calcular nuevas ponderaciones del conjunto de subgrupos que conforman el IPC general a partir del nuevo peso del alquiler, a nivel estatal y por CCAA.
- Cargar las nuevas ponderaciones, y unir varios subgrupos que en la forma más actualizada de proporcionar los datos del IPC ya no aparecen, para que las ponderaciones correspondan con los datos de precios que proporciona el INE.
- A partir de esas ponderaciones, recalcular el IPC para inquilinos a nivel estatal.
- A partir de esas ponderaciones, recalcular el IPC para inquilinos a nivel CCAA.
- Hacer gráficos.
- Calcular salarios con nuevos IPC.

## 0. Correr el resto de scripts.

## 1. Calcular ponderaciones del alquiler

Porcentaje del gasto dedicado al pago del alquiler por parte de la población inquilina.

Esto se realiza a partir de los microdatos de la Encuesta de Presupuestos Familiares (EPF). Calculo el peso del alquiler respecto al total del consumo sin alquiler imputado de cada hogar, a nivel estatal y por CCAA, con valores para cada año. Es importante que el año de la ponderación luego se "retrasa", pues el IPC utiliza las ponderaciones del año anterior. (En realidad utiliza las del año N-2, pero actualizándolas con información que no hace pública, por lo que la mejor aproximación es utilizar el año N-1, aunque eso implica que durante unos meses hasta que se publica la EPF del año N-1 no se puede actualizar el índice en tiempo real.

## 2. Calcular conjunto de ponderaciones a partir de las nuevas ponderaciones del alquiler

Después de haber obtenido las ponderaciones del alquiler de vivienda, es necesario reponderar el resto de componentes, para que el total de ponderaciones siga siendo 1000. Idealmente podría utilizar todas las ponderaciones de la EPF para inquilinos para que fuese más fino, pero no lo he hecho así y es bastante más trabajo. De modo que a partir de los nuevos pesos del gasto en alquiler, se recalculan manteniendo su peso proporcional el resto de componentes. Para cada año se exporta un nuevo archivo de ponderaciones.

## 3. Cargar los datos, unirlos y generar nuevas agrupaciones de algunas ponderaciones

Cargamos los archivos uniéndolos en un único dataframe. En el dataframe resultante, hay que eliminar algún componente sumando su valor a otros, para uniformarlos y que coincidan perfectamente con los del IPC.

Este proceso se repite con los datos de CCAAs.

## 4. Cálculo del nuevo IPC con las nuevas ponderaciones para el IPC estatal

Cargamos los datos del IPC, previamente preparados en un excel, y lo primero que hay que hacer es desencadenarlos, dado que el INE los proporciona siguiendo la metodología de Laspeyres encadenado. Una vez desencadenados, es posible trabajar con los valores de los componentes.

Así, lo siguiente que hay que hacer es eliminar los valores antiguos del alquiler en los datos originales y unir mis nuevos valores de alquiler, procedentes del portal Idealista. La cuestión de la fuente de datos es un asunto metodológico complicado, porque idealista tiene a sobreestimar los incrementos de precios al reflejar solo los precios de las viviendas que están en el mercado. Pero la alternativa era utilizar el propio indicador de precios del IPC, que, comparado con las fuentes más fiables (como el IPVA, que tiene el problema de ser anual) tiene el problema contrario, infraestima los incrementos. Los datos de idealista los he "desencadenado" manualmente en el propio Excel, poniendo cada año en el mismo formato que el IPC, es decir en base 100 con referencia a diciembre del año anterior.

Una vez unido el componente del alquiler, hay que reponderar el índice desencadenado, recalculando el índice general a partir de sus componentes.

Por último, se vuelve a encadenar el índice general, para tenerlo en el mismo formato que el oficial. En este caso utilizamos una base enero 2021 = 100, aunque no tengo claro que mes utiliza como base o como trabaja eso el índice oficial, más alla de que su año base es también el 2021.

Las comprobaciones finales permiten comprobar la coherencia entre el índice encadenado y el índice sin encadenar.

## 5. Cálculo del nuevo IPC con las nuevas ponderaciones para el IPC por CCAA

Se repite el mismo proceso pero un poco más complejo, con los datos de todas las CCAAs. Aquí hay una diferencia importante, y es que los datos de alquiler de idealista no los he "desencadenado" manualmente antes de usarlos, sino que los he descargado y ordenado tal cual aparecen en la web, y en el propio script los coloco en base 100 con referencia a diciembre anterior antes de mezclarlos con el resto de datos del IPC.

## 6. Graficar

Por último, graficamos los resultados y los guardamos en la carpeta de graficos nuestro proyecto, comparando el nuevo índice de inquilinos con el índice general del INE.

## 7. Comparar salarios

Se comparan salarios deflactados con el IPC oficial con salarios deflactados con el IPC de inquilinos para cada CCAA. El grafico se guarda en la carperta de gráficos.
