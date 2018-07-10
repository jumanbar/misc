# coding=utf-8

# Ejemplo de dato diario en la web del INIA:
# http://www.inia.org.uy/gras/computo/gras_planilla_variables_visualizar_diarias.php?id=80321

# Al final de la URL hay un número que identifica a los datos de cada
# día. La idea es cambiar ese número en un loop, hacer la consulta en
# la web y extraer los datos

# Instalar paquetes de python: lxml y requests.
# Actualizar pip, por las dudas:
# C:\Python27> .\python.exe -m pip install --upgrade pip
# C:\Python27> .\Scripts\pip.exe install lxml
# C:\Python27> .\Scripts\pip.exe install requests

import lxml.html as LH
import requests, re

# Archivo csv en donde guardar los datos:
arch  = open('C:\Users\juan.barreneche\Downloads\datos_inia.csv', 'w')

inicio = 86000   # Sin saber cuál es el primer id empiezo por 1
fin = 86500  # El día que hice la consulta, este era el último dato
for i in range(inicio, fin):
	# Hacer la consulta web y guardar la respuesa en un objeto (no
	# conozco detalles).
	page = requests.get('http://www.inia.org.uy/gras/computo/gras_planilla_variables_visualizar_diarias.php?id=' + str(i))

	# Esto tampoco sé que hace, lo vi en un internet...
	tree = LH.fromstring(page.content)

    # Este loop saca todos los datos de 1 consulta (1 día); del html
    # toma el tr (table row), ya que los datos están en formato tabla.
    # Llegué a esto usando el Inspect Elment del firefox.
	tabla = [re.split("\n\s+", td.text_content().strip()) for td in tree.xpath('//tr[@class="v8"]')]

	# Cada fila es un parámetro diferente. Precipitación creo que es
	# tabla[11] o algo así.

	tabla[7][0] = re.sub('"', '', tabla[7][0]) # Tanque "A" -> Tanque A
	# Los elementos 14 y 15 tienen unos problemitas que se arreglan acá:
	if len(tabla[14]) == 3:
		tabla[14] = [''.join(tabla[14][0:2]).strip(), tabla[14][-1]]
	else:
		tabla[14] = [''.join(tabla[14][0:2]).strip()]
	if len(tabla[15]) == 3:
		tabla[15] = [''.join(tabla[15][0:2]).strip(), tabla[15][-1]]
	else:
		tabla[15] = [''.join(tabla[15][0:2]).strip()]
	del tabla[16]  # No sirve
	del tabla[-7:] # No sirven

	# Si es la primera iteración, escribir el encabezado de las columnas
	# en el csv:
	if i == inicio:
		arch.write("id;")
		for j in range(len(tabla)):
			arch.write(tabla[j][0].encode('latin_1'))
			if j < (len(tabla) - 1):
				arch.write(';')
			else:
				arch.write('\n')
	# Ahora sí, escribir los datos:
	arch.write(str(i)) # Este es el id, que no viene desde la web
	# Este loop escribe todos los datos, separando con ;
	for j in range(len(tabla)):
		entrada =  ';' + ''.join(tabla[j][1:]).encode('latin_1')
		arch.write(re.sub("\s+", " ", entrada))
	arch.write('\n') # Paso a la siguiente línea del archivo csv

arch.write('\n') # Nueva línea para terminar el archivo csv
arch.close()     # Cerrar el archivo
