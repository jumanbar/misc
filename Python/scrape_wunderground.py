from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
from selenium.webdriver.common.keys import Keys
import requests, sys, re, os

## NOTE: needs the geckodriver.exe file in the working directory!!
## Geckodriver github page: https://github.com/mozilla/geckodriver/releases
if not os.path.isfile("geckodriver.exe"):
    sys.exit("geckodriver.exe needs to be on the working directory or PATH")

# Alternative code (using Chrome and read_html from pandas, but just printing
# in stdout) in SO question:
# https://stackoverflow.com/a/55307310/1030523


# Sample list of dates:
fechas = ['2019-02-19', '2019-02-20', '2019-02-21', '2019-02-22', '2019-02-23']

# Using a file with a list of dates:
# with open('fechas_unicas.txt', 'r') as fechas_file:
#     fechas = [linea.strip() for linea in fechas_file]

for j in range(len(fechas)):

    print '==== EXTRACTING DATA FROM DAY: ' + fechas[j] + '===='

    # URL with the data (Airport near Punta del Este, Maldonado, Uruguay):
    url = 'https://www.wunderground.com/history/daily/uy/maldonado/SULS/date/' + fechas[j]

    # Commands related to the webdriver (not sure what they do, but I can guess):
    bi = FirefoxBinary(r'C:\Program Files (x86)\Mozilla Firefox\\firefox.exe')
    br = webdriver.Firefox(firefox_binary=bi)

    # This starts an instance of Firefox at the specified URL:
    br.get(url)

    # I understand that at this point the data is in html format and can be
    # extracted with BeautifulSoup:
    sopa = BeautifulSoup(br.page_source, 'lxml')

    # Close the firefox instance started before:
    br.quit()

    # I'm only interested in the tables contained on the page:
    tablas = sopa.find_all('table')


    prefijo = 'fecha' + fechas[j]

    # Write all the tables into csv files:
    for i in range(len(tablas)):
        tabla = tablas[i]
        nombre_archivo = 'wunderground\\' + prefijo + '_' + str(i) + '.csv'
        print "Escribiendo tabla en el archivo: " + nombre_archivo
        out_file = open(nombre_archivo, 'w')

        # ---- Write the table header: ----
        table_head = tabla.findAll('th')
        output_head = []
        for head in table_head:
            output_head.append(head.text.strip())

        # Some cleaning and formatting of the text before writing:
        encabezado = '"' + '";"'.join(output_head) + '"'
        encabezado = re.sub('\s', '', encabezado) + '\n'
        out_file.write(encabezado.encode(encoding='UTF-8'))

        # ---- Write the rows: ----
        output_rows = []
        filas = tabla.findAll('tr')
        for j in range(1, len(filas)):
            table_row = filas[j]
            columns = table_row.findAll('td')
            output_row = []
            for column in columns:
                output_row.append(column.text.strip())

            # Some cleaning and formatting of the text before writing:
            fila = '"' + '";"'.join(output_row) + '"'
            fila = re.sub('\s', '', fila) + '\n'
            out_file.write(fila.encode(encoding='UTF-8'))

        out_file.close()

    # sys.exit("Fin de la prueba")
