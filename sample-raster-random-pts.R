# OBSOLETO: el paquete 'raster' tiene la función 'sampleRandom' que, persumiblemente, sirve para lo mismo...

# NOTA: eventualmente voy a cambiar el script para que haga lo siguiente:
# 1. dada una carpeta con una colección de archivos (capas raster, como pueden ser capas WORLDCLIM), haga un muestreo
#    de puntos aleatorios en todas las capas (los puntos serían los mismos para todas).
# 2. tomar todos los datos muestreados de las capas y meterlos en una tabla (la cual se guarda como csv).

# Esto me anda con R 2.12.1 (2010-12-16), con los paquetes "raster" y "rgdal"
# instalados.

# Fue probado con la capa raster "prec10_44.tif" obtenida de la base de datos
# WORLDCLIM. No he probado con otros tipos de formatos. De todas formas teniendo
# rgdal seguramente el protocolo funcione para la gran mayoría de formatos que
# existen.

# Si hace falta instalarlos:
# install.packages('raster')
# install.packages('rgdal')

# Cargar el paquete:
library(raster)

### PARA GENERAR LAS COORDENADAS DE LOS PUNTOS ###

# Input del usuario:
# Archivo con una capa raster:
archivo <- 'prec10_44.tif'
# Número de puntos a muestrear:
npts <- 60000

# Importar el archivo:
r <- raster(archivo)

# Límites de la capa (coordenadas):
xmin <- r@extent@xmin
ymin <- r@extent@ymin
xmax <- r@extent@ymax
ymax <- r@extent@ymax
xmax <- r@extent@xmax

# Pasos para generar coordenadas aleatorias:
xcor <- runif(npts, xmin, xmax)    # 1 - Coordenadas x aleatorias
ycor <- runif(npts, ymin, ymax)    # 2 - Coordenadas y aleatorias
xy   <- data.frame(x=xcor, y=ycor) # 3 - Crea tabla con coordenadas

# Para ver cuantos valores no NA ("buenos"):
tmp <- extract(r, xy) # 4 - Extrae los valores de esas coordenadas
bad <- is.na(tmp)     # 5 - Cuáles son los "malos"
xy  <- xy[!bad,]      # 6 - Elimina los "malos"
nrow(xy) # Número de valores "buenos"

# De repente acá conviene probar varias veces hasta que la cantidad de valores
# "buenos" sea respetable... (para maxent, con 5 mil está sobrado creo). No
# se puede determinar de antemano cuántos buenos se van a conseguir en cada
# generación de coordenadas aleatorias, ya que depende de cuántos puntos
# caen en el agua y cuántos no.
# Una posible opción (que no está puesta acá) es ir acumulando puntos hasta
# llegar a algo aceptable (repitiendo los pasos 1 al 6 en combinación con
# rbind).

# El objeto "xy" es el que tiene las coordenadas a muestrear, es recomendable
# guardarlo en un archivo de texto:
write.csv2(xy, file='xy.csv')


### EXTRAER LOS VALORES PARA OTRAS CAPAS ###

# Input del usuario:
# Cuántas capas son en total?:
ncapas <- 1
# Nombres de las capas:
nombres <- c('c1')

# Si es necesario, importar la tabla con las coordenadas:
# xy <- read.csv2('xy.csv')

# Tabla final (tendrá los valores de todas las capas):
tabla <- as.data.frame(matrix(0, nrow=nrow(xy), ncol=ncapas + 2))
tabla[, 1:2] <- xy
names(tabla) <- c('x', 'y', nombres)

# Para extraer todos los valores de todas las capas, se toma el objeto xy y
# la función extract (paquete raster) para obtenerlos (pasos a repetir):

nc      <- 1                 # 1 - Número de capa (NO REPETIR!)
capa    <- 'capa.tiff'       # 2 - Nombre del archivo de la capa
r       <- raster(capa)      # 3 - Importar
valores <- extract(capa, xy) # 4 - Extraer valores
tabla[, nc + 2] <- valores   # 5 - Actualizar tabla
nc <- nc + 1                 # 6 - Actualizar nc

