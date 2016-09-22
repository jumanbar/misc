# Ej: bash grass_make_composite.sh 83_toar swir 7 5 1
basename=$1 # Ej: 83_toar
sufix=$2    # Ej: swir

redband=$3   # Ej: 7 (SWIR)
greenband=$4 # Ej: 5 (SWIR)
blueband=$5  # Ej: 1 (SWIR)

echo Setting region to match map extents: ${basename}_$redband
g.region raster=${basename}_$redband

###### Mejorar colores
echo Enhancing colors...
i.colors.enhance \
  red=${basename}_$redband \
  green=${basename}_$greenband \
  blue=${basename}_$blueband


###### Composite
echo Creating comosite...
r.composite --overwrite \
  red=${basename}_$redband \
  green=${basename}_$greenband \
  blue=${basename}_$blueband \
  output=${basename}_${sufix}
