basemap="$1"

g.region raster=$basemap

r.info map=$basemap -r > range.txt

echo $(grep min range.txt | sed "s/min=//"):\
     $(grep max range.txt | sed "s/max=//"):\
     0:255 > reglas.txt

r.recode --overwrite \
  input=${basemap}@PERMANENT \
  output=${basemap}_rsc \
  rules=reglas.txt

echo Range of input raster $basemap:
cat range.txt

echo Range of output ${basemap}_rsc:
r.info map=${basemap}_rsc -r
