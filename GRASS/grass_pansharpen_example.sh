###### Reflectance correction
## Calculates top-of-atmosphere radiance or reflectance and temperature for 
## Landsat MSS/TM/ETM+/OLI
i.landsat.toar \
  input=83_20151115_B \
  output=83_toar \
  metfile=/home/jmb/Ramsar_2016/SIG/Landsat/83_2015_11_15/83_2015_11_15_UTM22S_MTL.txt

###### Recode rasters created by i.landsat.toar
basemap="$1" ## $1 = starndard input.. ej: toar3

g.region raster=$basemap # Este paso asegura que el panchromatic (hi-res) no
g.region raster=$basemap # This step makes sures the hi-res panchromatic raster
                         # doesn't loose resolution as it's recoded

## Get range of pixel values, for latter use (r.recode)
r.info map=$basemap -r > range.txt
echo $(grep min range.txt | sed "s/min=//"):\
     $(grep max range.txt | sed "s/max=//"):\
     0:255 > reglas.txt

## r.recode changes from float point values to 0-255 integer:
r.recode --overwrite \
  input=${basemap}@PERMANENT \
  output=${basemap}_rsc \
  rules=reglas.txt

## Show r.recode's work:
echo Range of input raster $basemap:
cat range.txt

echo Range of output ${basemap}_rsc:
r.info map=${basemap}_rsc -r

###### Pansharpen
## Now to bussiness: make that pansharpen
i.pansharpen --overwrite \
  red=toar4_rsc@PERMANENT \
  green=toar3_rsc@PERMANENT \
  blue=toar2_rsc@PERMANENT \
  pan=toar8_rsc@PERMANENT \
  output=84_15nov15_pan_b \
  method=brovey

###### Remove rasters from the grassdata files
# g.remove -f type=raster name=84_15nov15_pan_blue,84_15nov15_pan_brovey_blue

###### Enhance colors before making the comosite
i.colors.enhance \
  red=83_15nov15_pan_ihs_red@PERMANENT \
  green=83_15nov15_pan_ihs_green@PERMANENT \
  blue=83_15nov15_pan_ihs_blue@PERMANENT

###### Create a new raster by way of compositing the 3 new formed rasters (RGB
###### bands).
r.composite \
  red=84_15nov15_pan_ihs_red@PERMANENT \
  green=84_15nov15_pan_ihs_green@PERMANENT \
  blue=84_15nov15_pan_ihs_blue@PERMANENT \
  output=84_15nov15_composite_sharp


