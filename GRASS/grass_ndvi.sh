$basename=$1 # Ej: 83_20151115_B

i.vi --overwrite \
  red=${basename}4 \
  output=${basename}_ndvi \
  viname=ndvi \
  nir=${basename}5 \
  green=${basename}3 \
  blue=${basename}2
