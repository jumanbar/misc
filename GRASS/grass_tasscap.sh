row=83

i.tasscap --overwrite input=${row}_toar_2@PERMANENT,${row}_toar_3@PERMANENT,${row}_toar_4@PERMANENT,${row}_toar_5@PERMANENT,${row}_toar_6@PERMANENT,${row}_toar_7@PERMANENT output=${row}_tasscap sensor=landsat8_oli

wait

i.colors.enhance --overwrite red=${row}_tasscap.1@PERMANENT green=${row}_tasscap.2@PERMANENT blue=${row}_tasscap.3@PERMANENT

wait

r.composite --overwrite red=${row}_tasscap.1@PERMANENT green=${row}_tasscap.2@PERMANENT blue=${row}_tasscap.3@PERMANENT output=${row}_tasscap_comp

wait

r.out.gdal input=${row}_tasscap_comp@PERMANENT output=/home/jmb/Ramsar_2016/SIG/Raster/tasscap/${row}_tasscap_comp format=GTiff
