# Función 100% inútil que hice para entretenerme

mepa <- function(n_=4) {
	x <- 'me parece bien'
	if(n_ <= 1) {
		x <- sub('me', 'te', x)
		x <- sub('parece', 'parezca', x)
		x <- paste('me parece bien que', x)
	} else {
		for(i in 1:(n_ - 1)) {
			x <- c('me parece bien que', x)
		}		
		x <- gsub('parece', 'parezca', x)
		x[(1:n_%%2) == 0] <- gsub('te', 'me', x[(1:n_ %% 2) == 0])
		x[(1:n_%%2) == 1] <- gsub('me', 'te', x[(1:n_ %% 2) == 1])
		x <- c('me parece bien que', x)
		x <- paste(x, collapse=' ')
	}
	return(x)
}

### EJEMPLOS:
# mepa(8)
# for(i in 1:6) cat(mepa(i), append=TRUE, file='mepa.txt', sep='\n')
### Luego abrir el archivo mepa.txt de la carpeta de trabajo...
