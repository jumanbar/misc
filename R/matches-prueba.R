source('matches.R')

#~ gmean <- rnorm(1, 1.5, sd=.5)
#~ while(gmean <= 0) {
#~ 	gmean <- rnorm(1, 1.5, sd=.5)
#~ }
gmean <- 1.5

g <- rpois(2 * nrow(resultados), gmean)

for(i in 1:nrow(resultados)) {
	e1 <- as.character(resultados[i,1])
	e2 <- as.character(resultados[i,4])
	tabla <- partido(e1, e2, g[i * 2 - 1], g[i * 2])
}

#~ n_ <- 1e2
#~ y <- x <- numeric(n_)
#~ for(j in 1:n_) {
#~ 	print(j)
#~ 	source('prueba.R')
#~ 	x[j] <- tabla$PTS[1]
#~ 	y[j] <- tabla$PTS[length(nombres)]
#~ }
#~ 
#~ hist(x, freq=F, xlab='Puntos Winners')
#~ lines(density(x))
#~ legend('topright', legend=mean(x), bty='n')
#~ hist(y, freq=F, xlab='Puntos Lossers')
#~ lines(density(y))
#~ legend('topright', legend=mean(y), bty='n')
#~ hist(z, freq=F, xlab='Diferencia')
#~ lines(density(z))
#~ legend('topright', legend=mean(z), bty='n')
#~ savePlot('fixture/resumen_100_corridas.png')
