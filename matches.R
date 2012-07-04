# Partidos de futbol ficticios y la tabla de posiciones correspondiente

idayvuelta <- TRUE

#~ nombres <- c('cacho', 'pepe', 'tato', 'lolo', 'toto', 'kike', 'tito', 'cacu', 'mono')
nombres <- c('cacho', 'pepe', 'tato')

matches <- combn(nombres, 2)

orden   <- sample(ncol(matches))

matches <- matches[, orden]

resultados <- data.frame(matches[1,], NA, NA, matches[2,])
names(resultados) <- c('loc', 'gloc', 'gvis', 'vis')

if(idayvuelta) {
	resultados2 <- data.frame(matches[2,], NA, NA, matches[1,])
	names(resultados2) <- c('loc', 'gloc', 'gvis', 'vis')
	resultados <- rbind(resultados, resultados2)
}

#~ goles <<- as.data.frame(matrix(0, nrow=length(nombres), ncol=length(nombres)))
#~ colnames(goles) <- rownames(goles) <- nombres
tabla <- as.data.frame(matrix(0, length(nombres), 8))
colnames(tabla) <- c('PJ', 'PG', 'PE', 'PP', 'GF', 'GC', 'DG','PTS')
rownames(tabla) <- nombres

partido <- function(l, v, golesl, golesv, tab=tabla) {

	resul <- resultados
	fila <- resul$loc == l & resul$vis == v
	cols <- 2:3
	if(!any(fila)) {
		fila <- resul$loc == v & resul$vis == l
		cols <- 3:2
	}
	resul[fila, cols] <- c(golesl, golesv)
	resultados <<- resul
#~ 	goles[l,v] <- golesl
#~ 	goles[v,l] <- golesv
	tab <- actualizarTabla(tab, l, v, golesl, golesv)
	tab <- ordenarTabla(tab)
	return(tab)
}

ordenarTabla <- function(tab) {
	orden <- order(tab$PTS, tab$DG, tab$PG, tab$GF, decreasing=TRUE)
	return(tab[orden,])
}

actualizarTabla <- function(tab, l, v, golesl, golesv) {
	lgana <- golesl > golesv
	vgana <- golesl < golesv
	empat <- golesl == golesv
	tab[l,] <- tab[l,] + c(1, lgana, empat, vgana, golesl, golesv, golesl - golesv, lgana * 3 + empat)
	tab[v,] <- tab[v,] + c(1, vgana, empat, lgana, golesv, golesl, golesv - golesl, vgana * 3 + empat)
	return(tab)
}



