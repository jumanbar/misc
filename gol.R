## Game of life: Juego de la vida de J.H. Conway
## Ejemplos:
# gol(plotname='ejemplo.pdf', tfinal=600) -> x
# ini <- cbind(rep(3,3), 2:4)
# gol(N=5, init=ini, plotname='ejemplo2.pdf', tfinal=20) -> x
# gol(init='nave.csv', tfinal=300) -> x
# gol(init='glider_shuter.csv', tfinal=300) -> x
gol <- function(init='random', N=30, tfinal=60, saveplot=TRUE, plotname='gameOfLife.pdf') {
  if(length(N) == 1)
    N1 <- N2 <- N
  if(length(N) == 2) {
    N1 <- N[1]
    N2 <- N[2]
  }
  if(is.character(init[1])) {
    if(init == 'random')
      base <- matrix(rbinom(N1 * N2, 1, 0.5), nrow=N1, ncol=N2)
    else {
      base <- read.table(init)
      base <- as.matrix(base)
      N1   <- nrow(base)
      N2   <- ncol(base)
    }
  } else {
    if(is.null(dim(init)))
      stop('El arg. init debe ser matriz')
    if(dim(init)[2] == 2) {
      base <- matrix(0, N1, N2)
      base[init] <- 1
    } else {
      base <- init
      N1   <- nrow(base)
      N2   <- ncol(base)
    }
  }
  require(simecol)
  
  out <- vector('list', tfinal)
  out[[1]] <- base
  if(saveplot) {
    pdf(plotname)
    image(base, col=0:1)
  }
  for(t_ in 2:tfinal) {
    ## Para que los bordes estén conectados, creo tmp.
    ## Esto genera una estructura de 'toro'
    tmp <- cbind(base[,N2], base, base[,1])
    tmp <- rbind(c(base[N1, N2], base[N1,], base[N1, 1]),
		 tmp,
		 c(base[1, N2], base[1,], base[1, 1]))
    vec <- neighbors(tmp)
    dim(vec) <- c(N1 + 2, N2 + 2)

    ## Ahora recorto vec para seguir:
    vec <- vec[- c(1, N1 + 2), - c(1, N2 + 2)]
    mueren <- (base)  * (vec > 3 | vec < 2)
    nacen  <- (!base) * (vec == 3)
    nueva  <- base + nacen - mueren
    out[[t_]] <- base <- nueva
    if(saveplot)
      image(nueva, col=0:1)
  }
  if(saveplot) {
    dev.off()
    print(paste('Se guardó el archivo', plotname,
    'en el directorio de trabajo actual:', getwd()))
  }
  class(out) <- 'gol'
  invisible(out)
}

plot.gol <- function(x, ...) {  
   for(i in 1:length(x))
      image(x[[1]], col=0:1, ...)
}
   
