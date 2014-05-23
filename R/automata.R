## Automata elemental
## (http://en.wikipedia.org/wiki/Elementary_cellular_automaton)
## Reglas:
#### 111 110 101 100 011 010 001 000
#110  0   1   1   0   1   1   1   0
# 30  0   0   0   1   1   1   1   0
# 37  0   0   1   0   0   1   0   1
# 18  0   0   0   1   0   0   1   0
# 90  0   1   0   1   1   0   1   0
#150  1   0   0   1   0   1   1   0
# 15  0   0   0   0   1   1   1   1
# 45  0   0   1   0   1   1   0   1
#169  1   0   1   0   1   0   0   1
# (números binarios: http://www.digitconvert.com/)
## Otras reglas interesantes:
## 9 - 22 - 26 - 41 - 54 - 57 - 60 - 62 - 73 - 104 - 105 - 106 - 122 - 126 - 146 - 154 - 184
# Ejs.:
# autom(regla=c(0,1,0,1,1,0,1,0)) -> x
# autom(regla=150, N=100, tfinal=200, init=rbinom(100, 1, .1)) -> x
autom <- function(
                  N=50,
                  tfinal=100,
                  regla=c(0, 1, 1, 0, 1, 1, 1, 0), # regla 110
                  init,
                  ...) {
  if (length(regla) == 1) {
    if (regla > 255 || regla < 0)
      stop('Debe cumplir!: 0 <= regla <= 255')
    regla.b <- integer.base.b(regla)
    largo   <- length(regla.b)
    if (largo < 8)
      regla <- c(numeric(8 - largo), regla.b)
    else
      regla <- regla.b
  }
  ref <- rbind(c(1, 1, 1),c(1, 1, 0),c(1, 0, 1),
               c(1, 0, 0),c(0, 1, 1),c(0, 1, 0),
               c(0, 0, 1),c(0, 0, 0))
  if (missing(init))
    fila <- round(runif(N))
  else
    fila <- init
  out <- matrix(ncol=N, nrow=tfinal)
  out[1,] <- fila
  for (t_ in 2:tfinal) {
    for (i in 2:(N - 1)) {
      cell  <- fila[(i - 1):(i + 1)]
      resta <- t(ref) -  cell
      suma  <- colSums(abs(resta))
      a     <- which(suma == 0)
      out[t_, i] <- regla[a]
    }
    cell1  <- fila[c(N, 1:2)]
    resta1 <- t(ref) - cell1
    suma1  <- colSums(abs(resta1))
    out[t_, 1] <- regla[suma1 == 0]
    cell2  <- fila[c((N - 1):N, 1)]
    resta2 <- t(ref) - cell2
    suma2  <- colSums(abs(resta2))
    out[t_, N] <- regla[suma2 == 0]
    fila   <- out[t_,]
  }
   
  image(out, ...)
  return(out)
}

# Convertidor: entero --> binario
# (http://tolstoy.newcastle.edu.au/R/help/03b/3251.html)
integer.base.b <- function(x, b=2) {
  xi <- as.integer(x) 
  if (any(is.na(xi) | ((x-xi)!=0)) | any(x < 0))
    print(list(ERROR="x not integer or negative", x=x))
  N <- length(x) 
  xMax <- max(x)
  if (xMax > 0)
    ndigits <- (floor(logb(xMax, base=2)) + 1) 
  else
    ndigits <- 1
  Base.b  <- array(NA, dim=c(N, ndigits))
  for(i in 1:ndigits) {
    Base.b[, ndigits-i+1] <- (x %% b) 
    x <- (x %/% b) 
  } 
  if(N ==1)
    Base.b[1, ]
  else
    Base.b 
} 

