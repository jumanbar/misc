## EJEMPLOS (VER ?Startup)
# options(width=65, digits=5)
# options(show.signif.stars=FALSE)
# ps.options(horizontal=FALSE)
# set.seed(1234)
.First <- function() cat("\n   Cargando funciones personalizadas ...\n\n")
.Last <- function()  cat("\n   Tamanana!\n\n")

ayuda <- function(tipo='text') {
  options(help_type=tipo)
}

# setRepositories(addURLs=c(LaPlata = "http://mirror.fcaglp.unlp.edu.ar/CRAN/"))

options(editor='vim', menu.graphics=FALSE)

ejes <- function(x=0, y=0, col=4, ...)
	abline(h=x, v=y, col=col, ...)

num2char <- function(x) {
# x: integer
  n <- nchar(as.character(max(x)))
  m <- nchar(as.character(x))
  for (i in 1:(n - 1)) {
    s <- m == i
    x[s] <- paste(paste(rep(0, n - i), collapse=''), x[s], sep = "")
  }
  return(x)
}

tau <- 2 * pi
