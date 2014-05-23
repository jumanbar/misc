crtHexGrid <- function(d, Nx, Ny) {
  xcoor <- seq(0, d * (Nx - 1), by=d)
  xcoor <- rep(xcoor, Ny)
  xind  <- rep(1:Ny, each=Nx)
  p <- xind %% 2 == 0
  xcoor[p] <- xcoor[p] + d / 2
  
  dy <- d * sin(pi / 3)
  ycoor <- seq(0, dy * (Ny - 1), by=dy)
  ycoor <- rep(ycoor, each=Nx)

  return(cbind(xcoor, ycoor))
}

crtHexagon <- function(d) {
  ang  <- seq(2 * pi / 6, 2 * pi, len=6)
  coor <- rbind(c(0, 0), cbind(d * cos(ang), d * sin(ang)))
  return(coor)
}
