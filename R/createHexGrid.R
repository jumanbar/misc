#' Hexagonal grid creator
#'
#' Creates a grid made of hexagons (or equilateral triangles if you wish), with
#' \code{Nx} by \code{Ny} points.
#'
#' @param d A single numeric value, the (minimum) distance between points and the side of the triangles.
#'
#' @param Nx integer: The number of points along the x axis.
#'
#' @param Ny integer: The number of points along the y axis.
#'
#' @value
#'
#'
#' @example ...
#'
#'
#'
#'
hex_grid <- function(d, Nx, Ny) {
  xcoor <- seq(0, d * (Nx - 1), by = d/2)
  xcoor <- rep.int(xcoor, Ny)
  xind  <- rep(1:Ny, each=Nx)
  p <- xind %% 2 == 0
  xcoor[p] <- xcoor[p] + d / 2

  dy <- d * sin(pi / 3)
  ycoor <- seq(0, dy * (Ny - 1), by=dy)
  ycoor <- rep(ycoor, each=Nx)
  
  return(cbind(xcoor, ycoor))
}

hexagon <- function(d) {
  ang  <- seq(2 * pi / 6, 2 * pi, len=6)
  coor <- rbind(c(0, 0), cbind(d * cos(ang), d * sin(ang)))
  return(coor)
}
