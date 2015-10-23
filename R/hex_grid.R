
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
#' @param x0 numeric: X coordinate for the initial point. For a \code{hex_grid} it's
#'        the point to the lower left corner, while for a \code{hexagon} it's
#'        the center point.
#'
#' @param y0 numeric: Y coordinate for the initial point. For a \code{hex_grid} it's
#'        the point to the lower left corner, while for a \code{hexagon} it's
#'        the center point.
#'
#' @param Ny integer: The number of points along the y axis.
#'
#' @param pch, col, lty numeric: X coordinate for the initial point. For a \code{hex_grid} it's
#'        the point to the lower left corner, while for a \code{hexagon} it's
#'        the center point.
#'
#' @return \code{hex_grid}: An object of class \code{\link{matrix}} and \code{hex_matrix}, which
#'         is basically a matrix with two columns: one for X coordinate and the
#'         second with the Y coordinate. Each row is for one point in the grid.
#'
#'         \code{hexagon}: An object of class \code{\link{matrix}} and \code{hexagon},
#'         same as with \code{hex_grid}.
#'
#'         \code{plot.hex_grid} and \code{plot.hexagon}: NULL
#'
#' @examples
#' plot(hx <- hex_grid(4, 10, 11))
#' points(hg <- hexagon(4, hx[33,1], hx[33,2]), col = 2, pch = 19, cex = 1)
#' plot(hg)
hex_grid <- function(d, Nx, Ny, x0 = 0, y0 = 0) {
  xcoor <- seq(0, d * (Nx - 1), by = d)
  xcoor <- rep.int(xcoor, Ny)
  xind  <- rep(1:Ny, each=Nx)
  p <- xind %% 2 == 0
  xcoor[p] <- xcoor[p] + d / 2

  dy <- d * sin(pi / 3)
  ycoor <- seq(0, dy * (Ny - 1), by=dy)
  ycoor <- rep(ycoor, each=Nx)
  
  out <- cbind(X = xcoor, Y = ycoor)
  out[,1] <- out[,1] + x0
  out[,2] <- out[,2] + y0
  class(out) <- c("matrix", "hex_grid")
  rownames(out) <- 1:nrow(out)
  return(out)
}

#' @describeIn hex_grid Create coordinates for one hexagon, with the same
#'             structure as a \code{hex_grid} object.
hexagon <- function(d, x0 = 0, y0 = 0) {
  ang  <- seq(2 * pi / 6, 2 * pi, len=6)
  coor <- rbind(c(x0, y0), cbind(x0 + d * cos(ang), y0 + d * sin(ang)))
  colnames(coor) <- c("X", "Y")
  rownames(coor) <- 1:nrow(coor)
  class(coor) <- c("matrix", "hexagon")
  return(coor)
}

#' @describeIn hex_grid Plot method for the class \code{hex_grid}.
plot.hex_grid <- function(hx, pch = 20, col = "gray", lty = 3, ...) {
  Nx <- sum(hx[,2] == max(hx[,2]))
  d <- hx[2, 1] - hx[1, 1]
  Ny <- nrow(hx) / Nx
  plot.default(hx, pch = pch, ...)
  xint <- seq(- (Nx - 1) * d * sin(pi / 3) * 2, Ny * d * sin(pi / 3) * 2, by = d * sin(pi / 3) * 2)
  for (i in xint)  abline(i, tan(pi / 3), col = col, lty = lty)
  
  xint <- seq(- Ny * d * sin(pi / 3) * 2, (Nx - 1) * d * sin(pi / 3) * 2, by = d * sin(pi / 3) * 2)
  for (i in xint)  abline(i, - tan(pi / 3), col = col, lty = lty)
  
  hint <- seq(0, Ny * d * sin(pi / 3), by = d * sin(pi / 3))
  abline(h = hint, col = col, lty = lty)
}

#' @describeIn hex_grid Method for ploting a \code{hexagon} object.
plot.hexagon <- function(x, pch = 20, col = "gray", lty = 3, ...) {
  d <- abs(x[1, 1] - x[7, 1])
  y0 <- x[1, 1] * tan(pi / 3)
  plot.default(x, pch = pch, ..., type = "n")
  y <- c(-y0 + x[1, 2] - 2 * d * sin(pi / 3), -y0 + x[1, 2], -y0 + x[1, 2] + 2 * d * sin(pi / 3))
  for (i in y) abline(i, tan(pi / 3), lty = lty, col = col)
  y <- c(y0 + x[1, 2] - 2 * d * sin(pi / 3), y0 + x[1, 2], y0 + x[1, 2] + 2 * d * sin(pi / 3))
  for (i in y) abline(i, -tan(pi / 3), lty = lty, col = col)
  # abline(y0 + x[1, 2], - tan(pi / 3), lty = lty, col = col)
  h <- c(min(x[, 2]), x[1, 2], max(x[, 2]))
  abline(h = h, col = col, lty = lty)
  points.default(x, pch = pch, ...)
}
