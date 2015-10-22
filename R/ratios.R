#' Ratios of vector values
#'
#' Computation of ratios of consecutive values in a numeric vector
#' 
#' @param x numeric: vector of numeric values.
#'
#' @value The ratios of consecutive values in a vector (2nd/1st, 3rd/2nd, and so on...). It is meant to be
#'        analogous to the \code{\link{diff}} function.
#'
#' @seealso \code{\link{diff}}.
ratios <- function(x) 10 ^ (diff(log10(x)))
