#' Me parece bien que te parezca bien
#'
#' Función 100% inútil que hice para entretenerme
#'
#' @param n Número de interaciones (integer)
#'
#' @return Character: un string con un diálogo de n ideas y vueltas entre dos
#'   personas a las que les parece bien que a la otra persona le parezca bien.
#' @export
#'
#' @examples
#' mepa(1)
#' mepa(2)
#' mepa(4)
#' # Escrbir en un archivito de texto:
#' for(i in 1:6) cat(mepa(i), append=TRUE, file='mepa.txt', sep='\n')
mepa <- function(n = 1L) {
  x <- 'me parece bien'
  if(n <= 1) {
    x <- sub('me', 'te', x)
    x <- sub('parece', 'parezca', x)
    x <- paste('me parece bien que', x)
  } else {
    for(i in 1:(n - 1)) {
      x <- c('me parece bien que', x)
    }		
    x <- gsub('parece', 'parezca', x)
    x[(1:n%%2) == 0] <- gsub('te', 'me', x[(1:n %% 2) == 0])
    x[(1:n%%2) == 1] <- gsub('me', 'te', x[(1:n %% 2) == 1])
    x <- c('me parece bien que', x)
    x <- paste(x, collapse=' ')
  }
  return(x)
}

