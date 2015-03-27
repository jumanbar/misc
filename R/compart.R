## Function made to improve performance over older version from
## R package 'bipartite' (http://cran.r-project.org/web/packages/bipartite/index.html)
## Help file: http://www.inside-r.org/packages/cran/bipartite/docs/compart
compart <- function(web) {
# Author: Juan M. Barreneche
# E-mail: jumanbar@gmail.com
# replacing the slower version below 22 May 2012
  cweb <- as.matrix(web)
  rc <- which(web > 0, arr.ind=TRUE)
  r <- rc[,1]
  c <- rc[,2]
  p1 <- numeric(length(r)) + 1
  p2 <- p1
  comp <- 1
  while (any(p2 > 0)) {
    p2[which(p2 == 1)[1]] <- -comp
    while (any(p2 != p1)) {
      p1 <- p2
      p2[r %in% r[p2 == -comp]] <- -comp
      p2[c %in% c[p2 == -comp]] <- -comp
    }
    locs <- matrix(rc[p2 == -comp,], ncol=2)
    cweb[locs] <- -comp
    comp <- comp + 1
  }
  out <- list(cweb=cweb, n.compart=max(abs(p2)))
  out
}

