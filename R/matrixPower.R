rw <- function(Po, t) {
  N <- length(Po)
  mat <- makeMat(N)
  out <- matrix.power(t(mat), t) %*% Po
  return(out)
}

makeMat <- function(N) {
  mat <- matrix(0, N, N)
  for(i in 1:(N-1)) {
    mat[i, i + 1] <- .5
    mat[i + 1, i] <- .5
  }
  mat[1, 2] <- 1
  mat[N, N - 1] <- 1
  return(mat)
}


matrix.power <- function(mat, t) {
  # test if mat is a square matrix
  # treat n < 0 and n = 0 -- this is left as an exercise
  # trap non-integer n and return an error
  if (t == 1)
    return(mat)
  result <- diag(1, ncol(mat))
  while (t > 0) {
    if (t %% 2 != 0) {
      result <- result %*% mat
      t <- t - 1
    }
    mat <- mat %*% mat
    t <- t / 2
  }
  return(result)
}
