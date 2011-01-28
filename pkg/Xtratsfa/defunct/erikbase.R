# Some functions from this file have been eliminated by changing the code
#  to R direct R function calls, and some others have been made into
#  local functions within the only function tha clled then (in cfaml.R)
#  other functionthat uses them.
# erikbase.R: basic matrix (and other) routines for R
# Erik Meijer, Oct 2004

vec <- function(A) {  matrix(c(A),ncol=1)}
# vec of a matrix: stack its columns in a vector


unit.vec <- function(i, n) {

  # i-th unit vector of order n,
  # i.e., i-th column of eye(n)

#  e.in <- zeros(n,1)
  e.in <- matrix(0,n,1)
  e.in[i,1] <- 1

  return(e.in)
}


diagonalization.matrix <- function(n) {

  # Diagonalization matrix of order n

  D <- NULL
  for (j in 1:n) {
    vv <- unit.vec(j,n) %x%  unit.vec(j,n)  # Kronecker product
    D <- cbind(D, vv)
  }

  return(D)
}

duplic <- function(n) {

  # Duplication matrix of order n

  D <- NULL
  for (j in 1:n) {
    vv <- unit.vec(j,n) %x%  unit.vec(j,n)  # Kronecker product
    D <- cbind(D, vv)
    if (j < n) {
      for (i in (j+1):n) {
        vv <- unit.vec(i,n) %x%  unit.vec(j,n) + unit.vec(j,n) %x%  unit.vec(i,n)
        D  <- cbind(D, vv)
      }
    }
  }

  return(D)
}

i.duplic <- function(n) {

  # Moore-Penrose inverse of duplication matrix of order n

  D   <- duplic(n)                          # duplication matrix
  i.D <- diag(1/diag(t(D) %*% D)) %*% t(D)  # (D'D)^{-1} D', where D'D is diagonal

  return(i.D)
}


