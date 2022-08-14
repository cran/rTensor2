tEIGdst <- function (tnsr)
{
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The eigen value decomposition only works for tensors with square lateral faces")
  dstz <- aperm(apply(tnsr@data, MARGIN = 1:2, dst), c(2,3,1))
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dstz[, , j])
    P_arr[, , j] <- decomp$vectors
    D_arr[, , j] <- diag(decomp$values)
  }
  P <- as.tensor(aperm(apply(P_arr, MARGIN = 1:2, idst), c(2,3,1)))
  D <- as.tensor(aperm(apply(D_arr, MARGIN = 1:2, idst), c(2,3,1)))
  invisible(list(P = P, D = D))
}
