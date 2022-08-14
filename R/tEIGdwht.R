tEIGdwht <- function (tnsr)
{
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dwhtz <- aperm(apply(tnsr@data, MARGIN = 1:2, fwht), c(2,3,1))
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dwhtz[, , j])
    decompp <- polar(decomp$vectors,decomp$values)
    P_arr[, , j] <- decompp$P
    D_arr[, , j] <- decompp$D
  }
  P <- as.tensor(aperm(apply(P_arr, MARGIN = 1:2,ifwht), c(2,3,1)))
  D <- as.tensor(aperm(apply(D_arr, MARGIN = 1:2,ifwht), c(2,3,1)))
  invisible(list(P = P, D = D))
}
