tQRdwht <- function (tnsr)
{
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dwhtz <- aperm(apply(tnsr@data, MARGIN = 1:2, fwht), c(2,3,1))
  Q_arr <- array(0, dim = c(n1, n2, n3))
  R_arr <- array(0, dim = c(n1, n2, n3))
    for (j in 1:n3) {
    decomp <- qr(dwhtz[, , j], nu = n1, nv = n2)
    Q_arr[, , j] <- qr.Q(decomp)
    R_arr[, , j] <- qr.R(decomp)
  }
  Q <- as.tensor(aperm(apply(Q_arr, MARGIN = 1:2, ifwht), c(2,3,1)))
  R <- as.tensor(aperm(apply(R_arr, MARGIN = 1:2, ifwht), c(2,3,1)))
  invisible(list(Q = Q, R = R))
}
