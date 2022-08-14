tINVdwht <- function(tnsr){
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")
  dwhtz <- aperm(apply(tnsr@data, MARGIN = 1:2, fwht), c(2,3,1))
  T_inv <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    T_inv[, , j] <- solve(dwhtz[, , j])
  }
  T_inv <- as.tensor(aperm(apply(T_inv, MARGIN = 1:2, ifwht), c(2,3,1)))
  return(T_inv)
}
