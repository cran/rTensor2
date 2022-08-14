tsym <- function (tnsr) {
  x <- as.array(tnsr@data)
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  tTsym <- array(0,dim = c(n2,n1,n3))
  for (i in 1:n3){
    tTsym[,,i] <- base::t(x[,,i])
  }
  indices <- c(n2,n1,n3)
  tTsym <- as.tensor(array(tTsym, dim = indices))
  return(tTsym)
}
