tLU <- function(tnsr,tform) {
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  if (tform=="dwt"){
    if (sum(as.numeric(intToBits(n3))) != 1)
      stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  }
  if (tform=="fft") {
    LU = tLUfft(tnsr)
  } else if (tform=="dwt") {
    LU = tLUdwt(tnsr)
  } else if (tform=="dct") {
    LU = tLUdct(tnsr)
  } else if(tform=="dst") {
    LU = tLUdst(tnsr)
  } else if(tform=="dwht") {
    LU = tLUdwht(tnsr)
  } else if(tform=="dht") {
    LU = tLUdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(list(L = LU$L, U = LU$U))
}
