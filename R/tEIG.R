tEIG <- function(tnsr,tform) {
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")

  if (tform=="fft") {
    TE = tEIGfft(tnsr)
  } else if (tform=="dwt") {
    TE = tEIGdwt(tnsr)
  } else if (tform=="dct") {
    TE = tEIGdct(tnsr)
  } else if(tform=="dst") {
    TE = tEIGdst(tnsr)
  } else if(tform=="dwht") {
    TE = tEIGdwht(tnsr)
  } else if(tform=="dht") {
    TE = tEIGdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  tEIG <- TE
  return(tEIG)
}
