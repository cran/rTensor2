polar <- function(P,D) {
  n <- length(D)
  retV <- matrix(0,nrow=n,ncol=n)
  retD <- matrix(0,nrow=n,ncol=n)
  for (i in 1:n) {
    V = P
    L = D
    Vtemp = V
    LD <- diag(L)
    j <- 1
    while (j <=length(L)){
      if (is.complex(L[j]) && (abs(Im(L[j])) > 0.0001)){
        # Process V
        V[,j] <- Re(Vtemp[,j])
        V[,(j+1)] <- Im(Vtemp[,j])

        # Process LD
        LD[j,j] <- Re(L[j])
        LD[j,(j+1)] <- Im(L[j])
        LD[(j+1),j] <- -Im(L[j])
        LD[(j+1),(j+1)] <- Re(L[j])
        j <- j+1
      }
      j <- j+1
    }
    V = Re(V)
    LD = Re(LD)
    return(list(P = V, D = LD))
  }
}
