QR <- function(A) {
  # Performs QR Decomposition of a Complex Matrix without pivoting.

  # Input: A square complex matrix A.
  # Output: Matrices Q and R so that A=QR.

  A <- as.matrix(A)
  # Get the number of rows and columns of the matrix
  n <- ncol(A)
  m <- nrow(A)

  # Initialize the Q and R matrices
  q <- matrix(0, m, n)
  r <- matrix(0, n, n)

  for (j in 1:n) {
    v = A[,j] # Step 1 of the Gram-Schmidt process v1 = a1
    # Skip the first column
    if (j > 1) {
      for (i in 1:(j-1)) {
        r[i,j] <- t(q[,i]) %*% A[,j] # Find the inner product (noted to be q^T a earlier)
        # Subtract the projection from v which causes v to become perpendicular to all columns of Q
        v <- v - r[i,j] * q[,i]
      }
    }
    # Find the L2 norm of the jth diagonal of R
    r[j,j] <- sqrt(sum(v^2))
    # The orthogonalized result is found and stored in the ith column of Q.
    q[,j] <- v / r[j,j]
  }

  # Collect the Q and R matrices into a list and return
  qrcomp <- list('Q'=q, 'R'=r)
  return(qrcomp)
}
