\name{LU}
\alias{LU}
\title{LU Decomposition of a Complex Matrix}
\usage{
LU(A)
}
\arguments{
\item{A}{: an  \eqn{n x n} matrix}
}
\value{
a lower triangular matrix L and an upper triangular matrix U so that MATRIX = LU.
}
\description{
Decompose matrix A into the product of a lower triangular matrix L and an upper triangular matrix U.
}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\examples{
z <- complex(real = rnorm(16), imag = rnorm(16))
A <- matrix(z,nrow=4)
LU(A)
}
\keyword{misc}
