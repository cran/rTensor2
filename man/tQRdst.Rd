\name{tQRdst}
\alias{tQRdst}
\title{Tensor QR Decomposition Using  the Discrete Sine Transform}

\description{
Performs a QR decomposition of 3-mode tensor using the discrete Sine transform.
}
\usage{
tQRdst(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

If QR decomposition is performed on a \eqn{n x n x k} tensor, the components in the returned value are:

    Q: The left singular value tensor object (\eqn{n x n x k})

    R: The right singular value tensor object (\eqn{n x n x k})

}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\examples{
require(rTensor)
T <- rand_tensor(modes=c(2,2,4))
print(tQRdst(T))
}

