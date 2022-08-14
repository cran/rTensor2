\name{tSVDdht}
\alias{tSVDdht}
\title{Tensor Singular Value Decomposition Using  the Discrete Harley Transform}

\description{
Performs a tensor singular value decomposition on any 3-mode tensor using the discrete Harley transform.
}
\usage{
tSVDdht(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

If the SVD is performed on a \eqn{m x n x k} tensor, the components in the returned value are:

    U: The left singular value tensor object (\eqn{m x m x k})

    V: The right singular value tensor object (\eqn{n x n x k})

    S: A diagonal tensor (\eqn{m x n x k})
}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\examples{
require(rTensor)
T <- rand_tensor(modes=c(2,3,4))
print(tSVDdht(T))
}

