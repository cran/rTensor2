\name{tsym}
\alias{tsym}
\title{Transpose of a Symmetric 3-mode Tensor}

\description{
Performs the transpose of a symmetric 3-mode tensor.
}
\usage{
tsym(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}
}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
T <- rand_tensor(modes=c(2,3,4))
print(tsym(T))
}
