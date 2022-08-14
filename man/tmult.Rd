\name{tmult}
\alias{tmult}
\title{Tensor Multiplication Using Any Discrete Transform}

\description{
Multiplies two 3-mode tensors using any discrete transform.
}
\usage{
tmult(x,y,tform)
}
\arguments{
\item{x}{: a 3-mode tensor}

\item{y}{: a 3-mode tensor}

\item{tform}{: Any discrete transform.  Supported transforms are:

    fft: Fast Fourier Transform

    dwt: Discrete Wavelet Transform (Haar Wavelet)

    dct: Discrete Cosine transform

    dst: Discrete Sine transform

    dht: Discrete Hadley transform

    dwht: Discrete Walsh-Hadamard transform}
}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

"transform" (e.g. "dst")}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\examples{
require(rTensor)
T1 <- rand_tensor(modes=c(2,2,4))
T2 <- rand_tensor(modes=c(2,3,4))
print(tmult(T1,T2,"dst"))
}

