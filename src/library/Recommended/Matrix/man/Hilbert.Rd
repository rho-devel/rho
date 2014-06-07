\name{Hilbert}
\alias{Hilbert}
\title{Generate a Hilbert matrix}
\description{
    Generate the \code{n} by \code{n} symmetric Hilbert matrix.  Because
    these matrices are ill-conditioned for moderate to large \code{n},
    they are often used for testing numerical linear algebra code.
}
\usage{
Hilbert(n)
}
\arguments{
  \item{n}{a non-negative integer.}
}
\value{
  the \code{n} by \code{n} symmetric Hilbert matrix as a
  \code{"dpoMatrix"} object.
}
\seealso{the class \code{\linkS4class{dpoMatrix}}}
\examples{
Hilbert(6)
}
\keyword{array}
\keyword{algebra}
