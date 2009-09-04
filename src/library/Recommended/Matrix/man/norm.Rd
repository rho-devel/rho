\name{norm}
\title{Matrix Norms}
\alias{norm}
\alias{norm,ANY,missing-method}
\alias{norm,matrix,character-method}
\alias{norm,Matrix,character-method}
\usage{
norm(x, type, \dots)
}
\description{
    Computes a matrix norm of \code{x} using Lapack.  The norm can be
    the one norm, the infinity norm, the Frobenius norm, or the maximum
    modulus among elements of a matrix, as determined by the value of
    \code{type}.
}
\arguments{
  \item{x}{
    a real or complex matrix.
  }
  \item{type}{
    A character indicating the type of norm desired.
    \describe{
      \item{\code{"O"}, \code{"o"} or \code{"1"}}{specifies the one norm,
	(maximum absolute column sum);}
      \item{\code{"I"} or \code{"i"}}{specifies the infinity norm (maximum
	absolute row sum);}
      \item{\code{"F"} or \code{"f"}}{specifies the Frobenius norm (the
	Euclidean norm of \code{x} treated as if it were a vector); and}
      \item{\code{"M"} or \code{"m"}}{specifies the maximum modulus of
	all the elements in \code{x}.}
    }
    The default is \code{"O"}.  Only the first character of
    \code{type[1]} is used.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
    A numeric value of class \code{"norm"}, representing the quantity
    chosen according to \code{type}.
}
\details{
  For dense matrices, the methods eventually call the Lapack functions
  \code{dlange}, \code{dlansy}, \code{dlantr}, \code{zlange},
  \code{zlansy}, and \code{zlantr}.
}
\references{
  Anderson, E., et al. (1994).
  \emph{LAPACK User's Guide,}
  2nd edition, SIAM, Philadelphia.
}
\examples{
x <- Hilbert(9)
norm(x, "1")
norm(x, "I")
norm(x, "F")
norm(x, "M")
}
\keyword{algebra}
