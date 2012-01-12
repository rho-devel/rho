\name{symmpart}
\title{Symmetric Part and Skew(symmetric) Part of a Matrix}
%% Hmm, want the generic and methods all in this file ...
%% \docType{methods}
\alias{symmpart}
\alias{skewpart}
%
\alias{symmpart-methods}
\alias{skewpart-methods}
\alias{symmpart,Matrix-method}
\alias{skewpart,Matrix-method}
\alias{symmpart,diagonalMatrix-method}
\alias{skewpart,diagonalMatrix-method}
\alias{symmpart,ddenseMatrix-method}
\alias{skewpart,ddenseMatrix-method}
\alias{symmpart,denseMatrix-method}
\alias{skewpart,denseMatrix-method}
\alias{symmpart,symmetricMatrix-method}
\alias{skewpart,symmetricMatrix-method}
\alias{symmpart,matrix-method}
\alias{skewpart,matrix-method}
%
\description{
  \code{symmpart(x)} computes the symmetric part \code{(x + t(x))/2} and
  \code{skewpart(x)} the
  skew symmetric part \code{(x - t(x))/2} of a square matrix \code{x},
  more efficiently for specific Matrix classes.

  Note that \code{x == symmpart(x) + skewpart(x)} for all square
  matrices -- apart from extraneous \code{\link{NA}} values in the RHS.
}
\usage{
symmpart(x)
skewpart(x)
}
\arguments{
  \item{x}{a \emph{square} matrix; either \dQuote{traditional} of class
    \code{"matrix"}, or typically, inheriting from the
    \code{\linkS4class{Matrix}} class.}
}
% \section{Methods}{
% \describe{

% \item{x = "Matrix"}{ ~~describe this method here }
% }}
\details{
  These are generic functions with several methods for different matrix
  classes, use e.g., \code{\link{showMethods}(symmpart)} to see them.
}
\value{
  \code{symmpart()} returns a symmetric matrix, inheriting from
  \code{\linkS4class{symmetricMatrix}} iff \code{x} inherited from
  \code{Matrix}.

  \code{skewpart()} returns a skew-symmetric matrix,
  typically of the same class as \code{x} (or the closest
  \dQuote{general} one, see \code{\linkS4class{generalMatrix}}).
}
%\references{ ~put references to the literature/web site here ~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
m <- Matrix(1:4, 2,2)
symmpart(m)
skewpart(m)

stopifnot(all(m == symmpart(m) + skewpart(m)))

## investigate the current methods:
showMethods(skewpart, include = TRUE)
}
\keyword{array}
\keyword{arith}
