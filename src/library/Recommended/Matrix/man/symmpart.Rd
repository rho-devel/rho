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
  symmpart(x) computes the symmetric part \code{(x + t(x))/2} and the
  skew symmetric part \code{(x - t(x))/2} of a square matrix \code{x}.
  Note that \code{x == symmpart(x) + skewpart(x)} for all square
  matrices.
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
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
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
}
\keyword{array}
\keyword{arith}
