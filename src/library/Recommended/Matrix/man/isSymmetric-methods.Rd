\name{isSymmetric-methods}
\docType{methods}
\alias{isSymmetric-methods}
\alias{isSymmetric,symmetricMatrix-method}
\alias{isSymmetric,triangularMatrix-method}
\alias{isSymmetric,denseMatrix-method}
\alias{isSymmetric,diagonalMatrix-method}
\alias{isSymmetric,sparseMatrix-method}
\title{Methods for Function isSymmetric in Package 'Matrix'}
\description{
  \code{isSymmetric(M)} returns a \code{\link{logical}} indicating
  if \code{M} is a symmetric matrix.  This is a \pkg{base} function
  with a default method for the traditional matrices of \code{\link{class}}
  \code{"matrix"}.  Methods here are defined for virtual Matrix classes
  such that it works for all objects inheriting from class
  \code{\linkS4class{Matrix}}.
}
\seealso{\code{\link{forceSymmetric}}, \code{\link{symmpart}}, and the
  formal class (and subclasses) \code{"\linkS4class{symmetricMatrix}"}.
}
%% \usage{
%% }
%% \section{Methods}{
%%   \describe{
%%     \item{object = "symmetricMatrix"}{ ... }
%%     \item{object = "triangularMatrix"}{ ... }
%%     \item{object = "denseMatrix"}{ ... }
%%     \item{object = "diagonalMatrix"}{ ... }
%%     \item{object = "sparseMatrix"}{ ... }
%%   }
%% }
\examples{
isSymmetric(Diagonal(4)) # TRUE of course
M <- Matrix(c(1,2,2,1), 2,2)
isSymmetric(M) # TRUE (*and* of formal class "dsyMatrix")
isSymmetric(as(M, "dgeMatrix")) # still symmetric, even if not "formally"
isSymmetric(triu(M)) # FALSE
}
\keyword{methods}
