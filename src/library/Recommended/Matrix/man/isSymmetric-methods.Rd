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
  \code{M} is a symmetric matrix.  The methods are defined for virtual
  Matrix classes such that it works for all objects inheriting from
  class \code{\linkS4class{Matrix}}.
}
\section{Methods}{
  \describe{
    \item{object = "symmetricMatrix"}{ ... }
    \item{object = "triangularMatrix"}{ ... }
    \item{object = "denseMatrix"}{ ... }
    \item{object = "diagonalMatrix"}{ ... }
    \item{object = "sparseMatrix"}{ ... }
  }
}
\keyword{methods}
