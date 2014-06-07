\name{Schur-class}
\docType{class}
\alias{Schur-class}
%
\title{Class "Schur" of Schur Matrix Factorizations}
\description{Class \code{"Schur"} is the class of Schur matrix
  factorizations.  These are a generalization of eigen value (or
  \dQuote{spectral}) decompositions for general (possibly asymmmetric)
  square matrices, see the \code{\link{Schur}()} function.
}
\section{Objects from the Class}{
  Objects of class \code{"Schur"} are typically created by
  \code{\link{Schur}()}.
}
\section{Slots}{
  \code{"Schur"} has slots
  \describe{
    \item{\code{T}:}{Upper Block-triangular \code{\linkS4class{Matrix}}
      object.}
    \item{\code{Q}:}{Square \emph{orthogonal} \code{"Matrix"}.}
    \item{\code{EValues}:}{numeric or complex vector of eigenvalues of \code{T}.}
    \item{\code{Dim}:}{the matrix dimension: equal to \code{c(n,n)} of
      class \code{"integer"}.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{MatrixFactorization}"}, directly.
}
% \section{Methods}{
%   are all inherited from the MatrixFactorization class
% }
% \note{There's not yet a class for \emph{sparse} Schur decompositions;
%   mainly because there's no \code{\link{Schur}()} method for those neither.
% }
\seealso{
  \code{\link{Schur}()} for object creation;
  \code{\linkS4class{MatrixFactorization}}.
}
\examples{
showClass("Schur")
Schur(M <- Matrix(c(1:7, 10:2), 4,4))
## Trivial, of course:
str(Schur(Diagonal(5)))

## for more examples, see Schur()
}
\keyword{classes}
