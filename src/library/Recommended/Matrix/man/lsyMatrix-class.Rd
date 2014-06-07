\name{lsyMatrix-class}
\title{Symmetric Dense Logical Matrices}
\docType{class}
\alias{lspMatrix-class}
\alias{lsyMatrix-class}
%
\alias{coerce,lspMatrix,dspMatrix-method}
\alias{coerce,lspMatrix,lsyMatrix-method}
\alias{coerce,lspMatrix,lgeMatrix-method}
\alias{coerce,lsyMatrix,dsyMatrix-method}
\alias{coerce,lsyMatrix,lgeMatrix-method}
\alias{coerce,lsyMatrix,lspMatrix-method}
\alias{coerce,matrix,lsyMatrix-method}
\alias{coerce,matrix,lspMatrix-method}
\alias{diag,lspMatrix-method}
\alias{diag,lsyMatrix-method}
\alias{diag<-,lspMatrix-method}
\alias{diag<-,lsyMatrix-method}
\alias{t,lspMatrix-method}
\alias{t,lsyMatrix-method}
%
\description{
  The \code{"lsyMatrix"} class is the class of symmetric, dense logical
  matrices in non-packed storage and \code{"lspMatrix"} is the class of
  of these in packed storage. In the packed form, only the upper
  triangle or the lower triangle is stored.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("lsyMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{x}:}{Object of class \code{"logical"}. The logical
      values that constitute the matrix, stored in column-major order.}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}), see the
      \code{\linkS4class{Matrix}} class.}
    \item{\code{factors}:}{Object of class \code{"list"}.  A named
      list of factorizations that have been computed for the matrix.}
  }
}
\section{Extends}{
  Both extend classes \code{"\linkS4class{ldenseMatrix}"} and
  \code{"\linkS4class{symmetricMatrix}"}, directly; further, class
  \code{"Matrix"} and others, \emph{in}directly.  Use
  \code{\link{showClass}("lsyMatrix")}, e.g., for details.
}
\section{Methods}{
  Currently, mainly \code{\link{t}()} and coercion methods (for
  \code{\link{as}(.)}; use, e.g.,
  \code{\link{showMethods}(class="dsyMatrix")} for details.
}
%\references{}
%\author{}
\seealso{
  \code{\linkS4class{lgeMatrix}}, \code{\linkS4class{Matrix}},
  \code{\link[base]{t}}
}
\examples{
(M2 <- Matrix(c(TRUE, NA,FALSE,FALSE), 2,2)) # logical dense (ltr)
str(M2)
# can
(sM <- M2 | t(M2)) # "lge"
as(sM, "lsyMatrix")
str(sM <- as(sM, "lspMatrix")) # packed symmetric
}
\keyword{classes}
