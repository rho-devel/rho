\name{nMatrix-class}
\docType{class}
\alias{nMatrix-class}
\alias{show,nMatrix-method}
\alias{coerce,matrix,nMatrix-method}
\alias{coerce,nMatrix,matrix-method}
\alias{coerce,nMatrix,dMatrix-method}
\alias{coerce,dMatrix,nMatrix-method}
\alias{coerce,nMatrix,lMatrix-method}
\alias{coerce,lMatrix,nMatrix-method}
% %\alias{coerce,dMatrix,dgeMatrix-method}
%
%% Group methods
\alias{Arith,nMatrix,numeric-method}
\alias{Arith,numeric,nMatrix-method}
\alias{Ops,nMatrix,numeric-method}
\alias{Ops,numeric,nMatrix-method}
% \alias{Arith,dMatrix,dMatrix-method}
% \alias{Math2,dMatrix,numeric-method}% Math2 = round + signif, but
% \alias{Math2,dMatrix,missing-method}
\alias{Summary,nMatrix-method}
% \alias{Compare,dMatrix,dMatrix-method}
% \alias{Compare,dMatrix,numeric-method}
\alias{Compare,nMatrix,nMatrix-method}
\alias{Logic,nMatrix,Matrix-method}
\alias{Logic,Matrix,nMatrix-method}
\alias{Ops,nMatrix,lMatrix-method}
\alias{Ops,lMatrix,nMatrix-method}
% % for silly reasons, need these 2+3 as well:
% \alias{round,dMatrix,numeric-method}
% \alias{signif,dMatrix,numeric-method}
% \alias{log,dMatrix-method}
% \alias{gamma,dMatrix-method}
% \alias{lgamma,dMatrix-method}
% %
% \alias{zapsmall,dMatrix-method}
%
\title{Class "nMatrix" of Non-zero Pattern Matrices}
\description{
  The \code{nMatrix} class is the virtual \dQuote{mother} class of all
  \emph{\bold{n}on-zero pattern} (or simply \emph{patter\bold{n}})
  matrices in the \pkg{Matrix} package.
}
%\section{Objects from the Class}{A virtual Class: No objects may be
%  created from it.
%}
\section{Slots}{
  Common to \emph{all} matrix object in the package:
  \describe{
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
    \item{\code{Dimnames}:}{list of length two; each component
      containing NULL or a \code{\link{character}} vector length
      equal the corresponding \code{Dim} element.}
  }
}
\section{Methods}{
  There is a bunch of coercion methods (for \code{\link{as}(..)}), e.g.,
  \describe{
    \item{coerce}{\code{signature(from = "nMatrix", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "nMatrix", to = "dMatrix")}: ... }
    \item{coerce}{\code{signature(from = "nMatrix", to = "lMatrix")}: ... }

    \item{coerce}{\code{signature(from = "matrix", to = "nMatrix")}:
      Note that these coercions (must) coerce \code{\link{NA}}s to
      non-zero, hence conceptually \code{TRUE}.
      This is particularly important when
      \code{\linkS4class{sparseMatrix}} objects are coerced to
      \code{"nMatrix"} and hence to \code{\linkS4class{nsparseMatrix}}.
    }
    \item{coerce}{\code{signature(from = "dMatrix", to = "nMatrix")}: ... }
    \item{coerce}{\code{signature(from = "lMatrix", to = "nMatrix")}: ... }
  }
}
\seealso{
  The classes \code{\linkS4class{lMatrix}},
  \code{\linkS4class{nsparseMatrix}}, and the mother class,
  \code{\linkS4class{Matrix}}.
}
\examples{
getClass("nMatrix")

L3 <- Matrix(upper.tri(diag(3)))
L3 # an "ltCMatrix"
as(L3, "nMatrix") # -> ntC*

## similar, not using Matrix()
as(upper.tri(diag(3)), "nMatrix")# currently "ngTMatrix"
}
\keyword{classes}
\keyword{algebra}
