\name{Cholesky-class}
\docType{class}
\alias{Cholesky-class}
\alias{pCholesky-class}
\alias{BunchKaufman-class}
\alias{pBunchKaufman-class}
%
\alias{coerce,pCholesky,lMatrix-method}
\alias{coerce,pBunchKaufman,lMatrix-method}
\alias{coerce,Cholesky,lMatrix-method}
\alias{coerce,BunchKaufman,lMatrix-method}
\alias{show,BunchKaufman-method}
\alias{show,pBunchKaufman-method}
%
\title{Cholesky and Bunch-Kaufman Decompositions}
\description{The \code{"Cholesky"} class is the class of Cholesky
  decompositions of positive-semidefinite, real matrices.  The
  \code{"BunchKaufman"} class is the class of Bunch-Kaufman
  decompositions of symmetric, real matrices.  The \code{"pCholesky"}
  and \code{"pBunchKaufman"} classes are their \emph{\bold{p}acked}
  storage versions.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Cholesky",
    ...)} or \code{new("BunchKaufman", ...)}, etc,
  or rather by calls of the form \code{chol(pm)} or
  \code{BunchKaufman(pm)} where \code{pm} inherits from
  the \code{"\linkS4class{dpoMatrix}"} or \code{"\linkS4class{dsyMatrix}"}
  class or as a side-effect of other functions
  applied to \code{"dpoMatrix"} objects (see \code{\linkS4class{dpoMatrix}}).
}
\section{Slots}{
  A Cholesky decomposition extends class
  \code{\linkS4class{MatrixFactorization}} but is basically a triangular
  matrix extending the \code{"\linkS4class{dtrMatrix}"} class.
  \describe{
    \item{\code{uplo}:}{inherited from the \code{"dtrMatrix"} class.}
    \item{\code{diag}:}{inherited from the \code{"dtrMatrix"} class.}
    \item{\code{x}:}{inherited from the \code{"dtrMatrix"} class.}
    \item{\code{Dim}:}{inherited from the \code{"dtrMatrix"} class.}
    \item{\code{Dimnames}:}{inherited from the \code{"dtrMatrix"} class.}
  }
  A Bunch-Kaufman decomposition also extends the \code{"dtrMatrix"}
  class and has a \code{perm} slot representing a permutation matrix.
  The packed versions extend the \code{"dtpMatrix"} class.
}
\section{Extends}{
  Class \code{"MatrixFactorization"} and \code{"dtrMatrix"}, directly.
  Class \code{"dgeMatrix"}, by class \code{"dtrMatrix"}.
  Class \code{"Matrix"}, by class \code{"dtrMatrix"}.
}
\section{Methods}{
  No methods defined with class "Cholesky" in the signature.
}
\seealso{
  Classes \code{\linkS4class{dtrMatrix}}, \code{\linkS4class{dpoMatrix}};
  function \code{\link{chol}}.
}
\examples{
(sm <- as(as(Matrix(diag(5) + 1), "dsyMatrix"), "dspMatrix"))
signif(csm <- chol(sm), 4)

(pm <- crossprod(Matrix(rnorm(18), nrow = 6, ncol = 3)))
(ch <- chol(pm))
if (toupper(ch@uplo) == "U") # which is TRUE
   crossprod(ch)
stopifnot(all.equal(as(crossprod(ch), "matrix"),
                    as(pm, "matrix"), tol=1e-14))
}
\keyword{classes}
\keyword{algebra}
