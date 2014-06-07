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
  decompositions of positive-semidefinite, real dense matrices.  The
  \code{"BunchKaufman"} class is the class of Bunch-Kaufman
  decompositions of symmetric, real matrices.  The \code{"pCholesky"}
  and \code{"pBunchKaufman"} classes are their \emph{\bold{p}acked}
  storage versions.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Cholesky",
    ...)} or \code{new("BunchKaufman", ...)}, etc,
  or rather by calls of the form \code{\link{chol}(pm)} or
  \code{\link{BunchKaufman}(pm)} where \code{pm} inherits from
  the \code{"\linkS4class{dpoMatrix}"} or \code{"\linkS4class{dsyMatrix}"}
  class or as a side-effect of other functions
  applied to \code{"dpoMatrix"} objects (see \code{\linkS4class{dpoMatrix}}).
}
\note{
  \enumerate{
    \item Objects of class \code{"Cholesky"} typically stem from
      \code{\link{chol}(D)}, applied to a \emph{dense} matrix \code{D}.

      On the other hand, the \emph{function} \code{\link{Cholesky}(S)}
      applies to a \emph{sparse} matrix \code{S}, and results in objects
      inheriting from class \code{\linkS4class{CHMfactor}}.

    \item For traditional matrices \code{m}, \code{chol(m)} is a
      traditional matrix as well, triangular, but simply an \eqn{n\times
	n}{n * n} numeric \code{\link{matrix}}.  Hence, for compatibility,
      the \code{"Cholesky"} and \code{"BunchKaufman"} classes (and their
      \code{"p*"} packed versions) also extend triangular Matrix classes
      (such as "dtrMatrix").

      Consequently, \code{\link{determinant}(R)} for \code{R <- chol(A)}
      returns the determinant of \code{R}, not of \code{A}.  This is in
      contrast to class \code{\linkS4class{CHMfactor}} objects \code{C}, where
      \code{determinant(C)} gives the determinant of the \emph{original}
      matrix \code{A}, for \code{C <- Cholesky(A)}, see also the
      \code{determinant} method documentation on the class
      \code{\linkS4class{CHMfactor}} page.
  }
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
  Both these factorizations can \emph{directly} be treated as
  (triangular) matrices, as they extend \code{"dtrMatrix"}, see above.
  There are currently no further explicit methods defined with class
  \code{"Cholesky"} or \code{"BunchKaufman"} in the signature.
}
\seealso{
  Classes \code{\linkS4class{dtrMatrix}}, \code{\linkS4class{dpoMatrix}};
  function \code{\link{chol}}.

  Function \code{\link{Cholesky}} resulting in class
  \code{\linkS4class{CHMfactor}} objects, \emph{not} class "Cholesky"
  ones, see the section \sQuote{Note}.
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
