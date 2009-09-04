\name{diagonalMatrix-class}
\title{Class "diagonalMatrix" of Diagonal Matrices}
\docType{class}
\alias{diagonalMatrix-class}
%
\alias{as.vector,diagonalMatrix,missing-method}
\alias{coerce,matrix,diagonalMatrix-method}
\alias{coerce,diagonalMatrix,denseMatrix-method}
\alias{coerce,diagonalMatrix,generalMatrix-method}
\alias{coerce,diagonalMatrix,matrix-method}
\alias{coerce,diagonalMatrix,nMatrix-method}
\alias{coerce,diagonalMatrix,nsparseMatrix-method}
\alias{coerce,Matrix,diagonalMatrix-method}
\alias{cbind2,sparseMatrix,diagonalMatrix-method}
\alias{cbind2,diagonalMatrix,sparseMatrix-method}
\alias{rbind2,sparseMatrix,diagonalMatrix-method}
\alias{rbind2,diagonalMatrix,sparseMatrix-method}
\alias{determinant,diagonalMatrix,logical-method}
\alias{norm,diagonalMatrix,character-method}
%
\alias{coerce,ddiMatrix,dgeMatrix-method}
\alias{coerce,ddiMatrix,ddenseMatrix-method}
\alias{coerce,ldiMatrix,ldenseMatrix-method}
% Mainly against ambiguity warnings:
% Horrible-Hack: currently define for "all subclasses of diagonalMatrix"
% ------------- in ../R/diagMatrix.R
\alias{coerce,ddiMatrix,symmetricMatrix-method}
\alias{coerce,ldiMatrix,symmetricMatrix-method}
\alias{coerce,ddiMatrix,triangularMatrix-method}
\alias{coerce,ldiMatrix,triangularMatrix-method}
%_no_longer_ \alias{coerce,ddiMatrix,sparseMatrix-method}
%_no_longer_ \alias{coerce,ldiMatrix,sparseMatrix-method}
\alias{coerce,ddiMatrix,CsparseMatrix-method}
\alias{coerce,ldiMatrix,CsparseMatrix-method}
\alias{coerce,ddiMatrix,TsparseMatrix-method}
\alias{coerce,ldiMatrix,TsparseMatrix-method}
%
\alias{cbind2,ddiMatrix,matrix-method}
\alias{cbind2,ldiMatrix,matrix-method}
\alias{cbind2,matrix,ddiMatrix-method}
\alias{cbind2,matrix,ldiMatrix-method}
\alias{rbind2,ddiMatrix,matrix-method}
\alias{rbind2,ldiMatrix,matrix-method}
\alias{rbind2,matrix,ddiMatrix-method}
\alias{rbind2,matrix,ldiMatrix-method}
%
\alias{cbind2,ddiMatrix,atomicVector-method}
\alias{cbind2,ldiMatrix,atomicVector-method}
\alias{cbind2,atomicVector,ddiMatrix-method}
\alias{cbind2,atomicVector,ldiMatrix-method}
\alias{rbind2,ddiMatrix,atomicVector-method}
\alias{rbind2,ldiMatrix,atomicVector-method}
\alias{rbind2,atomicVector,ddiMatrix-method}
\alias{rbind2,atomicVector,ldiMatrix-method}
\alias{diag,diagonalMatrix-method}
% \alias{diag,ddiMatrix-method}
% \alias{diag,ldiMatrix-method}
%
\alias{Ops,ddiMatrix,sparseMatrix-method}
\alias{Ops,sparseMatrix,ddiMatrix-method}
\alias{Ops,ldiMatrix,sparseMatrix-method}
\alias{Ops,sparseMatrix,ldiMatrix-method}
\alias{Ops,ddiMatrix,numeric-method}
\alias{Ops,numeric,ddiMatrix-method}
\alias{Ops,ldiMatrix,numeric-method}
\alias{Ops,numeric,ldiMatrix-method}
%
\alias{Ops,ddiMatrix,ANY-method}
\alias{Ops,ANY,ddiMatrix-method}
\alias{Ops,ldiMatrix,ANY-method}
\alias{Ops,ANY,ldiMatrix-method}
\alias{Ops,ddiMatrix,Matrix-method}
\alias{Ops,Matrix,ddiMatrix-method}
\alias{Ops,ldiMatrix,Matrix-method}
\alias{Ops,Matrix,ldiMatrix-method}
\alias{Ops,ddiMatrix,dMatrix-method}
\alias{Ops,dMatrix,ddiMatrix-method}
\alias{Ops,ldiMatrix,dMatrix-method}
\alias{Ops,dMatrix,ldiMatrix-method}
%
\alias{Ops,ddiMatrix,ddiMatrix-method}
\alias{Ops,ddiMatrix,ldiMatrix-method}
\alias{Ops,ldiMatrix,ddiMatrix-method}
\alias{Ops,ldiMatrix,ldiMatrix-method}
\alias{Arith,numeric,ddiMatrix-method}
\alias{Arith,numeric,ldiMatrix-method}
\alias{Arith,ddiMatrix,numeric-method}
\alias{Arith,ldiMatrix,numeric-method}

\alias{all,ddiMatrix-method}
\alias{all,ldiMatrix-method}
\alias{any,ddiMatrix-method}
\alias{any,ldiMatrix-method}
\alias{prod,ddiMatrix-method}
\alias{prod,ldiMatrix-method}
\alias{sum,ddiMatrix-method}
\alias{sum,ldiMatrix-method}

%%--"hack"-- for all these signatures explicitly (in loop in ../R/diagMatrix.R ):

%% <[dln]denseMatrix o [dl]diMatrix :
\alias{^,ddenseMatrix,ddiMatrix-method}
\alias{^,ddenseMatrix,ldiMatrix-method}
\alias{^,ddiMatrix,ddenseMatrix-method}
\alias{^,ddiMatrix,ldenseMatrix-method}
\alias{^,ddiMatrix,ndenseMatrix-method}
\alias{^,ldenseMatrix,ddiMatrix-method}
\alias{^,ldenseMatrix,ldiMatrix-method}
\alias{^,ldiMatrix,ddenseMatrix-method}
\alias{^,ldiMatrix,ldenseMatrix-method}
\alias{^,ldiMatrix,ndenseMatrix-method}
\alias{^,ndenseMatrix,ddiMatrix-method}
\alias{^,ndenseMatrix,ldiMatrix-method}
\alias{/,ddiMatrix,ddenseMatrix-method}
\alias{/,ddiMatrix,ldenseMatrix-method}
\alias{/,ddiMatrix,ndenseMatrix-method}
\alias{/,ldiMatrix,ddenseMatrix-method}
\alias{/,ldiMatrix,ldenseMatrix-method}
\alias{/,ldiMatrix,ndenseMatrix-method}
\alias{*,ddenseMatrix,ddiMatrix-method}
\alias{*,ddenseMatrix,ldiMatrix-method}
\alias{*,ddiMatrix,ddenseMatrix-method}
\alias{*,ddiMatrix,ldenseMatrix-method}
\alias{*,ddiMatrix,ndenseMatrix-method}
\alias{*,ldenseMatrix,ddiMatrix-method}
\alias{*,ldenseMatrix,ldiMatrix-method}
\alias{*,ldiMatrix,ddenseMatrix-method}
\alias{*,ldiMatrix,ldenseMatrix-method}
\alias{*,ldiMatrix,ndenseMatrix-method}
\alias{*,ndenseMatrix,ddiMatrix-method}
\alias{*,ndenseMatrix,ldiMatrix-method}
\alias{&,ddenseMatrix,ddiMatrix-method}
\alias{&,ddenseMatrix,ldiMatrix-method}
\alias{&,ddiMatrix,ddenseMatrix-method}
\alias{&,ddiMatrix,ldenseMatrix-method}
\alias{&,ddiMatrix,ndenseMatrix-method}
\alias{&,ldenseMatrix,ddiMatrix-method}
\alias{&,ldenseMatrix,ldiMatrix-method}
\alias{&,ldiMatrix,ddenseMatrix-method}
\alias{&,ldiMatrix,ldenseMatrix-method}
\alias{&,ldiMatrix,ndenseMatrix-method}
\alias{&,ndenseMatrix,ddiMatrix-method}
\alias{&,ndenseMatrix,ldiMatrix-method}
\alias{\%/\%,ddiMatrix,ddenseMatrix-method}
\alias{\%/\%,ddiMatrix,ldenseMatrix-method}
\alias{\%/\%,ddiMatrix,ndenseMatrix-method}
\alias{\%/\%,ldiMatrix,ddenseMatrix-method}
\alias{\%/\%,ldiMatrix,ldenseMatrix-method}
\alias{\%/\%,ldiMatrix,ndenseMatrix-method}
\alias{\%\%,ddiMatrix,ddenseMatrix-method}
\alias{\%\%,ddiMatrix,ldenseMatrix-method}
\alias{\%\%,ddiMatrix,ndenseMatrix-method}
\alias{\%\%,ldiMatrix,ddenseMatrix-method}
\alias{\%\%,ldiMatrix,ldenseMatrix-method}
\alias{\%\%,ldiMatrix,ndenseMatrix-method}
%% Matrix o [dl]diMatrix :
\alias{^,Matrix,ddiMatrix-method}
\alias{^,Matrix,ldiMatrix-method}
\alias{^,ddiMatrix,Matrix-method}
\alias{^,ldiMatrix,Matrix-method}
\alias{/,ddiMatrix,Matrix-method}
\alias{/,ldiMatrix,Matrix-method}
\alias{*,Matrix,ddiMatrix-method}
\alias{*,Matrix,ldiMatrix-method}
\alias{*,ddiMatrix,Matrix-method}
\alias{*,ldiMatrix,Matrix-method}
\alias{&,Matrix,ddiMatrix-method}
\alias{&,Matrix,ldiMatrix-method}
\alias{&,ddiMatrix,Matrix-method}
\alias{&,ldiMatrix,Matrix-method}
\alias{\%/\%,ddiMatrix,Matrix-method}
\alias{\%/\%,ldiMatrix,Matrix-method}
\alias{\%\%,ddiMatrix,Matrix-method}
\alias{\%\%,ldiMatrix,Matrix-method}
%>
%-end{H.Hack}
\alias{Ops,diagonalMatrix,diagonalMatrix-method}
\alias{Ops,diagonalMatrix,ddiMatrix-method}
\alias{Ops,ddiMatrix,diagonalMatrix-method}

\alias{Summary,ddiMatrix-method}
\alias{Summary,ldiMatrix-method}
%
\alias{t,diagonalMatrix-method}
\alias{print,diagonalMatrix-method}
\alias{show,diagonalMatrix-method}
\alias{\%*\%,dgeMatrix,diagonalMatrix-method}
\alias{\%*\%,matrix,diagonalMatrix-method}
\alias{\%*\%,diagonalMatrix,dgeMatrix-method}
\alias{\%*\%,diagonalMatrix,diagonalMatrix-method}
\alias{\%*\%,diagonalMatrix,matrix-method}
\alias{\%*\%,diagonalMatrix,CsparseMatrix-method}
\alias{\%*\%,diagonalMatrix,sparseMatrix-method}
\alias{\%*\%,CsparseMatrix,diagonalMatrix-method}
\alias{\%*\%,sparseMatrix,diagonalMatrix-method}
\alias{\%*\%,denseMatrix,diagonalMatrix-method}
\alias{\%*\%,diagonalMatrix,denseMatrix-method}
\alias{\%*\%,diagonalMatrix,lgeMatrix-method}
\alias{\%*\%,lgeMatrix,diagonalMatrix-method}
\alias{crossprod,CsparseMatrix,diagonalMatrix-method}
\alias{crossprod,diagonalMatrix,CsparseMatrix-method}
\alias{crossprod,diagonalMatrix,dgeMatrix-method}
\alias{crossprod,diagonalMatrix,diagonalMatrix-method}
\alias{crossprod,diagonalMatrix,lgeMatrix-method}
\alias{crossprod,diagonalMatrix,matrix-method}
\alias{crossprod,diagonalMatrix,missing-method}
\alias{crossprod,diagonalMatrix,sparseMatrix-method}
\alias{crossprod,sparseMatrix,diagonalMatrix-method}
\alias{tcrossprod,CsparseMatrix,diagonalMatrix-method}
\alias{tcrossprod,dgeMatrix,diagonalMatrix-method}
\alias{tcrossprod,diagonalMatrix,CsparseMatrix-method}
\alias{tcrossprod,diagonalMatrix,diagonalMatrix-method}
\alias{tcrossprod,diagonalMatrix,matrix-method}
\alias{tcrossprod,diagonalMatrix,missing-method}
\alias{tcrossprod,diagonalMatrix,sparseMatrix-method}
\alias{tcrossprod,lgeMatrix,diagonalMatrix-method}
\alias{tcrossprod,matrix,diagonalMatrix-method}
\alias{tcrossprod,sparseMatrix,diagonalMatrix-method}

\alias{solve,diagonalMatrix,Matrix-method}
\alias{solve,diagonalMatrix,matrix-method}
\alias{solve,diagonalMatrix,missing-method}
\description{
  Class "diagonalMatrix" is the virtual class of all diagonal matrices.
}
\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}
\section{Slots}{
  \describe{
    \item{\code{diag}:}{code{"character"} string, either \code{"U"} or
      \code{"N"}, where \code{"U"} means \sQuote{unit-diagonal}.}
    \item{\code{Dim}:}{matrix dimension, and}
    \item{\code{Dimnames}:}{the \code{\link{dimnames}}, a
      \code{\link{list}}, see the \code{\linkS4class{Matrix}} class
      description.  Typically \code{list(NULL,NULL)} for diagonal matrices.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{sparseMatrix}"}, directly.
}
\section{Methods}{
  These are just a subset of the signature for which defined methods.
  Currently, there are (too) many explicit methods defined in order to
  ensure efficient methods for diagonal matrices.

  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "diagonalMatrix")}: ... }
    \item{coerce}{\code{signature(from = "Matrix", to = "diagonalMatrix")}: ... }
    \item{coerce}{\code{signature(from = "diagonalMatrix", to = "generalMatrix")}: ... }
    \item{coerce}{\code{signature(from = "diagonalMatrix", to = "triangularMatrix")}: ... }
    \item{coerce}{\code{signature(from = "diagonalMatrix", to = "nMatrix")}: ... }
    \item{coerce}{\code{signature(from = "diagonalMatrix", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "diagonalMatrix", to = "sparseVector")}: ... }
    \item{t}{\code{signature(x = "diagonalMatrix")}: ... }
    \item{\%*\%}{\code{signature(x = "dgeMatrix", y = "diagonalMatrix")}: ... }
    \item{\%*\%}{\code{signature(x = "matrix", y = "diagonalMatrix")}: ... }
    \item{\%*\%}{\code{signature(x = "diagonalMatrix", y = "matrix")}: ... }
    \item{\%*\%}{\code{signature(x = "diagonalMatrix", y = "dgeMatrix")}: ... }

    \item{crossprod}{\code{signature(x = "diagonalMatrix", y = "dgeMatrix")}: ... }
    \cr and many more methods

    \item{solve}{\code{signature(a = "diagonalMatrix", b, ...)}: is
      trivially implemented, of course.}

    \item{*}{\code{signature(e1 = "ddiMatrix", e2="denseMatrix")}:
      arithmetic and other operators from the \code{\link[methods]{Ops}}
      group have a few dozen explicit method definitions, in order to
      keep the results \emph{diagonal} in many cases, including the following:}
    \item{/}{\code{signature(e1 = "ddiMatrix", e2="denseMatrix")}:
      the result is from class \code{\linkS4class{ddiMatrix}} which is
      typically very desirable.  Note that when \code{e2} contains
      off-diagonal zeros or \code{\link{NA}}s, we implicitly use \eqn{0 / x = 0}, hence
      differing from traditional \R arithmetic (where \eqn{0 / 0
      \mapsto \mbox{NaN}}{0/0 |-> NaN}), in order to preserve sparsity.}
  }
}
\seealso{
  \code{\linkS4class{ddiMatrix}} and \code{\linkS4class{ldiMatrix}} are
  \dQuote{actual} classes extending \code{"diagonalMatrix"}.
}
\examples{
I5 <- Diagonal(5)
D5 <- Diagonal(x = 10*(1:5))
## trivial (but explicitly defined) methods:
stopifnot(identical(crossprod(I5), I5),
          identical(tcrossprod(I5), I5),
          identical(crossprod(I5, D5), D5),
          identical(tcrossprod(D5, I5), D5),
          identical(solve(D5), solve(D5, I5)),
          all.equal(D5, solve(solve(D5)), tol = 1e-12)
          )
solve(D5)
}
\keyword{classes}
