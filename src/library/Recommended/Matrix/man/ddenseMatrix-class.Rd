\name{ddenseMatrix-class}
\docType{class}
\alias{ddenseMatrix-class}
\title{Virtual Class "ddenseMatrix" of Numeric Dense Matrices}
% now separated from ./dgeMatrix-class.Rd - on purpose
% METHODS:
% undocu (FIXME)
\alias{\%*\%,ddenseMatrix,ddenseMatrix-method}
\alias{crossprod,ddenseMatrix,missing-method}
\alias{crossprod,ddenseMatrix,dgCMatrix-method}
\alias{tcrossprod,ddenseMatrix,missing-method}
%
\alias{show,ddenseMatrix-method}
\alias{coerce,ddenseMatrix,matrix-method}
\alias{coerce,ddenseMatrix,dgeMatrix-method}
%\alias{coerce,ddenseMatrix,CsparseMatrix-method}
%
% Group
\alias{Arith,ddenseMatrix,ddenseMatrix-method}
\alias{Arith,ddenseMatrix,logical-method}
\alias{Arith,ddenseMatrix,numeric-method}
\alias{Arith,logical,ddenseMatrix-method}
\alias{Arith,numeric,ddenseMatrix-method}
\alias{Math,ddenseMatrix-method}
\alias{Summary,ddenseMatrix-method}
%
\alias{as.numeric,ddenseMatrix-method}
\alias{diag,ddenseMatrix-method}
\alias{determinant,ddenseMatrix,missing-method}
\alias{determinant,ddenseMatrix,logical-method}
\alias{lu,ddenseMatrix-method}
\alias{norm,ddenseMatrix,missing-method}
\alias{norm,ddenseMatrix,character-method}
\alias{rcond,ddenseMatrix,missing-method}
\alias{rcond,ddenseMatrix,character-method}
\alias{solve,ddenseMatrix,missing-method}
\alias{solve,ddenseMatrix,numeric-method}
\alias{solve,ddenseMatrix,matrix-method}
\alias{solve,ddenseMatrix,Matrix-method}
\alias{solve,ddenseMatrix,ANY-method}
\alias{t,ddenseMatrix-method}
%
\description{This is the virtual class of all dense numeric (i.e.,
  \bold{d}ouble, hence \emph{\dQuote{ddense}}) S4 matrices.

  Its most important subclass is the \code{\linkS4class{dgeMatrix}} class.
  %% and now say what the difference is ! __ FIXME __
}
\section{Extends}{
  Class \code{"dMatrix"} directly;
  class \code{"Matrix"}, by the above.
}
\section{Slots}{
  the same slots at its subclass \code{\linkS4class{dgeMatrix}}, see
  there.
}
\section{Methods}{
  Most methods are implemented via \code{as(*, "dgeMatrix")} and are
  mainly used as \dQuote{fallbacks} when the subclass doesn't need its
  own specialized method.

  Use \code{\link{showMethods}(class = "ddenseMatrix", where =
      "package:Matrix")} for an overview.
}
%\references{}
%\note{}
\seealso{
  The virtual classes \code{\linkS4class{Matrix}},
  \code{\linkS4class{dMatrix}}, and \code{\linkS4class{dsparseMatrix}}.
}
\examples{
showClass("ddenseMatrix")

showMethods(class = "ddenseMatrix", where = "package:Matrix")
}
\keyword{classes}
