\name{RsparseMatrix-class}
\docType{class}
\alias{RsparseMatrix-class}
%
\alias{coerce,RsparseMatrix,CsparseMatrix-method}
\alias{coerce,RsparseMatrix,TsparseMatrix-method}
\alias{coerce,RsparseMatrix,denseMatrix-method}
\alias{coerce,RsparseMatrix,dgeMatrix-method}
\alias{coerce,RsparseMatrix,generalMatrix-method}
\alias{coerce,RsparseMatrix,matrix-method}
\alias{coerce,RsparseMatrix,dsparseMatrix-method}
\alias{coerce,RsparseMatrix,lsparseMatrix-method}
\alias{coerce,RsparseMatrix,nsparseMatrix-method}
\alias{coerce,RsparseMatrix,dMatrix-method}
\alias{coerce,RsparseMatrix,lMatrix-method}
\alias{coerce,RsparseMatrix,nMatrix-method}
\alias{coerce,matrix,RsparseMatrix-method}
\alias{coerce,denseMatrix,RsparseMatrix-method}
\alias{coerce,sparseMatrix,RsparseMatrix-method}
% all other ..RMatrix coercions -- here as well for now --
\alias{coerce,matrix,dgRMatrix-method}
%
\alias{t,RsparseMatrix-method}
%
\title{Class "RsparseMatrix" of Sparse Matrices in Column-compressed Form}
\description{The \code{"RsparseMatrix"} class is the virtual class of
  all sparse matrices coded in sorted compressed row-oriented form.
  Since it is a virtual class, no objects may be created from it.  See
  \code{showClass("RsparseMatrix")} for its subclasses.
}
\section{Slots}{
  \describe{
    \item{\code{j}:}{Object of class \code{"integer"} of length \code{nnzero}
      (number of non-zero elements).  These are the row numbers for
      each non-zero element in the matrix.}
    \item{\code{p}:}{Object of class \code{"integer"} of pointers, one
      for each row, to the initial (zero-based) index of elements in
      the row.}
    \item{\code{Dim}, \code{Dimnames}:}{inherited from
      the superclass, see \code{\linkS4class{sparseMatrix}}.}
  }
}
\section{Extends}{
  Class \code{"sparseMatrix"}, directly.
  Class \code{"Matrix"}, by class \code{"sparseMatrix"}.
}
\section{Methods}{
  Only \bold{few} methods are defined currently on purpose, since we
  rather use the \code{\linkS4class{CsparseMatrix}} in \pkg{Matrix}.
  Recently, more methods were added but \emph{beware} that these
  typically do \emph{not} return \code{"RsparseMatrix"} results, but
  rather Csparse* or Tsparse* ones.
  \describe{
    \item{t}{\code{signature(x = "RsparseMatrix")}: ... }
    \item{coerce}{\code{signature(from = "RsparseMatrix", to = "CsparseMatrix")}: ... }
    \item{coerce}{\code{signature(from = "RsparseMatrix", to = "TsparseMatrix")}: ... }
  }
}
\seealso{
  its superclass, \code{\linkS4class{sparseMatrix}}, and, e.g., class
  \code{\linkS4class{dgRMatrix}} for the links to other classes.
}
\examples{
showClass("RsparseMatrix")
}
\keyword{classes}
