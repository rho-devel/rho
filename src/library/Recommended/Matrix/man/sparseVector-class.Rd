%%----- Docu for *all* sparse vector classes ----------
\name{sparseVector-class}
\docType{class}
\title{Sparse Vector Classes}
\alias{sparseVector-class}
% sub classes
\alias{dsparseVector-class}
\alias{isparseVector-class}
\alias{lsparseVector-class}
\alias{nsparseVector-class}
\alias{zsparseVector-class}
\alias{xsparseVector-class}% the class union
%% Group methods
% \alias{Math,sparseVector-method}
% %\alias{Math2,sparseVector,numeric-method}
\alias{Arith,sparseVector,sparseVector-method}
\alias{Arith,dsparseVector,dsparseVector-method}
\alias{-,dsparseVector,missing-method}
\alias{Logic,sparseVector,sparseVector-method}
\alias{Logic,lsparseVector,lsparseVector-method}
\alias{Ops,ANY,sparseVector-method}
\alias{Ops,sparseVector,ANY-method}
\alias{Ops,sparseVector,atomicVector-method}
\alias{Ops,atomicVector,sparseVector-method}
%
\alias{coerce,ANY,sparseVector-method}
\alias{coerce,TsparseMatrix,sparseVector-method}
\alias{coerce,diagonalMatrix,sparseVector-method}
\alias{coerce,sparseMatrix,sparseVector-method}
\alias{coerce,atomicVector,sparseVector-method}
\alias{coerce,atomicVector,dsparseVector-method}
\alias{coerce,triangularVector,sparseVector-method}
\alias{coerce,sparseVector,vector-method}
\alias{coerce,xsparseVector,dsparseVector-method}
\alias{coerce,xsparseVector,isparseVector-method}
\alias{coerce,xsparseVector,lsparseVector-method}
\alias{coerce,xsparseVector,zsparseVector-method}
\alias{coerce,xsparseVector,nsparseVector-method}
%
\alias{as.numeric,sparseVector-method}
\alias{as.vector,sparseVector,missing-method}
\alias{as.vector,sparseVector,character-method}
\alias{dim<-,sparseVector-method}
\alias{length,sparseVector-method}
\alias{rep,sparseVector-method}
\alias{show,sparseVector-method}
% ../R/products.R :
\alias{\%*\%,Matrix,sparseVector-method}
\alias{\%*\%,sparseVector,Matrix-method}
\alias{crossprod,Matrix,sparseVector-method}
\alias{crossprod,sparseVector,Matrix-method}
\alias{solve,Matrix,sparseVector-method}
\alias{tcrossprod,Matrix,sparseVector-method}
\alias{tcrossprod,sparseVector,Matrix-method}
%
\alias{[,sparseVector,index,ANY,ANY-method}
\alias{[<-,sparseVector,index,missing,replValue-method}
%
\description{Sparse Vector Classes: The virtual mother class
  \code{"sparseVector"} has the five actual daughter classes
   \code{"dsparseVector"}, \code{"isparseVector"},
   \code{"lsparseVector"}, \code{"nsparseVector"}, and
   \code{"zsparseVector"}, where we've mainly implemented methods for
   the \code{d*}, \code{l*} and \code{n*} ones.
}
\section{Slots}{
  \describe{
    \item{\code{length}:}{class \code{"integer"} - the \code{\link{length}}
     of the sparse vector.}
    \item{\code{i}:}{class \code{"integer"} - the (1-based) indices of
      the non-zero entries.}
    \item{\code{x}:}{(for all but \code{"nsparseVector"}):
      the non-zero entries.  This is of class \code{"numeric"} for class
      \code{"dsparseVector"},  \code{"logical"} for class
      \code{"lsparseVector"}, etc.

      Note that \code{"nsparseVector"}s have no \code{x} slot.
      Further, mainly for ease of method definitions, we've defined the
      class union (see \code{\link{setClassUnion}}) of all sparse vector
      classes which \emph{have} an \code{x} slot, as class \code{"xsparseVector"}.
      }
  }
}
\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "sparseVector")}: simply extracts
      the \code{length} slot.}
    \item{show}{\code{signature(object = "sparseVector")}: The
      \code{\link{show}} method for sparse vectors prints
      \emph{\dQuote{structural}} zeroes as \code{"."} using the
      non-exported \code{prSpVector} function which allows further
      customization such as replacing \code{"."} by \code{" "} (blank).

      Note that \code{\link{options}(max.print)} will influence how many
      entries of large sparse vectors are printed at all.}
    \item{as.vector}{\code{signature(x = "sparseVector", mode = "character")}
      coerces sparse vectors to \dQuote{regular}, i.e., atomic vectors.
      This is the same as \code{as(x, "vector")}.}
    \item{dim<-}{\code{signature(x = "sparseVector", value = "integer")}
      coerces a sparse vector to a sparse Matrix, i.e., an object
      inheriting from \code{\linkS4class{sparseMatrix}}, of the
      appropriate dimension.}
    \item{rep}{\code{signature(x = "sparseVector")} repeat \code{x},
      with the same argument list \code{(x, times, length.out, each,
      ...)} as the default method for rep().}
  }
}
%\author{Martin}
\examples{
getClass("sparseVector")
getClass("dsparseVector")
getClass("xsparseVector")# those with an 'x' slot

sx <- c(0,0,3, 3.2, 0,0,0,-3:1,0,0,2,0,0,5,0,0)
(ss <- as(sx, "sparseVector"))

ix <- as.integer(round(sx))
(is <- as(ix, "sparseVector"))
## an "isparseVector" (!)

## rep() works too:
rep(is, length.out= 25)

## Using `dim<-`  as in base R :
r <- ss
dim(r) <- c(4,5) # becomes a sparse Matrix:
r

}
\keyword{classes}
