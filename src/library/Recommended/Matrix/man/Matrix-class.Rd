\name{Matrix-class}
\docType{class}
\alias{Matrix-class}
\alias{!,Matrix-method}
%% Group methods:
\alias{-,Matrix,missing-method}
\alias{+,Matrix,missing-method}
\alias{Arith,Matrix,Matrix-method}
\alias{Ops,Matrix,Matrix-method}
\alias{Ops,Matrix,ANY-method}
\alias{Ops,ANY,Matrix-method}
\alias{Ops,Matrix,matrix-method}
\alias{Ops,matrix,Matrix-method}
\alias{Ops,logical,Matrix-method}
\alias{Ops,Matrix,logical-method}
\alias{Logic,logical,Matrix-method}
\alias{Logic,Matrix,logical-method}
\alias{Logic,Matrix,ANY-method}
\alias{Logic,ANY,Matrix-method}
\alias{Summary,Matrix-method}
\alias{mean,Matrix-method}
\alias{\%*\%,Matrix,numeric-method}
\alias{\%*\%,numeric,Matrix-method}
\alias{\%*\%,Matrix,ANY-method}
\alias{\%*\%,ANY,Matrix-method}
\alias{\%*\%,Matrix,matrix-method}
\alias{\%*\%,matrix,Matrix-method}
% "[" are in ./Xtrct-methods.Rd ;  "[<-" in ./Subassign-methods.Rd
\alias{cbind2,ANY,Matrix-method}
\alias{cbind2,Matrix,ANY-method}
\alias{cbind2,Matrix,Matrix-method}
\alias{cbind2,Matrix,NULL-method}
\alias{cbind2,Matrix,atomicVector-method}
\alias{cbind2,Matrix,missing-method}
\alias{cbind2,NULL,Matrix-method}
\alias{cbind2,atomicVector,Matrix-method}
\alias{rbind2,ANY,Matrix-method}
\alias{rbind2,Matrix,ANY-method}
\alias{rbind2,Matrix,Matrix-method}
\alias{rbind2,Matrix,NULL-method}
\alias{rbind2,Matrix,atomicVector-method}
\alias{rbind2,Matrix,missing-method}
\alias{rbind2,NULL,Matrix-method}
\alias{rbind2,atomicVector,Matrix-method}
\alias{crossprod,ANY,Matrix-method}
\alias{crossprod,Matrix,numeric-method}
\alias{crossprod,Matrix,ANY-method}
\alias{crossprod,Matrix,missing-method}
\alias{crossprod,Matrix,Matrix-method}
\alias{crossprod,Matrix,matrix-method}
\alias{crossprod,matrix,Matrix-method}
\alias{crossprod,numeric,Matrix-method}
\alias{tcrossprod,ANY,Matrix-method}
\alias{tcrossprod,Matrix,numeric-method}
\alias{tcrossprod,Matrix,ANY-method}
\alias{tcrossprod,Matrix,missing-method}
\alias{tcrossprod,Matrix,Matrix-method}
\alias{tcrossprod,Matrix,matrix-method}
\alias{tcrossprod,matrix,Matrix-method}
\alias{tcrossprod,numeric,Matrix-method}
%
\alias{cov2cor,Matrix-method}
\alias{det}% "the function" (our copy of base::det)
\alias{determinant,Matrix,missing-method}
\alias{determinant,Matrix,logical-method}
\alias{diag,Matrix-method}
\alias{drop,Matrix-method}
\alias{head,Matrix-method}
\alias{tail,Matrix-method}
\alias{diff,Matrix-method}
\alias{dim,Matrix-method}
\alias{dim<-,Matrix-method}
\alias{dimnames,Matrix-method}
\alias{dimnames<-,Matrix,list-method}
\alias{dimnames<-,Matrix,NULL-method}
\alias{length,Matrix-method}
\alias{show,Matrix-method}
\alias{as.array,Matrix-method}
\alias{as.matrix,Matrix-method}
\alias{as.vector,Matrix,missing-method}
\alias{as.numeric,Matrix-method}
\alias{as.logical,Matrix-method}
\alias{t,Matrix-method}
\alias{unname,Matrix-method}
\alias{coerce,Matrix,vector-method}
\alias{coerce,Matrix,numeric-method}
\alias{coerce,Matrix,logical-method}
\alias{coerce,Matrix,integer-method}
\alias{coerce,Matrix,complex-method}
%
\alias{coerce,Matrix,matrix-method}%
\alias{coerce,matrix,Matrix-method}
\alias{coerce,Matrix,denseMatrix-method}
\alias{coerce,Matrix,CsparseMatrix-method}
\alias{coerce,Matrix,sparseMatrix-method}
\alias{solve,Matrix,ANY-method}
\alias{solve,ANY,Matrix-method}
\alias{solve,Matrix,matrix-method}
\alias{solve,matrix,Matrix-method}
\alias{solve,Matrix,diagonalMatrix-method}
\alias{solve,Matrix,missing-method}
\alias{solve,Matrix,numeric-method}
%\alias{unname,Matrix-method}
\alias{unname,Matrix,missing-method}
%
\title{Virtual Class "Matrix" Class of Matrices}
\description{
  The \code{Matrix} class is a class contained by all actual
  classes in the \pkg{Matrix} package.  It is a \dQuote{virtual} class.
}
\section{Slots}{
  Common to \emph{all} matrix objects in the package:
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
  \describe{
    \item{determinant}{\code{signature(x = "Matrix", logarithm = "missing")}: and}
    \item{determinant}{\code{signature(x = "Matrix", logarithm = "logical")}:
      compute the (\eqn{\log}) determinant of \code{x}.  The method
      chosen depends on the actual Matrix class of \code{x}.  Note that
      \code{\link[base]{det}} also works for all our matrices, calling the
      appropriate \code{determinant()} method.  The \code{Matrix::det}
      is an exact copy of \code{base::det}, but in the correct
      namespace, and hence calling the S4-aware version of
      \code{determinant()}.).}
    \item{diff}{\code{signature(x = "Matrix")}: As \code{\link{diff}()}
      for traditional matrices, i.e., applying \code{diff()} to each
      column.}
    \item{dim}{\code{signature(x = "Matrix")}: extract matrix dimensions
      \code{\link{dim}}.}
    \item{dim<-}{\code{signature(x = "Matrix", value = "ANY")}: where
      \code{value} is integer of length 2.  Allows to \emph{reshape}
      Matrix objects, but only when \code{prod(value) == prod(dim(x))}.}
    \item{dimnames}{\code{signature(x = "Matrix")}: extract
      \code{\link{dimnames}}.}
    \item{dimnames<-}{\code{signature(x = "Matrix", value = "list")}: set
      the \code{dimnames} to a \code{\link{list}} of length 2, see
      \code{\link{dimnames<-}}.}
    \item{length}{\code{signature(x = "Matrix")}: simply defined as
      \code{prod(dim(x))} (and hence of mode \code{"double"}).}
    \item{show}{\code{signature(object = "Matrix")}: \code{\link{show}}
      method for \code{\link{print}}ing.}
    \item{image}{\code{signature(object = "Matrix")}: draws an
      \code{\link{image}} of the matrix entries, using
      \code{\link[lattice]{levelplot}()} from package \pkg{lattice}.}
    \item{head}{\code{signature(object = "Matrix")}: return only the
      \emph{\dQuote{head}}, i.e., the first few rows.}
    \item{tail}{\code{signature(object = "Matrix")}: return only the
      \emph{\dQuote{tail}}, i.e., the last few rows of the respective matrix.}
    \cr %------------------------------------
    \item{as.matrix, as.array}{\code{signature(x = "Matrix")}: the same as
      \code{as(x, "matrix")}; see also the note below.}
    \item{as.vector}{\code{signature(x = "Matrix", mode = "missing")}:
      \code{as.vector(m)} should be identical to \code{as.vector(as(m,
	"matrix"))}, implemented more efficiently for some subclasses.}
    \item{as(x, "vector"), as(x, "numeric")}{etc, similarly.}
  }
  There are many more methods that (conceptually should) work for all
  \code{"Matrix"} objects, e.g., \code{\link{colSums}},
  \code{\link{rowMeans}}.  Even \pkg{base} functions may work
  automagically (if they first call \code{\link{as.matrix}()} on their
  principal argument), e.g., \code{\link{apply}}, \code{\link{eigen}},
  \code{\link{svd}} or \code{\link{kappa}} all do work via coercion to a
  \dQuote{traditional} (dense) \code{\link{matrix}}.
  %% --> ../tests/base-matrix-fun.R
}
\note{
  Loading the \code{Matrix} namespace \dQuote{overloads}
  \code{\link{as.matrix}} and \code{\link{as.array}} in the \pkg{base}
  namespace by the equivalent of \code{function(x) as(x, "matrix")}.
  Consequently, \code{as.matrix(m)} or \code{as.array(m)} will properly
  work when \code{m} inherits from the \code{"Matrix"} class ---
  \emph{also} for functions in package \pkg{base} and other packages.
  E.g., \code{\link{apply}} or \code{\link{outer}} can therefore be applied
  to \code{"Matrix"} matrices.
}
%\references{}
\author{Douglas Bates \email{bates@stat.wisc.edu} and Martin Maechler}
\seealso{
  the classes \code{\linkS4class{dgeMatrix}},
  \code{\linkS4class{dgCMatrix}}, and function
  \code{\link{Matrix}} for construction (and examples).

  Methods, e.g., for \code{\link[=kronecker-methods]{kronecker}}.
}
\examples{
slotNames("Matrix")

cl <- getClass("Matrix")
names(cl@subclasses) # more than 40 ..

showClass("Matrix")#> output with slots and all subclasses

(M <- Matrix(c(0,1,0,0), 6, 4))
dim(M)
diag(M)
cm <- M[1:4,] + 10*Diagonal(4)
diff(M)
## can reshape it even :
dim(M) <- c(2, 12)
M
stopifnot(identical(M, Matrix(c(0,1,0,0), 2,12)),
          all.equal(det(cm),
                    determinant(as(cm,"matrix"), log=FALSE)$modulus,
                    check.attr=FALSE))
}
\keyword{classes}
\keyword{algebra}
