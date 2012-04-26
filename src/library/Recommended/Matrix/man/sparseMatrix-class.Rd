\name{sparseMatrix-class}
\docType{class}
\title{Virtual Class "sparseMatrix" --- Mother of Sparse Matrices}
\alias{sparseMatrix-class}
%% Group methods
\alias{Math,sparseMatrix-method}
%\alias{Math2,sparseMatrix,numeric-method}
\alias{Ops,sparseMatrix,sparseMatrix-method}
\alias{Ops,sparseMatrix,numeric-method}
\alias{Ops,numeric,sparseMatrix-method}
\alias{Ops,diagonalMatrix,sparseMatrix-method}
\alias{Ops,sparseMatrix,diagonalMatrix-method}
%
\alias{cbind2,sparseMatrix,sparseMatrix-method}
\alias{cbind2,denseMatrix,sparseMatrix-method}
\alias{cbind2,sparseMatrix,denseMatrix-method}
\alias{cbind2,sparseMatrix,matrix-method}
\alias{cbind2,matrix,sparseMatrix-method}
\alias{cbind2,sparseMatrix,numeric-method}
\alias{cbind2,numeric,sparseMatrix-method}
\alias{rbind2,sparseMatrix,sparseMatrix-method}
\alias{rbind2,denseMatrix,sparseMatrix-method}
\alias{rbind2,sparseMatrix,denseMatrix-method}
\alias{rbind2,sparseMatrix,matrix-method}
\alias{rbind2,matrix,sparseMatrix-method}
\alias{rbind2,sparseMatrix,numeric-method}
\alias{rbind2,numeric,sparseMatrix-method}
%
\alias{coerce,ANY,sparseMatrix-method}
\alias{coerce,factor,sparseMatrix-method}
\alias{coerce,sparseMatrix,generalMatrix-method}
\alias{coerce,sparseMatrix,symmetricMatrix-method}
\alias{coerce,sparseMatrix,triangularMatrix-method}
\alias{-,sparseMatrix,missing-method}
\alias{cov2cor,sparseMatrix-method}
\alias{diag,sparseMatrix-method}
\alias{dim<-,sparseMatrix-method}
\alias{format,sparseMatrix-method}
\alias{lu,sparseMatrix-method}
\alias{mean,sparseMatrix-method}
\alias{print,sparseMatrix-method}
\alias{show,sparseMatrix-method}
\alias{summary,sparseMatrix-method}
\alias{norm,sparseMatrix,character-method}
\alias{determinant,dgCMatrix,logical-method}
\alias{determinant,dsparseMatrix,logical-method}
\alias{determinant,dtCMatrix,logical-method}
\alias{determinant,sparseMatrix,missing-method}
\alias{determinant,sparseMatrix,logical-method}

% "[" are in ./Xtrct-methods.Rd
%\alias{colMeans,..} etc are now in ./colSums.Rd
% graph stuff
\alias{coerce,graph,Matrix-method}
\alias{coerce,graph,sparseMatrix-method}
\alias{coerce,graph,CsparseMatrix-method}
\alias{coerce,graphAM,sparseMatrix-method}
\alias{coerce,graphNEL,CsparseMatrix-method}
\alias{coerce,graphNEL,TsparseMatrix-method}
\alias{coerce,sparseMatrix,graph-method}
\alias{coerce,sparseMatrix,graphNEL-method}
\alias{coerce,TsparseMatrix,graphNEL-method}
\alias{coerce,dgTMatrix,graphNEL-method}
%
\description{Virtual Mother Class of All Sparse Matrices}
\section{Slots}{
  \describe{
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
    \item{\code{Dimnames}:}{a list of length two - inherited from class
      \code{Matrix}, see \code{\linkS4class{Matrix}}.}
  }
}
\section{Extends}{
  Class \code{"Matrix"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{(object = "sparseMatrix")}: The
      \code{\link{show}} method for sparse matrices prints
      \emph{\dQuote{structural}} zeroes as \code{"."} using
      \code{\link{printSpMatrix}()} which allows further customization.}
    \item{print}{\code{signature(x = "sparseMatrix")}, ....\cr
      The \code{\link{print}} method for sparse matrices by default is the
      same as \code{show()} but can be called with extra optional
      arguments, see \code{\link{printSpMatrix}()}.}
    \item{format}{\code{signature(x = "sparseMatrix")}, ....\cr
      The \code{\link{format}} method for sparse matrices, see
      \code{\link{formatSpMatrix}()} for details such as the extra
      optional arguments.}
    \item{summary}{\code{(object = "sparseMatrix")}: Returns
      an object of S3 class \code{"sparseSummary"} which is basically a
      \code{\link{data.frame}} with columns \code{(i,j,x)} (or just
      \code{(i,j)} for \code{\linkS4class{nsparseMatrix}} class objects)
      with the stored (typically non-zero) entries.  The
      \code{\link{print}} method resembles Matlab's way of printing
      sparse matrices, and also the MatrixMarket format, see
      \code{\link{writeMM}}.}
    \item{cbind2}{\code{(x = *, y = *)}: several methods for binding
      matrices together, column-wise, see the basic \code{\link{cbind}}
      and \code{\link{rbind}} functions.\cr
      Note that the result will typically be sparse, even when one
      argument is dense and larger than the sparse one.
    }
    \item{rbind2}{\code{(x = *, y = *)}: binding matrices together
      row-wise, see \code{cbind2} above.}
    \item{determinant}{\code{(x = "sparseMatrix", logarithm=TRUE)}:
      \code{\link{determinant}()} methods for sparse matrices typically
      work via \code{\link{Cholesky}} or \code{\link{lu}} decompositions.}
    \item{diag}{\code{(x = "sparseMatrix")}: extracts the diagonal of a
      sparse matrix.}
    \item{dim<-}{\code{signature(x = "sparseMatrix", value = "ANY")}:
      allows to \emph{reshape} a sparse matrix to a sparse matrix with
      the same entries but different dimensions. \code{value} must be of
      length two and fulfill \code{prod(value) == prod(dim(x))}.}
    \item{coerce}{\code{signature(from = "factor", to = "sparseMatrix")}:
      Coercion of a factor to \code{"sparseMatrix"} produces the matrix
      of indicator \bold{rows} stored as an object of class
      \code{"dgCMatrix"}.  To obtain columns representing the interaction
      of the factor and a numeric covariate, replace the \code{"x"} slot
      of the result by the numeric covariate then take the transpose.
      Missing values (\code{\link{NA}}) from the factor are translated
      to columns of all \code{0}s.}
  }
  See also \code{\link{colSums}}, \code{\link{norm}},
  ... %% FIXME
  for methods with separate help pages.
}
\note{
  In method selection for multiplication operations (i.e. \code{\%*\%}
  and the two-argument form of \code{\link[base]{crossprod}})
  the sparseMatrix class takes precedence in the sense that if one
  operand is a sparse matrix and the other is any type of dense matrix
  then the dense matrix is coerced to a \code{dgeMatrix} and the
  appropriate sparse matrix method is used.
}
%\author{Martin}
\examples{
showClass("sparseMatrix") ## and look at the help() of its subclasses
M <- Matrix(0, 10000, 100)
M[1,1] <- M[2,3] <- 3.14
M  ## show(.) method suppresses printing of the majority of rows

data(CAex); dim(CAex) # 72 x 72 matrix
determinant(CAex) # works via sparse lu(.)

## factor -> t( <sparse design matrix> ) :
(fact <- gl(5, 3, 30, labels = LETTERS[1:5]))
(Xt <- as(fact, "sparseMatrix"))  # indicator rows

## missing values --> all-0 columns:
f.mis <- fact
i.mis <- c(3:5, 17)
is.na(f.mis) <- i.mis
Xt != (X. <- as(f.mis, "sparseMatrix")) # differ only in columns 3:5,17
stopifnot(all(X.[,i.mis] == 0), all(Xt[,-i.mis] == X.[,-i.mis]))
}
\keyword{classes}
