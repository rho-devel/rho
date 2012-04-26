\name{CHMfactor-class}
\title{CHOLMOD-based Cholesky Factorizations}
\docType{class}
\alias{CHMfactor-class}% "mother" virtual
\alias{CHMsimpl-class}% virtual
\alias{CHMsuper-class}% virtual
\alias{dCHMsimpl-class}
\alias{nCHMsimpl-class}
\alias{dCHMsuper-class}
\alias{nCHMsuper-class}
\alias{isLDL}
%
\alias{coerce,CHMfactor,Matrix-method}
\alias{coerce,CHMfactor,sparseMatrix-method}
\alias{coerce,CHMfactor,pMatrix-method}
\alias{expand,CHMfactor-method}
\alias{solve,CHMfactor,ddenseMatrix-method}
\alias{solve,CHMfactor,dsparseMatrix-method}
\alias{solve,CHMfactor,diagonalMatrix-method}
\alias{solve,CHMfactor,numeric-method}
\alias{solve,CHMfactor,matrix-method}
\alias{solve,CHMfactor,missing-method}
\alias{solve,CHMfactor,ANY-method}
\alias{determinant,CHMfactor,missing-method}
\alias{determinant,CHMfactor,logical-method}
\alias{update,CHMfactor-method}
%
\description{The virtual class \code{"CHMfactor"} is a class of
  CHOLMOD-based Cholesky factorizations of symmetric, sparse,
  compressed, column-oriented matrices.  Such a factorization is
  simplicial (virtual class \code{"CHMsimpl"}) or supernodal (virtual
  class \code{"CHMsuper"}).  Objects that inherit from these classes are
  either numeric factorizations (classes \code{"dCHMsimpl"} and
  \code{"dCHMsuper"}) or symbolic factorizations (classes
  \code{"nCHMsimpl"} and \code{"nCHMsuper"}).
}
\usage{% want usage for the update method which has "surprising arguments"
\S4method{update}{CHMfactor}(object, parent, mult = 0, \dots)
isLDL(x)
}
\arguments{
  \item{object,x}{a \code{"CHMfactor"} object.}
  \item{parent}{a \code{"\linkS4class{dsCMatrix}"} matrix or
    \code{"\linkS4class{dgCMatrix}"} object with the same nonzero
    pattern as the matrix that generated \code{object}.  If
    \code{parent} is a symmetric matrix (class
    \code{"\linkS4class{dsCMatrix}"}) then \code{object} should be a
    decomposition of a matrix with the same nonzero pattern as
    \code{parent}.  If \code{parent} is not symmetric then \code{object}
    should be the decomposition of a matrix with the same nonzero
    pattern as \code{tcrossprod(parent)}.}
  \item{mult}{a numeric scalar (default 0).  \code{mult} times the
    identity matrix is (implicitly) added to \code{parent} or
    \code{tcrossprod(parent)} before updating the decomposition
    \code{object}.}
  \item{\dots}{Optional additional arguments to the methods.}
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dCHMsuper", ...)}
  but are more commonly created via \code{\link{Cholesky}()},
  applied to  \code{\linkS4class{dsCMatrix}} or
  \code{\linkS4class{lsCMatrix}} objects.
}
\section{Slots}{
  of \code{"CHMfactor"} and all classes inheriting from it:
  \describe{
    \item{\code{perm}:}{An integer vector giving the 0-based
      permutation of the rows and columns chosen to reduce fill-in and
      for post-ordering.}
    \item{\code{colcount}:}{Object of class \code{"integer"} .... }%% FIXME
    \item{\code{type}:}{Object of class \code{"integer"} .... }
  }

  Slots of the non virtual classes \dQuote{[dl]CHM(super|simpl)}:
  \describe{
    \item{\code{p}:}{Object of class \code{"integer"} of pointers, one
      for each column, to the initial (zero-based) index of elements in
      the column. Only present in classes that contain \code{"CHMsimpl"}.}
    \item{\code{i}:}{Object of class \code{"integer"} of length nnzero
      (number of non-zero elements).  These are the row numbers for
      each non-zero element in the matrix.  Only present in classes that
      contain \code{"CHMsimpl"}.}
    \item{\code{x}:}{For the \code{"d*"} classes: \code{"numeric"} - the
      non-zero elements of the matrix.}
  }
}
\section{Methods}{
  \describe{
    \item{isLDL}{\code{(x)} returns a \code{\link{logical}} indicating if
      \code{x} is an \eqn{LDL'} decomposition or (when \code{FALSE}) an
      \eqn{LL'} one.}
    \item{coerce}{\code{signature(from = "CHMfactor", to = "sparseMatrix")}
      Returns the lower triangular factor \eqn{L} from the \eqn{LL'}
      form of the Cholesky factorization.  Note that (currently) the
      factor from the \eqn{LL'} form is always returned, even if the
      \code{"CHMfactor"} object represents an \eqn{LDL'} decomposition.
      Furthermore, this is the factor after any fill-reducing
      permutation has been applied.  See the \code{expand} method for
      obtaining both the permutation matrix, \eqn{P}, and the lower
      Cholesky factor, \eqn{L}.}
    \item{coerce}{\code{signature(from = "CHMfactor", to =
	"pMatrix")} Returns the permutation matrix, \eqn{P},
      representing the fill-reducing permutation used in the
      decomposition.}
    \item{expand}{\code{signature(x = "CHMfactor")} Returns a list with
      components \code{P}, the matrix representing the fill-reducing
      permutation, and \code{L}, the lower triangular Cholesky factor.
      The original positive-definite matrix corresponds to the product
      \eqn{P'LL'P}. Because of fill-in during the decomposition the
      product may apparently have more non-zeros than the original
      matrix, even after applying \code{\link{drop0}} to it. However,
      the extra "non-zeros" should be very small in magnitude.}
    \item{image}{\code{signature(x = "CHMfactor")} Plot the image of the
      lower triangular factor, \eqn{L}, from the decomposition.  This method
      is equivalent to \code{image(as(x, "sparseMatrix"))} so the
      comments in the above description of the \code{coerce} method
      apply here too.}
    \item{solve}{\code{signature(a = "CHMfactor", b = "ddenseMatrix")}
      The \code{solve} methods for a \code{"CHMfactor"} object take an
      optional third argument \code{system} whose value can be one of the
      character strings \code{"A"}, \code{"LDLt"}, \code{"LD"},
      \code{"DLt"}, \code{"L"}, \code{"Lt"}, \code{"D"}, \code{"P"} or
      \code{"Pt"}.  This argument describes the system to be solved.  The
      default,  \code{"A"}, is to solve \eqn{Ax = b} for \eqn{x} where
      \code{A} is sparse, positive-definite matrix that was factored to produce
      \code{a}.  Analogously, \code{system = "L"} returns the solution
      \eqn{x}, of \eqn{Lx = b}; similarly, for all system codes
      \bold{but} \code{"P"} and \code{"Pt"} where, e.g., \code{x <-
	solve(a, b,system="P")} is equivalent to \code{x <- P \%*\% b}.}
    \item{solve}{\code{signature(a = "CHMfactor", b = "matrix")}, and}
    \item{solve}{\code{signature(a = "CHMfactor", b = "numeric")} call
      the above \code{signature(*, b = "ddenseMatrix")} method.}

    \item{solve}{\code{signature(a = "CHMfactor", b = "dsparseMatrix")}
      takes the same third argument \code{system} and calls the
      corresponding sparse CHOLMOD algorithm.}

    \item{determinant}{\code{signature(x = "CHMfactor", logarithm =
      "logical")} returns the determinant (or the logarithm of the
      determinant, if \code{logarithm = TRUE}, the default) of the
      factor \eqn{L} from the \eqn{LL'} decomposition (even if the
      decomposition represented by \code{x} is of the \eqn{LDL'}
      form (!)).  This is the square root of the determinant (half the
      logarithm of the determinant when \code{logarithm = TRUE}) of the
      positive-definite matrix that was decomposed.}

    %% since 0.999375-8 (2008-03-25):
    \item{update}{\code{signature(object = "CHMfactor")}.  The
      \code{\link{update}} method requires an additional argument
      \code{parent}, which is a \code{"\linkS4class{dsCMatrix}"} object with
      the same structure of nonzeros as the matrix that was decomposed
      to produce \code{object}, and provides an optional argument
      \code{mult}, a numeric scalar.  This method updates the numeric
      values in \code{object} to the decomposition of \eqn{A+mI} where
      \eqn{A} is the matrix represented by \code{parent} and \eqn{m} is
      the scalar \code{mult}. Because only the numeric values are
      updated this method should be faster than creating and decomposing
      \eqn{A+mI}.  It is not uncommon to want, say, the determinant of
      \eqn{A+mI} for many different values of \eqn{m}.  This method
      would be the preferred approach in such cases.}

  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  \code{\link{Cholesky}}, also for examples;
  class \code{\linkS4class{dgCMatrix}}.
}
\examples{% most are in ./Cholesky.Rd
## An example for the expand() method
n <- 1000; m <- 200; nnz <- 2000
set.seed(1)
M1 <- spMatrix(n, m,
               i = sample(n, nnz, replace = TRUE),
               j = sample(m, nnz, replace = TRUE),
               x = round(rnorm(nnz),1))
XX <- crossprod(M1)
CX <- Cholesky(XX)
isLDL(CX)
str(CX) ## a "dCHMsimpl" object
r <- expand(CX)
L.P <- with(r, crossprod(L,P))  ## == L'P
PLLP <- crossprod(L.P)          ## == (L'P)' L'P == P'LL'P
b <- sample(m)
stopifnot(all.equal(PLLP, XX), % not needed: factorsCheck is off: check.attr=FALSE),
          all(as.vector(solve(CX, b, system="P" )) == r$P \%*\% b),
          all(as.vector(solve(CX, b, system="Pt")) == t(r$P) \%*\% b) )

   ## [ See  help(Cholesky)  for more examples ]
}
\keyword{classes}
\keyword{algebra}
