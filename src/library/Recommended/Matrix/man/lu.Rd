\name{lu}
\title{(Generalized) Triangular Decomposition of a Matrix}
\alias{lu}
\alias{lu,matrix-method}
\alias{lu,dgeMatrix-method}
\alias{lu,dgCMatrix-method}
\usage{
lu(x, \dots)
\S4method{lu}{matrix}(x, warnSing = TRUE, \dots)
\S4method{lu}{dgeMatrix}(x, warnSing = TRUE, \dots)
\S4method{lu}{dgCMatrix}(x, errSing = TRUE, \dots)
}
\description{
  Computes (generalized) triangular decompositions of square and other
  dense matrices.
}
\arguments{
  \item{x}{a dense or sparse matrix, in the latter case of square
    dimension.  No missing values or IEEE special values are allowed.}
  \item{warnSing}{(when \code{x} is a
    \code{"\linkS4class{denseMatrix}"}) logical specifying if a \code{\link{warning}}
    should be signalled when \code{x} is singular.}
  \item{errSing}{(when \code{x} is a
    \code{"\linkS4class{sparseMatrix}"}) logical specifying if an error
    (see \code{\link{stop}}) should be signalled when \code{x} is
    singular.  When \code{x} is singular, \code{lu(x, errSing=FALSE)}
    returns \code{\link{NA}} instead of an LU decomposition.  No
    warning is signalled and the useR should be careful in that case.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  An object of class \code{"LU"}, i.e., \code{"\linkS4class{denseLU}"}%% ./LU-class.Rd
  (see its separate help page),
  or \code{"sparseLU"}, see \code{\linkS4class{sparseLU}}; this is
  a representation of a triangular decomposition of \code{x}.
}
\details{
  \code{lu()} is a generic function with special methods for different types
  of matrices.  Use \code{\link{showMethods}("lu")} to list all the methods
  for the \code{\link{lu}} generic.

  The method for class \code{\linkS4class{dgeMatrix}} (and all dense
  matrices) is based on LAPACK's \code{"dgetrf"} subroutine.  It returns
  a decomposition also for singular and non-square matrices.

  The method for class \code{\linkS4class{dgCMatrix}} (and all sparse
  matrices) is based on functions from the CSparse library.  It signals
  an error (or returns \code{NA}, when \code{errSing = FALSE}, see
  above) when the decomposition algorithm fails, as when \code{x} is
  (too close to) singular.% yes, it would be nice if we could have it
  % behave more similar to the dense method: still give an LU,
  % and an optional warning.
}
\references{
  Golub, G., and Van Loan, C. F. (1989).
  \emph{Matrix Computations,}
  2nd edition, Johns Hopkins, Baltimore.

  Tim Davis (2005)
  \url{http://www.cise.ufl.edu/research/sparse/CSparse/}

  Timothy A. Davis (2006)
  \emph{Direct Methods for Sparse Linear Systems}, SIAM Series
  \dQuote{Fundamentals of Algorithms}.
}
\seealso{
  Class definitions \code{\linkS4class{LU}} and \code{\linkS4class{sparseLU}}
  and function \code{\link{expand}};
  \code{\link{qr}}, \code{\link{chol}}.
}
\examples{

##--- Dense  -------------------------
x <- Matrix(rnorm(9), 3, 3)
lu(x)
dim(x2 <- round(10 * x[,-3]))# non-square
expand(lu2 <- lu(x2))

##--- Sparse (see more in ?"sparseLU-class")----- % ./sparseLU-class.Rd

pm <- as(readMM(system.file("external/pores_1.mtx",
                            package = "Matrix")),
         "CsparseMatrix")
str(pmLU <- lu(pm))		# p is a 0-based permutation of the rows
                                # q is a 0-based permutation of the columns
## permute rows and columns of original matrix
ppm <- pm[pmLU@p + 1L, pmLU@q + 1L]
pLU <- drop0(pmLU@L \%*\% pmLU@U) # L \%*\% U -- dropping extra zeros
## equal up to "rounding"
ppm[1:14, 1:5]
pLU[1:14, 1:5]
}
\keyword{array}
\keyword{algebra}
