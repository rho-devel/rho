\name{colSums}
\alias{colSums}
\alias{colMeans}
\alias{rowSums}
\alias{rowMeans}
\alias{colMeans,diagonalMatrix-method}
\alias{colSums,diagonalMatrix-method}
\alias{rowMeans,diagonalMatrix-method}
\alias{rowSums,diagonalMatrix-method}
\alias{colMeans,CsparseMatrix-method}
\alias{colSums,CsparseMatrix-method}
\alias{rowMeans,CsparseMatrix-method}
\alias{rowSums,CsparseMatrix-method}
\alias{colMeans,TsparseMatrix-method}
\alias{colSums,TsparseMatrix-method}
\alias{rowMeans,TsparseMatrix-method}
\alias{rowSums,TsparseMatrix-method}
\alias{colMeans,RsparseMatrix-method}
\alias{colSums,RsparseMatrix-method}
\alias{rowMeans,RsparseMatrix-method}
\alias{rowSums,RsparseMatrix-method}
\alias{colMeans,dgCMatrix-method}
\alias{colSums,dgCMatrix-method}
\alias{rowMeans,dgCMatrix-method}
\alias{rowSums,dgCMatrix-method}
\alias{colMeans,igCMatrix-method}
\alias{colSums,igCMatrix-method}
\alias{rowMeans,igCMatrix-method}
\alias{rowSums,igCMatrix-method}
\alias{colMeans,lgCMatrix-method}
\alias{colSums,lgCMatrix-method}
\alias{rowMeans,lgCMatrix-method}
\alias{rowSums,lgCMatrix-method}
\alias{colMeans,ngCMatrix-method}
\alias{colSums,ngCMatrix-method}
\alias{rowMeans,ngCMatrix-method}
\alias{rowSums,ngCMatrix-method}
% dense ones
\alias{colMeans,denseMatrix-method}
\alias{colSums,denseMatrix-method}
\alias{rowMeans,denseMatrix-method}
\alias{rowSums,denseMatrix-method}
\alias{colMeans,ddenseMatrix-method}
\alias{colSums,ddenseMatrix-method}
\alias{rowMeans,ddenseMatrix-method}
\alias{rowSums,ddenseMatrix-method}
% NB: kept those documented in ./dgeMatrix-class.Rd
%
\title{Form Row and Column Sums and Means}
% see also ~/R/D/r-devel/R/src/library/base/man/colSums.Rd
\description{
  Form row and column sums and means for \code{\linkS4class{Matrix}} objects.
}
\usage{
colSums (x, na.rm = FALSE, dims = 1, \dots)
rowSums (x, na.rm = FALSE, dims = 1, \dots)
colMeans(x, na.rm = FALSE, dims = 1, \dots)
rowMeans(x, na.rm = FALSE, dims = 1, \dots)

\S4method{colSums}{CsparseMatrix}(x, na.rm = FALSE,
        dims = 1, sparseResult = FALSE)
\S4method{rowSums}{CsparseMatrix}(x, na.rm = FALSE,
        dims = 1, sparseResult = FALSE)
\S4method{colMeans}{CsparseMatrix}(x, na.rm = FALSE,
        dims = 1, sparseResult = FALSE)
\S4method{rowMeans}{CsparseMatrix}(x, na.rm = FALSE,
        dims = 1, sparseResult = FALSE)
}
\arguments{
  \item{x}{a Matrix, i.e., inheriting from  \code{\linkS4class{Matrix}}.}
  \item{na.rm}{logical. Should missing values (including \code{NaN})
    be omitted from the calculations?}
  \item{dims}{completely ignored by the \code{Matrix} methods.}
  \item{\dots}{potentially further arguments, for method \code{<->}
    generic compatibility.}
  \item{sparseResult}{logical indicating if the result should be sparse,
    i.e., inheriting from class \code{\linkS4class{sparseVector}}.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  returns a numeric vector if \code{sparseResult} is \code{FALSE} as per
  default.  Otherwise, returns a \code{\linkS4class{sparseVector}}.
}
%\author{Martin}
\seealso{\code{\link[base]{colSums}} and the
  \code{\linkS4class{sparseVector}} classes.
}
\examples{
(M <- bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))) # 7 x 8
colSums(M)
d <- Diagonal(10, c(0,0,10,0,2,rep(0,5)))
MM <- kronecker(d, M)
dim(MM) # 70 80
length(MM@x) # 160, but many are '0' ; drop those:
MM <- drop0(MM)
length(MM@x) # 32
  cm <- colSums(MM)
(scm <- colSums(MM, sparseResult = TRUE))
stopifnot(is(scm, "sparseVector"),
          identical(cm, as.numeric(scm)))
rowSums(MM, sparseResult = TRUE) # 16 of 70 are not zero
colMeans(MM, sparseResult = TRUE)
## Since we have no 'NA's, these two are equivalent :
stopifnot(identical(rowMeans(MM, sparseResult = TRUE),
                    rowMeans(MM, sparseResult = TRUE, na.rm = TRUE)),
	  rowMeans(Diagonal(16)) == 1/16,
	  colSums(Diagonal(7)) == 1)
}
\keyword{array}
\keyword{algebra}
\keyword{arith}
