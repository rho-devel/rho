\name{formatSparseM}
\alias{formatSparseM}
\alias{.formatSparseSimple}
\title{Formatting Sparse Numeric Matrices Utilities}
\description{
  Utilities for formatting sparse numeric matrices in a flexible way.
  These functions are used by the \code{\link{format}} and \code{print}
  methods for sparse matrices and can be applied as well to standard \R
  matrices.  Note that \emph{all} arguments but the first are optional.

  \code{formatSparseM()} is the main \dQuote{workhorse} of
  \code{\link{formatSpMatrix}}, the \code{format} method for sparse
  matrices.

  \code{.formatSparseSimple()} is a simple helper function, also dealing
  with (short/empty) column names construction.
}
\usage{
formatSparseM(x, zero.print = ".", align = c("fancy", "right"),
              m = as(x,"matrix"), asLogical=NULL, digits=NULL,
              cx, iN0, dn = dimnames(m))

.formatSparseSimple(m, asLogical=FALSE, digits=NULL,
		    col.names, note.dropping.colnames = TRUE,
                    dn=dimnames(m))
}
\arguments{
  \item{x}{an \R object inheriting from class \code{\linkS4class{sparseMatrix}}.}
  \item{zero.print}{character which should be used for
    \emph{structural} zeroes.  The default \code{"."} may occasionally
    be replaced by \code{" "} (blank); using \code{"0"} would look
    almost like \code{print()}ing of non-sparse matrices.}
  \item{align}{a string specifying how the \code{zero.print} codes
    should be aligned, see \code{\link{formatSpMatrix}}.}
  \item{m}{(optional) a (standard \R) \code{\link{matrix}} version of \code{x}.}
  \item{asLogical}{should the matrix be formatted as a logical matrix
    (or rather as a numeric one); mostly for \code{formatSparseM()}.}
  \item{digits}{significant digits to use for printing, see
    \code{\link{print.default}}.}
  \item{cx}{(optional) character matrix; a formatted version of \code{x}, still
    with strings such as \code{"0.00"} for the zeros.}
  \item{iN0}{(optional) integer vector, specifying the location of the
    \emph{non}-zeroes of \code{x}.}
  \item{col.names, note.dropping.colnames}{see \code{\link{formatSpMatrix}}.}
  \item{dn}{\code{\link{dimnames}} to be used; a list (of length two)
    with row and column names (or \code{\link{NULL}}).}
}
\seealso{
  \code{\link{formatSpMatrix}} which calls \code{formatSparseM()} and is
  the \code{\link{format}} method for sparse matrices.\cr
  \code{\link{printSpMatrix}} which is used by the (typically
  implicitly called) \code{\link{show}} and \code{\link{print}} methods
  for sparse matrices.
}
\value{
  a character matrix like \code{cx}, where the zeros have been replaced
  with (padded versions of) \code{zero.print}.
  As this is a \emph{dense} matrix, do not use these functions for
  really large (really) sparse matrices!
}
\author{Martin Maechler}
\examples{
m <- suppressWarnings(matrix(c(0, 3.2, 0,0, 11,0,0,0,0,-7,0), 4,9))
fm <- formatSparseM(m)
noquote(fm)
## nice, but this is nicer :
print(fm, quote=FALSE, right=TRUE)
## and "the same" as :
Matrix(m)

## align = "right" is cheaper -->  the "." are not aligned:
noquote(f2 <- formatSparseM(m,align="r"))
stopifnot(f2 == fm   |   m == 0, dim(f2) == dim(m),
         (f2 == ".") == (m == 0))
}
\keyword{utilities}
\keyword{print}
