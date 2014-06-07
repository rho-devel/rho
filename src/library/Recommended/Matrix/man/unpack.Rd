\name{unpack}
\title{Representation of Packed and Unpacked (Dense) Matrices}
\usage{
pack(x, \dots)
\S4method{pack}{matrix}(x, symmetric = NA, upperTri = NA, \dots)

unpack(x, \dots)
}
\alias{pack}
\alias{pack,symmetricMatrix-method}
\alias{pack,triangularMatrix-method}
\alias{pack,matrix-method}
\alias{pack,sparseMatrix-method}
\alias{unpack}
\alias{unpack,dtpMatrix-method}
\alias{unpack,dspMatrix-method}
\alias{unpack,symmetricMatrix-method}
\alias{unpack,triangularMatrix-method}
\alias{unpack,sparseMatrix-method}
\description{
  \dQuote{Packed} matrix storage here applies to dense matrices
  (\code{\linkS4class{denseMatrix}}) only, and there is available only
  for symmetric (\code{\linkS4class{symmetricMatrix}})
  or triangular (\code{\linkS4class{triangularMatrix}}) matrices,
  where only one triangle of the matrix needs to be stored.

  \code{unpack()} unpacks \dQuote{packed} matrices, where \cr
  \code{pack()} produces \dQuote{packed} matrices.
}
\arguments{
  \item{x}{
    \describe{
      \item{for \code{unpack()}:}{a matrix stored in packed form, e.g.,
	of class \code{"d?pMatrix"} where "?" is "t" for triangular or
	"s" for symmetric.}
      \item{for \code{pack()}:}{a (symmetric or triangular) matrix stored
	in full storage.}
    }
  }
  \item{symmetric}{logical (including \code{NA}) for optionally
    specifying if \code{x} is symmetric (or rather triangular).}
  \item{upperTri}{(for the triangular case only) logical
    (incl. \code{NA}) indicating if \code{x} is upper (or lower) triangular.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  \describe{
    \item{for \code{unpack()}:}{A \code{\linkS4class{Matrix}} object
	containing the full-storage representation of \code{x}.}
    \item{for \code{pack()}:}{
      A packed \code{Matrix} (i.e. of class \code{"..pMatrix"})
      representation of \code{x}.}
  }
}
\details{
  These are generic functions with special methods for different types
  of packed (or non-packed) symmetric or triangular dense
  matrices.  Use \code{\link{showMethods}("unpack")} to list
  the methods for \code{unpack()}, and similarly for \code{pack()}.
}
\examples{
showMethods("unpack")
(cp4 <- chol(Hilbert(4))) # is triangular
tp4 <- as(cp4,"dtpMatrix")# [t]riangular [p]acked
str(tp4)
(unpack(tp4))
stopifnot(identical(tp4, pack(unpack(tp4))))

(s <- crossprod(matrix(sample(15), 5,3))) # traditional symmetric matrix
(sp <- pack(s))
mt <- as.matrix(tt <- tril(s))
(pt <- pack(mt))
stopifnot(identical(pt, pack(tt)),
	  dim(s ) == dim(sp), all(s  == sp),
	  dim(mt) == dim(pt), all(mt == pt), all(mt == tt))
showMethods("pack")
}
\keyword{array}
\keyword{algebra}
