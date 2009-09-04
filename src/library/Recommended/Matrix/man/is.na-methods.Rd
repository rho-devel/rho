\name{is.na-methods}
\docType{methods}
\alias{is.na-methods}
\alias{is.na,denseMatrix-method}
\alias{is.na,sparseMatrix-method}
\title{is.na() Methods for 'Matrix' Objects}
\description{
  Methods for function \code{\link{is.na}()} for all Matrices (objects
  extending the \code{\linkS4class{Matrix}} class):

  \describe{
    \item{x = "denseMatrix"}{returns a \code{"nMatrix"} object of same
      dimension as \code{x}, with TRUE's whenever \code{x} was NA.}
    \item{x = "sparseMatrix"}{ditto.}
  }
}
\usage{
\S4method{is.na}{sparseMatrix}(x)
}
\arguments{
  \item{x}{sparse or dense matrix.}
}
\seealso{
  \code{\link{NA}}, \code{\link{is.na}};
  \code{\linkS4class{nMatrix}},
  \code{\linkS4class{denseMatrix}},
  \code{\linkS4class{sparseMatrix}}.
}
\examples{
M <- Matrix(1:6, nrow=4, ncol=3,
       dimnames = list(c("a", "b", "c", "d"), c("A", "B", "C")))
stopifnot(all(!is.na(M)))
M[2:3,2] <- NA
is.na(M)

A <- spMatrix(10,20, i = c(1,3:8),
                     j = c(2,9,6:10),
                     x = 7 * (1:7))
stopifnot(all(!is.na(A)))
A[2,3] <- A[1,2] <- A[5, 5:9] <- NA
inA <- is.na(A)
stopifnot(sum(inA) == 1+1+5)
}
\keyword{methods}

