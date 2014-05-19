\name{is.na-methods}
\title{is.na(), is.{in}finite() Methods for 'Matrix' Objects}
\docType{methods}
\alias{is.finite,ddenseMatrix-method}
\alias{is.finite,dgeMatrix-method}
\alias{is.finite,dsparseMatrix-method}
\alias{is.finite,diagonalMatrix-method}
\alias{is.finite,lMatrix-method}
\alias{is.finite,nMatrix-method}
\alias{is.finite,indMatrix-method}
\alias{is.infinite,ddenseMatrix-method}
\alias{is.infinite,dsparseMatrix-method}
\alias{is.infinite,diagonalMatrix-method}
\alias{is.infinite,lMatrix-method}
\alias{is.infinite,nMatrix-method}
\alias{is.infinite,indMatrix-method}
\alias{is.na,denseMatrix-method}
\alias{is.na,indMatrix-method}
\alias{is.na,sparseMatrix-method}
\alias{anyNA,nsparseMatrix-method}
\alias{anyNA,nsparseVector-method}
\alias{anyNA,sparseVector-method}
\alias{anyNA,xMatrix-method}
\alias{is.na-methods}
\description{
  Methods for function \code{\link{is.na}()}, \code{\link{is.finite}()},
  and \code{\link{is.infinite}()} for all Matrices (objects
  extending the \code{\linkS4class{Matrix}} class):

  \describe{
    \item{x = "denseMatrix"}{returns a \code{"nMatrix"} object of same
      dimension as \code{x}, with TRUE's whenever \code{x} is
    \code{\link{NA}}, finite, or infinite, respectively.}
    \item{x = "sparseMatrix"}{ditto.}
  }
}
\usage{
\S4method{is.na}{sparseMatrix}(x)
\S4method{is.finite}{dsparseMatrix}(x)
\S4method{is.infinite}{ddenseMatrix}(x)
## ...
## and for other classes

\S4method{anyNA}{xMatrix}(x)
\S4method{anyNA}{nsparseMatrix}(x)
\S4method{anyNA}{sparseVector}(x)
\S4method{anyNA}{nsparseVector}(x)
}
\arguments{
  \item{x}{sparse or dense matrix or sparse vector (here; any \R object in general).}
}
\seealso{
  \code{\link{NA}}, \code{\link{is.na}};
  \code{\link{is.finite}}, \code{\link{is.infinite}};
  \code{\linkS4class{nMatrix}},
  \code{\linkS4class{denseMatrix}}, \code{\linkS4class{sparseMatrix}}.

  The \code{\linkS4class{sparseVector}} class.
}
\examples{
M <- Matrix(1:6, nrow=4, ncol=3,
       dimnames = list(c("a", "b", "c", "d"), c("A", "B", "C")))
stopifnot(all(!is.na(M)))
M[2:3,2] <- NA
is.na(M)
if(exists("anyNA", mode="function"))
anyNA(M)

A <- spMatrix(10,20, i = c(1,3:8),
                     j = c(2,9,6:10),
                     x = 7 * (1:7))
stopifnot(all(!is.na(A)))
%TODO stopifnot(all(!is.na(A)), !anyNA(A))
A[2,3] <- A[1,2] <- A[5, 5:9] <- NA
inA <- is.na(A)
stopifnot(sum(inA) == 1+1+5)
\dontshow{
if(exists("anyNA", mode="function"))
stopifnot(anyNA(M), anyNA(A))
}% dont..
}
\keyword{methods}
