\name{drop0}
\alias{drop0}
\title{Drop "Explicit Zeroes" from a Sparse Matrix}
\description{
  Returns a sparse matrix with no \dQuote{explicit zeroes}, i.e., all
  zero or \code{FALSE} entries are dropped from the explicitly indexed
  matrix entries.
}
\usage{
drop0(x, tol = 0, is.Csparse = NA)
}
\arguments{
  \item{x}{a Matrix, typically sparse, i.e., inheriting from
    \code{\linkS4class{sparseMatrix}}.}
%   \item{clx}{[optional:] the \code{\link{class}} or \dQuote{class
%       definition} (see \code{\link{getClassDef}}; it is of class
%     \code{\linkS4class{classRepresentation}}) of \code{x}.\cr
%     This argument just exists for the possibility of speedup.}
  \item{tol}{non-negative number to be used as tolerance for checking if
    an entry \eqn{x_{i,j}}{x[i,j]} should be considered to be zero.}
  \item{is.Csparse}{logical indicating prior knowledge about the
    \dQuote{Csparseness} of \code{x}.  This exists for possible speedup
    reasons only.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  a Matrix like \code{x} but with no explicit zeros, i.e.,
  \code{!any(x@x == 0)}, always inheriting from
  \code{\linkS4class{CsparseMatrix}}.
}
% \author{Martin}
\note{When a sparse matrix is the result of matrix multiplications, you
  may want to consider combining \code{drop0()} with
  \code{\link{zapsmall}()},
  see the example.
}
\seealso{\code{\link{spMatrix}}, class \code{\linkS4class{sparseMatrix}}
}
\examples{
m <- spMatrix(10,20, i= 1:8, j=2:9,
              x = c(0:2,3:-1))
m
drop0(m)

## A larger example:
t5 <- new("dtCMatrix", Dim = c(5L, 5L), uplo = "L",
          x = c(10, 1, 3, 10, 1, 10, 1, 10, 10),
	  i = c(0L,2L,4L, 1L, 3L,2L,4L, 3L, 4L),
	  p = c(0L, 3L, 5L, 7:9))
TT <- kronecker(t5, kronecker(kronecker(t5,t5), t5))
IT <- solve(TT)
I. <- TT \%*\% IT ;  nnzero(I.) # 697 ( = 625 + 72 )
I.0 <- drop0(zapsmall(I.))
stopifnot(all(I.0 == Diagonal(625)))
}
\keyword{utilities}
\keyword{array}

