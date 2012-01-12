\name{slanczos}
\alias{slanczos}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compute truncated eigen decomposition of a symmetric matrix}
\description{ Uses Lanczos iteration to find the truncated eigen-decomposition of a symmetric matrix. 
}

\usage{
slanczos(A,k=10,kl=-1)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{A}{A symmetric matrix.}

\item{k}{If \code{kl} is negative, then the \code{k} largest magnitude eigenvalues are found, together with the corresponding eigenvectors. Otherwise the \code{k} highest eigenvalues are found.}

\item{kl}{The \code{kl} lowest eigenvalues are returned, unless \code{kl} is negative. }

}

\details{ The routine implements Lanczos iteration with full re-orthogonalization as described in Demmel (1997). Lanczos 
iteraction iteratively constructs a tridiagonal matrix, the eigenvalues of which converge to the eigenvalues of \code{A},
as the iteration proceeds (most extreme first). Eigenvectors can also be computed. For small \code{k} and \code{kl} the 
approach is faster than computing the full symmetric eigendecompostion. The tridiagonal eigenproblems are handled using LAPACK.

The implementation is not optimal: in particular the inner triadiagonal problems could be handled more efficiently. 
} 

\value{ A list with elements \code{values} (array of eigenvalues); \code{vectors} (matrix with eigenvectors in its columns); 
\code{iter} (number of iterations required).

}
 
\references{
Demmel, J. (1997) Applied Numerical Linear Algebra. SIAM

}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\seealso{\code{\link{cyclic.p.spline}}}

\examples{
 ## create some x's and knots...
 set.seed(1);
 n <- 700;A <- matrix(runif(n*n),n,n);A <- A+t(A)
 
 ## compare timings of slanczos and eigen
 system.time(er <- slanczos(A,10))
 system.time(um <- eigen(A,symmetric=TRUE))
 
 ## confirm values are the same...
 ind <- c(1:6,(n-3):n)
 range(er$values-um$values[ind]);range(abs(er$vectors)-abs(um$vectors[,ind]))
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









