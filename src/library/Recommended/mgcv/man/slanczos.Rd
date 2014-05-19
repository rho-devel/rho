\name{slanczos}
\alias{slanczos}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Compute truncated eigen decomposition of a symmetric matrix}
\description{ Uses Lanczos iteration to find the truncated eigen-decomposition of a symmetric matrix. 
}

\usage{
slanczos(A,k=10,kl=-1,tol=.Machine$double.eps^.5)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{A}{A symmetric matrix.}

\item{k}{Must be non-negative. If \code{kl} is negative, then the \code{k} largest magnitude eigenvalues 
are found, together with the corresponding eigenvectors. If \code{kl} is non-negative then the \code{k}
 highest eigenvalues are found together with their eigenvectors and the \code{kl} lowest eigenvalues with
eigenvectors are also returned.}

\item{kl}{If \code{kl} is non-negative then the \code{kl} lowest eigenvalues are returned together with their 
corresponding eigenvectors (in addition to the \code{k} highest eignevalues + vectors). 
negative \code{kl} signals that the \code{k} largest magnitude eigenvalues should be returned, with eigenvectors.}

\item{tol}{tolerance to use for convergence testing of eigenvalues. Error in eigenvalues will be less 
    than the magnitude of the dominant eigenvalue multiplied by \code{tol} (or the machine precision!).}

}

\details{ If \code{kl} is non-negative, returns the highest \code{k} and lowest \code{kl} eigenvalues, 
with their corresponding eigenvectors. If \code{kl} is negative, returns the largest magnitude \code{k} 
eigenvalues, with corresponding eigenvectors.

The routine implements Lanczos iteration with full re-orthogonalization as described in Demmel (1997). Lanczos 
iteraction iteratively constructs a tridiagonal matrix, the eigenvalues of which converge to the eigenvalues of \code{A},
as the iteration proceeds (most extreme first). Eigenvectors can also be computed. For small \code{k} and \code{kl} the 
approach is faster than computing the full symmetric eigendecompostion. The tridiagonal eigenproblems are handled using LAPACK.

The implementation is not optimal: in particular the inner triadiagonal problems could be handled more efficiently, and 
there would be some savings to be made by not always returning eigenvectors. 
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
 require(mgcv)
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









