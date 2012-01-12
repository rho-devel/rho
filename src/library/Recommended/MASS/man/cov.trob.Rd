% file MASS/man/cov.trob.Rd
% copyright (C) 1997-9 W. N. Venables and B. D. Ripley
%
\name{cov.trob}
\alias{cov.trob}
\title{
Covariance Estimation for Multivariate t Distribution
}
\description{
Estimates a covariance or correlation matrix assuming the data came
from a multivariate t distribution: this provides some degree of
robustness to outlier without giving a high breakdown point.
}
\usage{
cov.trob(x, wt = rep(1, n), cor = FALSE, center = TRUE, nu = 5,
         maxit = 25, tol = 0.01)
}
\arguments{
\item{x}{
data  matrix. Missing values (NAs) are not allowed.
}
\item{wt}{
A vector of weights for each case: these are treated as if the case \code{i}
actually occurred \code{wt[i]} times.
}
\item{cor}{
Flag to choose between returning the correlation (\code{cor = TRUE}) or
covariance (\code{cor = FALSE}) matrix.
}
\item{center}{
a logical value or a numeric vector providing the location about which
the covariance is to be taken. If \code{center = FALSE}, no centering
is done; if \code{center = TRUE} the MLE of the location vector is used.
}
\item{nu}{
\sQuote{degrees of freedom} for the multivariate t distribution. Must exceed
2 (so that the covariance matrix is finite).
}
\item{maxit}{
Maximum number of iterations in fitting.
}
\item{tol}{
Convergence tolerance for fitting.
}}
\value{
A list with the following components

\item{cov}{
the fitted covariance matrix.
}
\item{center}{
the estimated or specified location vector.
}
\item{wt}{
the specified weights: only returned if the \code{wt} argument was given.
}
\item{n.obs}{
the number of cases used in the fitting.
}
\item{cor}{
the fitted correlation matrix: only returned if \code{cor = TRUE}.
}
\item{call}{
The matched call.
}
\item{iter}{
The number of iterations used.
}}
\references{
  J. T. Kent, D. E. Tyler and Y. Vardi (1994)
  A curious likelihood identity for the multivariate t-distribution.
  \emph{Communications in Statistics---Simulation and Computation}
  \bold{23}, 441--453.

  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\seealso{
\code{\link{cov}}, \code{\link{cov.wt}}, \code{\link{cov.mve}}
}
\examples{
cov.trob(stackloss)
}
\keyword{multivariate}
