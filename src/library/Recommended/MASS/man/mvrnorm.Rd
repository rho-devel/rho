% file MASS/man/mvrnorm.Rd
% copyright (C) 1994-2012 W. N. Venables and B. D. Ripley
%
\name{mvrnorm}
\alias{mvrnorm}
\title{Simulate from a Multivariate Normal Distribution}
\description{
  Produces one or more samples from the specified
  multivariate normal distribution.
}
\usage{
mvrnorm(n = 1, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
}
\arguments{
  \item{n}{the number of samples required.}
  \item{mu}{a vector giving the means of the variables.}
  \item{Sigma}{a positive-definite symmetric matrix specifying the
    covariance matrix of the variables.}
  \item{tol}{tolerance (relative to largest variance) for numerical lack
    of positive-definiteness in \code{Sigma}.}
  \item{empirical}{logical. If true, mu and Sigma specify the empirical
    not population mean and covariance matrix.}
  \item{EISPACK}{logical. Set to true to reproduce results from MASS
    versions prior to 3.1-21.}
}
\value{
  If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
  \code{n} by \code{length(mu)} matrix with one sample in each row.
}
\section{Side Effects}{
  Causes creation of the dataset \code{.Random.seed} if it does
  not already exist, otherwise its value is updated.
}
\details{
  The matrix decomposition is done via \code{eigen}; although a Choleski
  decomposition might be faster, the eigendecomposition is
  stabler.
}
\references{
  B. D. Ripley (1987) \emph{Stochastic Simulation.} Wiley. Page 98.
}
\seealso{
  \code{\link{rnorm}}
}
\examples{
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
var(mvrnorm(n=1000, rep(0, 2), Sigma))
var(mvrnorm(n=1000, rep(0, 2), Sigma, empirical = TRUE))
}
\keyword{distribution}
\keyword{multivariate}
