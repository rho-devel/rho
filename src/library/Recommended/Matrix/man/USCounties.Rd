\name{USCounties}
\alias{USCounties}
\docType{data}
\title{USCounties Contiguity Matrix}
\description{
  This matrix represents the contiguities of 3111 US counties using the
  Queen criterion of at least a single shared boundary point. The
  representation is as a row standardised spatial weights matrix
  transformed to a symmetric matrix (see Ord (1975), p. 125).
}
\usage{data(USCounties)}
\format{
  A \eqn{3111 ^2} symmetric sparse matrix of class
  \code{\linkS4class{dsCMatrix}} with 9101 non-zero entries.
}
\details{
  The data were read into \R using \code{\link[spdep]{read.gal}}, and
  row-standardised and transformed to symmetry using
  \code{\link[spdep]{nb2listw}} and \code{\link[spdep]{similar.listw}}.
  This spatial weights object was converted to class
  \code{\linkS4class{dsCMatrix}} using
  \code{\link[spdep]{as_dsTMatrix_listw}} and coercion.
}
\source{
  The data were retrieved from \url{http://sal.uiuc.edu/weights/zips/usc.zip},
  files \dQuote{usc.txt} and \dQuote{usc\_q.GAL}, with permission for use
  and distribution from Luc Anselin.
}
\references{
  Ord, J. K. (1975)
  Estimation methods for models of spatial interaction;
  \emph{Journal of the American Statistical Association} \bold{70}, 120--126.
}
\examples{
data(USCounties)
(n <- ncol(USCounties))
IM <- .symDiagonal(n)
nn <- 50
set.seed(1)
rho <- runif(nn, 0, 1)
system.time(MJ <- sapply(rho, function(x)
	determinant(IM - x * USCounties, logarithm = TRUE)$modulus))

nWC <- -USCounties
C1 <- Cholesky(nWC, Imult = 2)
system.time(MJ1 <- n * log(rho) +
            sapply(rho, function(x)
                   c(determinant(update(C1, nWC, 1/x))$modulus)))
all.equal(MJ, MJ1)

C2 <- Cholesky(nWC, super = TRUE, Imult = 2)
system.time(MJ2 <- n * log(rho) +
            sapply(rho, function(x)
                   c(determinant(update(C2, nWC, 1/x))$modulus)))
all.equal(MJ, MJ2)
system.time(MJ3 <- n * log(rho) + Matrix:::ldetL2up(C1, nWC, 1/rho))
all.equal(MJ, MJ3)
system.time(MJ4 <- n * log(rho) + Matrix:::ldetL2up(C2, nWC, 1/rho))
all.equal(MJ, MJ4)
}
\keyword{datasets}
