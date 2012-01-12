\name{nearPD}
\alias{nearPD}
\title{Nearest Positive Definite Matrix}
\description{
 Compute the nearest positive definite matrix to an approximate
 one, typically a correlation or variance-covariance matrix.
}
\usage{
nearPD(x, corr = FALSE, keepDiag = FALSE, do2eigen = TRUE,
       doSym = FALSE, doDykstra = TRUE, only.values = FALSE,
       eig.tol = 1e-06, conv.tol = 1e-07, posd.tol = 1e-08,
       maxit = 100, trace = FALSE)
}
\arguments{
  \item{x}{numeric \eqn{n \times n}{n * n} approximately positive
    definite matrix, typically an approximation to a correlation or
    covariance matrix.  If \code{x} is not symmetric,
    \code{\link{symmpart}(x)} is used.}
  \item{corr}{logical indicating if the matrix should be a
    \emph{correlation} matrix.}
  \item{keepDiag}{logical, generalizing \code{corr}: if \code{TRUE}, the
    resulting matrix should have the same diagonal
    (\code{\link{diag}(x)}) as the input matrix.}
  \item{do2eigen}{logical indicating if a
    \code{\link[sfsmisc]{posdefify}()} eigen step should be applied to
    the result of the Higham algorithm.}
  \item{doSym}{logical indicating if \code{X <- (X + t(X))/2} should be
    done, after \code{X <- tcrossprod(Qd, Q)}; some doubt if this is necessary.}
  \item{doDykstra}{logical indicating if Dykstra's correction should be
    used; true by default.  If false, the algorithm is basically the
    direct fixpoint iteration
    \eqn{Y_k = P_U(P_S(Y_{k-1}))}{Y(k) = P_U(P_S(Y(k-1)))}.}
  \item{only.values}{logical; if \code{TRUE}, the result is just the
    vector of eigen values of the approximating matrix.}
  \item{eig.tol}{defines relative positiveness of eigenvalues compared
    to largest one, \eqn{\lambda_1}. Eigen values \eqn{\lambda_k} are
    treated as if zero when \eqn{\lambda_k / \lambda_1 \le eig.tol}.}
  \item{conv.tol}{convergence tolerance for Higham algorithm.}
  \item{posd.tol}{tolerance for enforcing positive definiteness (in the
    final \code{posdefify} step when \code{do2eigen} is \code{TRUE}).}
  \item{maxit}{maximum number of iterations allowed.}
  \item{trace}{logical or integer specifying if convergence monitoring
    should be traced.}
}
\details{
  This implements the algorithm of Higham (2002), and then (if
  \code{do2eigen} is true) forces positive definiteness using code from
  \code{\link[sfsmisc]{posdefify}}.  The algorithm of Knol DL and ten
  Berge (1989) (not implemented here) is more general in (1) that it
  allows constraints to fix some rows (and columns) of the matrix and (2)
  to force the smallest eigenvalue to have a certain value.

  Note that setting \code{corr = TRUE} just sets \code{diag(.) <- 1}
  within the algorithm.

  Higham (2002) uses Dykstra's correction, but the version by Jens
  Oehlschlaegel did not use it (accidentally), and has still lead to
  good results; this simplification, now only via \code{doDykstra = FALSE},
  was active in \code{nearPD()} upto Matrix version 0.999375-40.
}
\value{
  If \code{only.values = TRUE}, a numeric vector of eigen values of the
  approximating matrix;
  Otherwise, as by default, an S3 object of \code{\link{class}}
  \code{"nearPD"}, basically a list with components
  \item{mat}{a matrix of class \code{\linkS4class{dpoMatrix}}, the
  computed positive-definite matrix.}
  \item{eigenvalues}{numeric vector of eigen values of \code{mat}.}
  \item{corr}{logical, just the argument \code{corr}.}
  \item{normF}{the Frobenius norm (\code{\link{norm}(x-X, "F")}) of the
  difference between the original and the resulting matrix.}
  \item{iterations}{number of iterations needed.}
  \item{converged}{logical indicating if iterations converged.}
}
\references{%% more in /u/maechler/R/Pkgs/sfsmisc/man/posdefify.Rd
  Cheng, Sheung Hun and Higham, Nick (1998)
  A Modified Cholesky Algorithm Based on a Symmetric Indefinite Factorization;
  \emph{SIAM J. Matrix Anal.\ Appl.}, \bold{19}, 1097--1110.

  Knol DL, ten Berge JMF (1989)
  Least-squares approximation of an improper correlation matrix by a
  proper one.
  \emph{Psychometrika} \bold{54}, 53--61.

  Higham, Nick (2002)
  Computing the nearest correlation matrix - a problem from finance;
  \emph{IMA Journal of Numerical Analysis} \bold{22}, 329--343.
}
\author{Jens Oehlschlaegel donated a first version.  Subsequent changes
  by the Matrix package authors.
}
\seealso{A first version of this (with non-optional \code{corr=TRUE})
  has been available as \code{\link[sfsmisc]{nearcor}()}; and
  more simple versions with a similar purpose
  \code{\link[sfsmisc]{posdefify}()}, both from package \pkg{sfsmisc}.
}
\examples{
 ## Higham(2002), p.334f - simple example
 A <- matrix(1, 3,3); A[1,3] <- A[3,1] <- 0
 n.A <- nearPD(A, corr=TRUE, do2eigen=FALSE)
 n.A[c("mat", "normF")]
 stopifnot(all.equal(n.A$mat[1,2], 0.760689917),
	   all.equal(n.A$normF, 0.52779033, tol=1e-9) )

 set.seed(27)
 m <- matrix(round(rnorm(25),2), 5, 5)
 m <- m + t(m)
 diag(m) <- pmax(0, diag(m)) + 1
 (m <- round(cov2cor(m), 2))

 str(near.m <- nearPD(m, trace = TRUE))
 round(near.m$mat, 2)
 norm(m - near.m$mat) # 1.102 / 1.08

 if(require("sfsmisc")) {
    m2 <- posdefify(m) # a simpler approach
    norm(m - m2)  # 1.185, i.e., slightly "less near"
 }

 round(nearPD(m, only.values=TRUE), 9)

## A longer example, extended from Jens' original,
## showing the effects of some of the options:

pr <- Matrix(c(1,     0.477, 0.644, 0.478, 0.651, 0.826,
               0.477, 1,     0.516, 0.233, 0.682, 0.75,
               0.644, 0.516, 1,     0.599, 0.581, 0.742,
               0.478, 0.233, 0.599, 1,     0.741, 0.8,
               0.651, 0.682, 0.581, 0.741, 1,     0.798,
               0.826, 0.75,  0.742, 0.8,   0.798, 1),
             nrow = 6, ncol = 6)

nc.  <- nearPD(pr, conv.tol = 1e-7) # default
nc.$iterations  # 2
nc.1 <- nearPD(pr, conv.tol = 1e-7, corr = TRUE)
nc.1$iterations # 11 / 12 (!)
ncr   <- nearPD(pr, conv.tol = 1e-15)
str(ncr)# still 2 iterations
ncr.1 <- nearPD(pr, conv.tol = 1e-15, corr = TRUE)
ncr.1 $ iterations # 27 / 30 !

## But indeed, the 'corr = TRUE' constraint did ensure a better solution;
## cov2cor() does not just fix it up equivalently :
norm(pr - cov2cor(ncr$mat)) # = 0.09994
norm(pr -       ncr.1$mat)  # = 0.08746 / 0.08805
}
\keyword{algebra}
\keyword{array}
