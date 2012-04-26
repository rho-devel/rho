\name{condest}
\alias{condest}
\alias{onenormest}
\title{Compute Approximate CONDition number and 1-Norm of (Large) Matrices}
\description{
  \dQuote{Estimate}, i.e. compute approximately the CONDition number of
  a (potentially large, often sparse) matrix \code{A}.
  It works by apply a fast approximation of the 1-norm,
  \code{norm(A,"1")}, through \code{onenormest(.)}.
}
\usage{
condest(A, t = min(n, 5), normA = norm(A, "1"),
        silent = FALSE, quiet = TRUE)

onenormest(A, t = min(n, 5), A.x, At.x, n,
           silent = FALSE, quiet = silent,
           iter.max = 10, eps = 4 * .Machine$double.eps)
}
\arguments{
  \item{A}{a square matrix, optional for \code{onenormest()}, where
    instead of \code{A}, \code{A.x} and \code{At.x} can be specified,
    see there.}
  \item{t}{number of columns to use in the iterations.}
  \item{normA}{number; (an estimate of) the 1-norm of \code{A}, by
    default \code{\link{norm}(A, "1")}; may be replaced by an estimate.}
  \item{silent}{logical indicating if warning and (by default)
    convergence messages should be displayed.}
  \item{quiet}{logical indicating if convergence messages should be
    displayed.}
  \item{A.x, At.x}{when \code{A} is missing, these two must be given as
    functions which compute \code{A \%\% x}, or \code{t(A) \%\% x},
    respectively.}
  \item{n}{\code{ == nrow(A)}, only needed when \code{A} is not specified.}
  \item{iter.max}{maximal number of iterations for the 1-norm estimator.}
  \item{eps}{the relaive change that is deemed irrelevant.}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{Both functions return a \code{\link{list}};
  \code{onenormest()} with components,
  \item{est}{a number \eqn{> 0}, the estimated \code{norm(A, "1")}.}
  \item{v}{the maximal \eqn{A X} column.}

  The function \code{condest()} returns a list with components,
  \item{est}{a number \eqn{> 0}, the estimated condition number
    \eqn{\hat\kappa}{k.}; when \eqn{r :=}\code{rcond(A)},
    \eqn{1/\hat\kappa \approx r}{1/k. ~= r}.}
  \item{v}{integer vector length \code{n}, with an \code{1} at the index
    \code{j} with maximal column \code{A[,j]} in \eqn{A}.}
  \item{w}{numeric vector, the largest \eqn{A x} found.}
  \item{iter}{the number of iterations used.}
}
\references{
%% See also Tim Davis(2006, p.96):
  Nicholas J. Higham and Françoise Tisseur (2000).
  A Block Algorithm for Matrix 1-Norm Estimation, with an Application to 1-Norm
  Pseudospectra.
  \emph{SIAM J. Matrix Anal. Appl.} \bold{21}, 4, 1185--1201.
  \url{http://dx.doi.org/10.1137/S0895479899356080}
  %% and \url{http://citeseer.ist.psu.edu/223007.html}

  William W. Hager (1984).
  Condition Estimates.
  \emph{SIAM J. Sci. Stat. Comput.} \bold{5}, 311--316.
}
\author{This is based on octave's \code{condest()} and
  \code{onenormest()} implementations with original author
  Jason Riedy, U Berkeley; translation to \R and
  adaption by Martin Maechler.
}
\seealso{
  \code{\link{norm}}, \code{\link{rcond}}.
}
\examples{
data(KNex)
mtm <- with(KNex, crossprod(mm))
system.time(ce <- condest(mtm))
## reciprocal
1 / ce$est
system.time(rc <- rcond(mtm)) # takes ca  3 x  longer
rc
all.equal(rc, 1/ce$est) # TRUE -- the approxmation was good
}
