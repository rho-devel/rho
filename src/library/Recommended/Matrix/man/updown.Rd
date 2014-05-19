\name{updown}
\alias{updown}
\alias{updown-methods}
\alias{updown,ANY,ANY,ANY-method}
\alias{updown,character,mMatrix,CHMfactor-method}
\alias{updown,logical,mMatrix,CHMfactor-method}
\title{Up- and Down-Dating a Cholesky Decomposition}
\description{
  Compute the up- or down-dated Cholesky decomposition
}
\usage{
updown(update, C, L)
}
\arguments{
  \item{update}{logical (\code{TRUE} or \code{FALSE}) or \code{"+"} or
    \code{"-"} indicating if an up- or a down-date is to be computed.}
  \item{C}{any \R object, coercable to a sparse matrix (i.e., of
    subclass of \code{\linkS4class{sparseMatrix}}).}
  \item{L}{a Cholesky factor, specifically, of class
    \code{"\linkS4class{CHMfactor}"}.}
}

% \details{
% }

\value{
  an updated Cholesky factor, of the same dimension as \code{L}.
  Typically of class \code{"\linkS4class{dCHMsimpl}"} (a sub class of
  \code{"\linkS4class{CHMfactor}"}).
}
\references{
  CHOLMOD manual, currently beginning of chapter~18. ... %% FIXME
}
\author{Contributed by Nicholas Nagle, University of Tennessee, Knoxville, USA}
\section{Methods}{
  \describe{
    \item{\code{signature(update = "character", C = "mMatrix", L = "CHMfactor")}}{..}
    \item{\code{signature(update = "logical", C = "mMatrix", L = "CHMfactor")}}{ .. }
  }
}
\seealso{
 \code{\link{Cholesky}},
}
\examples{
dn <- list(LETTERS[1:3], letters[1:5])
## pointer vectors can be used, and the (i,x) slots are sorted if necessary:
m <- sparseMatrix(i = c(3,1, 3:2, 2:1), p= c(0:2, 4,4,6), x = 1:6, dimnames = dn)
cA <- Cholesky(A <- crossprod(m) + Diagonal(5))
166 * as(cA,"Matrix") ^ 2
uc1 <- updown("+",   Diagonal(5), cA)
## Hmm: this loses positive definiteness:
uc2 <- updown("-", 2*Diagonal(5), cA)
image(show(as(cA, "Matrix")))
image(show(c2 <- as(uc2,"Matrix")))# severely negative entries
##--> Warning
}
\keyword{methods}
