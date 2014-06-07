% file MASS/man/Skye.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Skye}
\alias{Skye}
\title{
  AFM Compositions of Aphyric Skye Lavas
}
\description{
  The \code{Skye} data frame has 23 rows and 3 columns.
}
\usage{
Skye
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{A}}{
      Percentage of sodium and potassium oxides.
    }
    \item{\code{F}}{
      Percentage of iron oxide.
    }
    \item{\code{M}}{
      Percentage of magnesium oxide.
    }
  }
}
\source{
  R. N. Thompson, J. Esson and A. C. Duncan (1972)
  Major element chemical variation in the Eocene lavas of the Isle of
  Skye. \emph{J. Petrology}, \bold{13}, 219--253.
}
\references{
  J. Aitchison (1986)
  \emph{The Statistical Analysis of Compositional Data.}
  Chapman and Hall, p.360.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
# ternary() is from the on-line answers.
ternary <- function(X, pch = par("pch"), lcex = 1,
                    add = FALSE, ord = 1:3, \dots)
{
  X <- as.matrix(X)
  if(any(X < 0)) stop("X must be non-negative")
  s <- drop(X \%*\% rep(1, ncol(X)))
  if(any(s<=0)) stop("each row of X must have a positive sum")
  if(max(abs(s-1)) > 1e-6) {
    warning("row(s) of X will be rescaled")
    X <- X / s
  }
  X <- X[, ord]
  s3 <- sqrt(1/3)
  if(!add)
  {
    oldpty <- par("pty")
    on.exit(par(pty=oldpty))
    par(pty="s")
    plot(c(-s3, s3), c(0.5-s3, 0.5+s3), type="n", axes=FALSE,
         xlab="", ylab="")
    polygon(c(0, -s3, s3), c(1, 0, 0), density=0)
    lab <- NULL
    if(!is.null(dn <- dimnames(X))) lab <- dn[[2]]
    if(length(lab) < 3) lab <- as.character(1:3)
    eps <- 0.05 * lcex
    text(c(0, s3+eps*0.7, -s3-eps*0.7),
         c(1+eps, -0.1*eps, -0.1*eps), lab, cex=lcex)
  }
  points((X[,2] - X[,3])*s3, X[,1], \dots)
}

ternary(Skye/100, ord=c(1,3,2))
}
\keyword{datasets}
