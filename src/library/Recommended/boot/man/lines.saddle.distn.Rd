\name{lines.saddle.distn}
\alias{lines.saddle.distn}
\title{
  Add a Saddlepoint Approximation to a Plot
}
\description{
  This function adds a line corresponding to a saddlepoint density or 
  distribution function approximation to the current plot.
}
\usage{
\method{lines}{saddle.distn}(x, dens = TRUE, h = function(u) u, J = function(u) 1, 
      npts = 50, lty = 1, \dots)
}
\arguments{
  \item{x}{
    An object of class \code{"saddle.distn"}  (see
    \code{\link{saddle.distn.object}} representing a saddlepoint
    approximation to a distribution.
  }
  \item{dens}{
    A logical variable indicating whether the saddlepoint density
    (\code{TRUE}; the default) or the saddlepoint distribution function
    (\code{FALSE}) should be plotted.
  }
  \item{h}{
    Any transformation of the variable that is required.  Its first argument
    must be the value at which the approximation is being performed and the
    function must be vectorized.
  }
  \item{J}{
    When \code{dens=TRUE} this function specifies the Jacobian for any
    transformation that may be necessary.  The first argument of \code{J}
    must the value at which the approximation is being performed and the
    function must be vectorized. If \code{h} is supplied \code{J} must
    also be supplied and both must have the same argument list.
  }
  \item{npts}{
    The number of points to be used for the plot.  These points will be evenly
    spaced over the range of points used in finding the saddlepoint
    approximation.
  }
  \item{lty}{
    The line type to be used.
  }
  \item{\dots}{
    Any additional arguments to \code{h} and \code{J}.
  }
}
\value{
  \code{sad.d} is returned invisibly.
}
\section{Side Effects}{
  A line is added to the current plot.
}
\details{
  The function uses \code{smooth.spline} to produce the saddlepoint
  curve.  When \code{dens=TRUE} the spline is on the log scale and when
  \code{dens=FALSE} it is on the probit scale.
}
\seealso{
  \code{\link{saddle.distn}}
}
\references{
  Davison, A.C. and Hinkley, D.V. (1997) 
  \emph{Bootstrap Methods and Their Application}. Cambridge University Press. 
}
\examples{
# In this example we show how a plot such as that in Figure 9.9 of
# Davison and Hinkley (1997) may be produced.  Note the large number of
# bootstrap replicates required in this example.
expdata <- rexp(12)
vfun <- function(d, i) {
     n <- length(d)
     (n-1)/n*var(d[i])
}
exp.boot <- boot(expdata,vfun, R = 9999)
exp.L <- (expdata - mean(expdata))^2 - exp.boot$t0
exp.tL <- linear.approx(exp.boot, L = exp.L)
hist(exp.tL, nclass = 50, probability = TRUE)
exp.t0 <- c(0, sqrt(var(exp.boot$t)))
exp.sp <- saddle.distn(A = exp.L/12,wdist = "m", t0 = exp.t0)

# The saddlepoint approximation in this case is to the density of
# t-t0 and so t0 must be added for the plot.
lines(exp.sp, h = function(u, t0) u+t0, J = function(u, t0) 1,
      t0 = exp.boot$t0)
}
\keyword{aplot}
\keyword{smooth}
\keyword{nonparametric}
