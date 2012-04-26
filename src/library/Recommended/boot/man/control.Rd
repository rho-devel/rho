\name{control}
\alias{control}
\title{
  Control Variate Calculations
}
\description{
  This function will find control variate estimates from a bootstrap
  output object.  It can either find the adjusted bias estimate using
  post-simulation balancing or it can estimate the bias, variance, third
  cumulant and quantiles, using the linear approximation as a control
  variate.
}
\usage{
control(boot.out, L = NULL, distn = NULL, index = 1, t0 = NULL,
        t = NULL, bias.adj = FALSE, alpha = NULL, \dots)
}
\arguments{
  \item{boot.out}{
    A bootstrap output object returned from \code{boot}.  The bootstrap
    replicates must have been generated using the usual nonparametric
    bootstrap.
  }
  \item{L}{
    The empirical influence values for the statistic of interest.  If
    \code{L} is not supplied then \code{empinf} is called to calculate
    them from \code{boot.out}.
  }
  \item{distn}{
    If present this must be the output from \code{smooth.spline} giving
    the distribution function of the linear approximation.  This is used
    only if \code{bias.adj} is \code{FALSE}.  Normally this would be
    found using a saddlepoint approximation. If it is not supplied in
    that case then it is calculated by \code{saddle.distn}.
  }
  \item{index}{
    The index of the variable of interest in the output of
    \code{boot.out$statistic}.
  }
  \item{t0}{
    The observed value of the statistic of interest on the original data
    set \code{boot.out$data}.  This argument is used only if
    \code{bias.adj} is \code{FALSE}. The input value is ignored if
    \code{t} is not also supplied.  The default value is is
    \code{boot.out$t0[index]}.
  }
  \item{t}{
    The bootstrap replicate values of the statistic of interest.  This
    argument is used only if \code{bias.adj} is \code{FALSE}.  The input
    is ignored if \code{t0} is not supplied also.  The default value is
    \code{boot.out$t[,index]}.
  }
  \item{bias.adj}{
    A logical variable which if \code{TRUE} specifies that the adjusted
    bias estimate using post-simulation balance is all that is required.
    If \code{bias.adj} is \code{FALSE} (default) then the linear
    approximation to the statistic is calculated and used as a control
    variate in estimates of the bias, variance and third cumulant as
    well as quantiles.
  }
  \item{alpha}{
    The alpha levels for the required quantiles if \code{bias.adj} is
    \code{FALSE}.
  }
  \item{\dots}{
    Any additional arguments that \code{boot.out$statistic} requires.
    These are passed unchanged every time \code{boot.out$statistic} is
    called.  \code{boot.out$statistic} is called once if \code{bias.adj}
    is \code{TRUE}, otherwise it may be called by \code{empinf} for
    empirical influence calculations if \code{L} is not supplied.
  }
}
\value{
  If \code{bias.adj} is \code{TRUE} then the returned value is the
  adjusted bias estimate.

  If \code{bias.adj} is \code{FALSE} then the returned value is a list
  with the following components
  
  \item{L}{
    The empirical influence values used.  These are the input values if
    supplied, and otherwise they are the values calculated by
    \code{empinf}.
  }
  \item{tL}{
    The linear approximations to the bootstrap replicates \code{t} of
    the statistic of interest.
  }
  \item{bias}{
    The control estimate of bias using the linear approximation to
    \code{t} as a control variate.
  }
  \item{var}{
    The control estimate of variance using the linear approximation to
    \code{t} as a control variate.
  }
  \item{k3}{
    The control estimate of the third cumulant using the linear
    approximation to \code{t} as a control variate.
  }
  \item{quantiles}{
    A matrix with two columns; the first column are the alpha levels
    used for the quantiles and the second column gives the corresponding
    control estimates of the quantiles using the linear approximation to
    \code{t} as a control variate.
  }
  \item{distn}{
    An output object from \code{smooth.spline} describing the
    saddlepoint approximation to the bootstrap distribution of the
    linear approximation to \code{t}.  If \code{distn} was supplied on
    input then this is the same as the input otherwise it is calculated
    by a call to \code{saddle.distn}.
  }
}
\details{
  If \code{bias.adj} is \code{FALSE} then the linear approximation to
  the statistic is found and evaluated at each bootstrap replicate.
  Then using the equation \emph{T* = Tl*+(T*-Tl*)}, moment estimates can
  be found.  For quantile estimation the distribution of the linear
  approximation to \code{t} is approximated very accurately by
  saddlepoint methods, this is then combined with the bootstrap
  replicates to approximate the bootstrap distribution of \code{t} and
  hence to estimate the bootstrap quantiles of \code{t}.
}
\references{
  Davison, A.C. and Hinkley, D.V. (1997) 
  \emph{Bootstrap Methods and Their Application}. Cambridge University Press.
  
  Davison, A.C., Hinkley, D.V. and Schechtman, E. (1986) Efficient bootstrap 
  simulation. \emph{Biometrika}, \bold{73}, 555--566.

  Efron, B. (1990) More efficient bootstrap computations.
  \emph{Journal of the American Statistical Association}, \bold{55}, 79--89.
}
\seealso{
\code{\link{boot}}, \code{\link{empinf}}, \code{\link{k3.linear}}, \code{\link{linear.approx}}, \code{\link{saddle.distn}}, \code{\link{smooth.spline}}, \code{\link{var.linear}}
}
\examples{
# Use of control variates for the variance of the air-conditioning data
mean.fun <- function(d, i)
{    m <- mean(d$hours[i])
     n <- nrow(d)
     v <- (n-1)*var(d$hours[i])/n^2
     c(m, v)
}
air.boot <- boot(aircondit, mean.fun, R = 999)
control(air.boot, index = 2, bias.adj = TRUE)
air.cont <- control(air.boot, index = 2)
# Now let us try the variance on the log scale.
air.cont1 <- control(air.boot, t0 = log(air.boot$t0[2]),
                     t = log(air.boot$t[, 2]))
}
\keyword{nonparametric}
