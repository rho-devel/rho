\name{saddle.distn}
\alias{saddle.distn}
\title{
  Saddlepoint Distribution Approximations for Bootstrap Statistics
}
\description{
  Approximate an entire distribution using saddlepoint methods.  This
  function can calculate simple and conditional saddlepoint distribution
  approximations for a univariate quantity of interest.  For the simple
  saddlepoint the quantity of interest is a linear combination of
  \bold{W} where \bold{W} is a vector of random variables.  For the
  conditional saddlepoint we require the distribution of one linear
  combination given the values of any number of other linear
  combinations. The distribution of \bold{W} must be one of multinomial,
  Poisson or binary.  The primary use of this function is to calculate
  quantiles of bootstrap distributions using saddlepoint approximations.
  Such quantiles are required by the function \code{\link{control}} to
  approximate the distribution of the linear approximation to a
  statistic.
}
\usage{
saddle.distn(A, u = NULL, alpha = NULL, wdist = "m", 
             type = "simp", npts = 20, t = NULL, t0 = NULL, 
             init = rep(0.1, d), mu = rep(0.5, n), LR = FALSE, 
             strata = NULL, \dots)
}
\arguments{
  \item{A}{
    This is a matrix of known coefficients or a function which returns
    such a matrix.  If a function then its first argument must be the
    point \code{t} at which a saddlepoint is required.   The most common
    reason for A being a function would be if the statistic is not
    itself a linear combination of the \bold{W} but is the solution to a
    linear estimating equation.
  }
  \item{u}{
    If \code{A} is a function then \code{u}  must also be a function
    returning a vector with length equal to the number of columns of the
    matrix returned by \code{A}. Usually all components other than the
    first will be constants as the other components are the values of
    the conditioning variables. If \code{A} is a matrix with more than
    one column (such as when \code{wdist = "cond"}) then \code{u} should
    be a vector with length one less than \code{ncol(A)}.  In this case
    \code{u} specifies the values of the conditioning variables.  If
    \code{A} is a matrix with one column or a vector then \code{u} is
    not used.
  }
  \item{alpha}{
    The alpha levels for the quantiles of the distribution which should be
    returned.  By default the 0.1, 0.5, 1, 2.5, 5, 10, 20, 50, 80, 90,
    95, 97.5, 99, 99.5 and 99.9 percentiles are calculated. 
  }
  \item{wdist}{
    The distribution of \bold{W}.  Possible values are \code{"m"}
    (multinomial), \code{"p"} (Poisson), or \code{"b"} (binary).
  }
  \item{type}{
    The type of saddlepoint to be used.  Possible values are
    \code{"simp"} (simple saddlepoint) and \code{"cond"} (conditional).
    If \code{wdist} is \code{"m"}, \code{type} is set to \code{"simp"}.
  }
  \item{npts}{
    The number of points at which the saddlepoint approximation should be
    calculated and then used to fit the spline.
  }
  \item{t}{
    A vector of points at which the saddlepoint approximations are
    calculated. These points should extend beyond the extreme quantiles
    required but still be in the possible range of the bootstrap
    distribution.  The observed value of the statistic should not be
    included in \code{t} as the distribution function approximation
    breaks down at that point.  The points should, however cover the
    entire effective range of the distribution including close to the
    centre. If \code{t} is supplied then \code{npts} is set to
    \code{length(t)}. When \code{t} is not supplied, the function
    attempts to find the effective range of the distribution and then
    selects points to cover this range.
  }
  \item{t0}{
    If \code{t} is not supplied then a vector of length 2 should be
    passed as \code{t0}. The first component of \code{t0} should be the
    centre of the distribution and the second should be an estimate of
    spread (such as a standard error). These two are then used to find
    the effective range of the distribution. The range finding mechanism
    does rely on an accurate estimate of location in \code{t0[1]}.
  }
  \item{init}{
    When \code{wdist} is \code{"m"}, this vector should contain the
    initial values to be passed to \code{nlmin} when it is called to
    solve the saddlepoint equations.
  }
  \item{mu}{
    The vector of parameter values for the distribution.  The
    default is that the components of \bold{W} are identically distributed.
  }
  \item{LR}{
    A logical flag.  When \code{LR} is \code{TRUE} the Lugananni-Rice
    cdf approximations are calculated and used to fit the spline.
    Otherwise the cdf approximations used are based on
    Barndorff-Nielsen's r*.
  }
  \item{strata}{
    A vector giving the strata when the rows of A relate to stratified
    data.  This is used only when \code{wdist} is \code{"m"}.
  }
  \item{\dots}{
    When \code{A} and \code{u} are functions any additional arguments
    are passed unchanged each time one of them is called.
  }
}
\value{
  The returned value is an object of class \code{"saddle.distn"}.  See the help
  file for \code{\link{saddle.distn.object}} for a description of such
  an object.
}
\details{
  The range at which the saddlepoint is used is such that the cdf
  approximation at the endpoints is more extreme than required by the
  extreme values of \code{alpha}.  The lower endpoint is found by
  evaluating the saddlepoint at the points \code{t0[1]-2*t0[2]},
  \code{t0[1]-4*t0[2]}, \code{t0[1]-8*t0[2]} etc.  until a point is
  found with a cdf approximation less than \code{min(alpha)/10}, then a
  bisection method is used to find the endpoint which has cdf
  approximation in the range (\code{min(alpha)/1000},
  \code{min(alpha)/10}). Then a number of, equally spaced, points are
  chosen between the lower endpoint and \code{t0[1]} until a total of
  \code{npts/2} approximations have been made. The remaining
  \code{npts/2} points are chosen to the right of \code{t0[1]} in a
  similar manner.  Any points which are very close to the centre of the
  distribution are then omitted as the cdf approximations are not
  reliable at the centre. A smoothing spline is then fitted to the
  probit of the saddlepoint distribution function approximations at the
  remaining points and the required quantiles are predicted from the
  spline.

  Sometimes the function will terminate with the message
  \code{"Unable to find range"}.  There are two main reasons why this may
  occur.  One is that the distribution is too discrete and/or the
  required quantiles too extreme, this can cause the function to be
  unable to find a point within the allowable range which is beyond the
  extreme quantiles.  Another possibility is that the value of
  \code{t0[2]} is too small and so too many steps are required to find
  the range. The first problem cannot be solved except by asking for
  less extreme quantiles, although for very discrete distributions the
  approximations may not be very good.  In the second case using a
  larger value of \code{t0[2]} will usually solve the problem.
}
\references{
  Booth, J.G. and Butler, R.W. (1990) Randomization distributions and 
  saddlepoint approximations in generalized linear models. 
  \emph{Biometrika}, \bold{77}, 787--796.

  Canty, A.J. and Davison, A.C. (1997) Implementation of saddlepoint 
  approximations to resampling distributions. 
  \emph{Computing Science and Statistics; Proceedings of the 28th
    Symposium on the Interface} 248--253.

  Davison, A.C. and Hinkley, D.V. (1997) 
  \emph{Bootstrap Methods and their Application}. Cambridge University Press.

  Jensen, J.L. (1995) \emph{Saddlepoint Approximations}. Oxford University Press.
}
\seealso{
  \code{\link{lines.saddle.distn}}, \code{\link{saddle}},
  \code{\link{saddle.distn.object}}, \code{\link{smooth.spline}}
}
\examples{
#  The bootstrap distribution of the mean of the air-conditioning 
#  failure data: fails to find value on R (and probably on S too)
air.t0 <- c(mean(aircondit$hours), sqrt(var(aircondit$hours)/12))
\dontrun{saddle.distn(A = aircondit$hours/12, t0 = air.t0)}

# alternatively using the conditional poisson
saddle.distn(A = cbind(aircondit$hours/12, 1), u = 12, wdist = "p",
             type = "cond", t0 = air.t0)

# Distribution of the ratio of a sample of size 10 from the bigcity 
# data, taken from Example 9.16 of Davison and Hinkley (1997).
ratio <- function(d, w) sum(d$x *w)/sum(d$u * w)
city.v <- var.linear(empinf(data = city, statistic = ratio))
bigcity.t0 <- c(mean(bigcity$x)/mean(bigcity$u), sqrt(city.v))
Afn <- function(t, data) cbind(data$x - t*data$u, 1)
ufn <- function(t, data) c(0,10)
saddle.distn(A = Afn, u = ufn, wdist = "b", type = "cond",
             t0 = bigcity.t0, data = bigcity)

# From Example 9.16 of Davison and Hinkley (1997) again, we find the 
# conditional distribution of the ratio given the sum of city$u.
Afn <- function(t, data) cbind(data$x-t*data$u, data$u, 1)
ufn <- function(t, data) c(0, sum(data$u), 10)
city.t0 <- c(mean(city$x)/mean(city$u), sqrt(city.v))
saddle.distn(A = Afn, u = ufn, wdist = "p", type = "cond", t0 = city.t0, 
             data = city)
}
\keyword{nonparametric}
\keyword{smooth}
\keyword{dplot}
% Converted by Sd2Rd version 1.15.
