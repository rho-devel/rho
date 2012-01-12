\name{boot.ci}
\alias{boot.ci}
\title{
  Nonparametric Bootstrap Confidence Intervals
}
\description{
  This function generates 5 different types of equi-tailed two-sided
  nonparametric confidence intervals.  These are the first order normal 
  approximation, the basic bootstrap interval, the studentized bootstrap 
  interval, the bootstrap percentile interval, and the adjusted bootstrap 
  percentile (BCa) interval.  All or a subset of these intervals can be 
  generated.  
}
\usage{
boot.ci(boot.out, conf = 0.95, type = "all", 
        index = 1:min(2,length(boot.out$t0)), var.t0 = NULL, 
        var.t = NULL, t0 = NULL, t = NULL, L = NULL,
        h = function(t) t, hdot = function(t) rep(1,length(t)),
        hinv = function(t) t, \dots)
}
\arguments{
  \item{boot.out}{
    An object of class \code{"boot"} containing the output of a bootstrap
    calculation.  
  }
  \item{conf}{
    A scalar or vector containing the confidence level(s) of the required
    interval(s).
  }
  \item{type}{
    A vector of character strings representing the type of intervals
    required. The value should be any subset of the values
    \code{c("norm","basic", "stud", "perc", "bca")} or simply \code{"all"}
    which will compute all five types of intervals.
  }
  \item{index}{
    This should be a vector of length 1 or 2.  The first element of
    \code{index} indicates the position of the variable of interest in
    \code{boot.out$t0} and the relevant column in \code{boot.out$t}.  The
    second element indicates the position of the variance of the variable of
    interest.  If both \code{var.t0} and \code{var.t} are supplied then the
    second element of \code{index} (if present) is ignored.  The default is
    that the variable of interest is in position 1 and its variance is in
    position 2 (as long as there are 2 positions in \code{boot.out$t0}).
  }
  \item{var.t0}{
    If supplied, a value to be used as an estimate of the variance of
    the statistic for the normal approximation and studentized intervals.
    If it is not supplied and \code{length(index)} is 2 then \code{var.t0}
    defaults to \code{boot.out$t0[index[2]]} otherwise \code{var.t0} is
    undefined.  For studentized intervals \code{var.t0} must be defined.
    For the normal approximation, if \code{var.t0} is undefined it defaults
    to \code{var(t)}.  If a transformation is supplied through the argument
    \code{h} then \code{var.t0} should be the variance of the untransformed
    statistic.
  }
  \item{var.t}{
    This is a vector (of length \code{boot.out$R}) of variances of the
    bootstrap replicates of the variable of interest.  It is used only for
    studentized intervals.  If it is not supplied and \code{length(index)}
    is 2 then \code{var.t} defaults to \code{boot.out$t[,index[2]]},
    otherwise its value is undefined which will cause an error for
    studentized intervals.  If a transformation is supplied through the
    argument \code{h} then \code{var.t} should be the variance of the
    untransformed bootstrap statistics.
  }
  \item{t0}{
    The observed value of the statistic of interest.  The default value
    is \code{boot.out$t0[index[1]]}.  Specification of \code{t0} and
    \code{t} allows the user to get intervals for a transformed statistic
    which may not be in the bootstrap output object.  See the second example
    below.  An alternative way of achieving this would be to supply the
    functions \code{h}, \code{hdot}, and \code{hinv} below.
  }
  \item{t}{
    The bootstrap replicates of the statistic of interest.  It must be a
    vector of length \code{boot.out$R}.  It is an error to supply one of
    \code{t0} or \code{t} but not the other.  Also if studentized
    intervals are required and \code{t0} and \code{t} are supplied then
    so should be \code{var.t0} and \code{var.t}.  The default value is
    \code{boot.out$t[,index]}.
  }
  \item{L}{
    The empirical influence values of the statistic of interest for the
    observed data.  These are used only for BCa intervals.  If a
    transformation is supplied through the parameter \code{h} then
    \code{L} should be the influence values for \code{t}; the values for
    \code{h(t)} are derived from these and \code{hdot} within the
    function. If \code{L} is not supplied then the values are calculated
    using \code{empinf} if they are needed.
  }
  \item{h}{
    A function defining a transformation.  The intervals are calculated
    on the scale of \code{h(t)} and the inverse function \code{hinv}
    applied to the resulting intervals.  It must be a function of one
    variable only and for a vector argument, it must return a vector of
    the same length, i.e. \code{h(c(t1,t2,t3))} should return
    \code{c(h(t1),h(t2),h(t3))}. The default is the identity function.
  }
  \item{hdot}{
    A function of one argument returning the derivative of \code{h}.  It
    is a required argument if \code{h} is supplied and normal,
    studentized or BCa intervals are required.  The function is used for
    approximating the variances of \code{h(t0)} and \code{h(t)} using
    the delta method, and also for finding the empirical influence
    values for BCa intervals.  Like \code{h} it should be able to take a
    vector argument and return a vector of the same length.  The default
    is the constant function 1.
  }
  \item{hinv}{
    A function, like \code{h}, which returns the inverse of \code{h}.
    It is used to transform the intervals calculated on the scale of
    \code{h(t)} back to the original scale. The default is the identity
    function.  If \code{h} is supplied but \code{hinv} is not, then the
    intervals returned will be on the transformed scale.
  }
  \item{\dots}{
    Any extra arguments that \code{boot.out$statistic} is
    expecting. These arguments are needed only if BCa intervals are required
    and \code{L} is not supplied since in that case \code{L} is calculated
    through a call to \code{empinf} which calls \code{boot.out$statistic}.
  }
}
\details{
  The formulae on which the calculations are based can be found in
  Chapter 5 of Davison and Hinkley (1997).  Function \code{boot} must be
  run prior to running this function to create the object to be passed as
  \code{boot.out}.

  Variance estimates are required for studentized intervals.  The variance
  of the observed statistic is optional for normal theory intervals.  If
  it is not supplied then the bootstrap estimate of variance is used.  The
  normal intervals also use the bootstrap bias correction.

  Interpolation on the normal quantile scale is used when a non-integer order
  statistic is required.  If the order statistic used is the smallest or
  largest of the R values in boot.out a warning is generated and such
  intervals should not be considered reliable.  
}
\value{
  An object of type \code{"bootci"} which contains the intervals.
  It has components
  \item{R}{
    The number of bootstrap replicates on which the intervals were based.
  }
  \item{t0}{
    The observed value of the statistic on the same scale as the intervals.
  }
  \item{call}{
    The call to \code{boot.ci} which generated the object.

    It will also contain one or more of the following components depending
    on the value of \code{type} used in the call to \code{bootci}.
  }
  \item{normal}{
    A matrix of intervals calculated using the normal approximation.  It will
    have 3 columns, the first being the level and the other two being the upper
    and lower endpoints of the intervals.
  }
  \item{basic}{
    The intervals calculated using the basic bootstrap method.
  }
  \item{student}{
    The intervals calculated using the studentized bootstrap method.
  }
  \item{percent}{
    The intervals calculated using the bootstrap percentile method.
  }
  \item{bca}{
    The intervals calculated using the adjusted bootstrap percentile
    (BCa) method.

    These latter four components will be matrices with 5 columns,  the
    first column containing the level, the next two containing the
    indices of the order statistics used in the calculations and the
    final two the calculated endpoints themselves. 
  }
}
\references{
  Davison, A.C. and Hinkley, D.V. (1997) 
  \emph{Bootstrap Methods and Their Application}, Chapter 5.
  Cambridge University Press.

  DiCiccio, T.J. and Efron  B. (1996) Bootstrap confidence intervals (with 
  Discussion). \emph{Statistical Science}, \bold{11}, 189--228.

  Efron, B. (1987) Better bootstrap confidence intervals (with Discussion).
  \emph{Journal of the American Statistical Association}, \bold{82}, 171--200.
}
\seealso{
  \code{\link{abc.ci}}, \code{\link{boot}},
  \code{\link{empinf}}, \code{\link{norm.ci}}
}
\examples{
# confidence intervals for the city data
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
city.boot <- boot(city, ratio, R = 999, stype = "w", sim = "ordinary")
boot.ci(city.boot, conf = c(0.90, 0.95),
        type = c("norm", "basic", "perc", "bca"))

# studentized confidence interval for the two sample 
# difference of means problem using the final two series
# of the gravity data. 
diff.means <- function(d, f)
{    n <- nrow(d)
     gp1 <- 1:table(as.numeric(d$series))[1]
     m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
     m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
     ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
     ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
     c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
grav1 <- gravity[as.numeric(gravity[,2]) >= 7, ]
grav1.boot <- boot(grav1, diff.means, R = 999, stype = "f",
                   strata = grav1[ ,2])
boot.ci(grav1.boot, type = c("stud", "norm"))

# Nonparametric confidence intervals for mean failure time 
# of the air-conditioning data as in Example 5.4 of Davison
# and Hinkley (1997)
mean.fun <- function(d, i) 
{    m <- mean(d$hours[i])
     n <- length(i)
     v <- (n-1)*var(d$hours[i])/n^2
     c(m, v)
}
air.boot <- boot(aircondit, mean.fun, R = 999)
boot.ci(air.boot, type = c("norm", "basic", "perc", "stud"))

# Now using the log transformation
# There are two ways of doing this and they both give the
# same intervals.

# Method 1
boot.ci(air.boot, type = c("norm", "basic", "perc", "stud"), 
        h = log, hdot = function(x) 1/x)

# Method 2
vt0 <- air.boot$t0[2]/air.boot$t0[1]^2
vt <- air.boot$t[, 2]/air.boot$t[ ,1]^2
boot.ci(air.boot, type = c("norm", "basic", "perc", "stud"), 
        t0 = log(air.boot$t0[1]), t = log(air.boot$t[,1]),
        var.t0 = vt0, var.t = vt)
}
\keyword{nonparametric}
\keyword{htest}

