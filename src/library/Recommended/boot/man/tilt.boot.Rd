\name{tilt.boot}
\alias{tilt.boot}
\title{
  Non-parametric Tilted Bootstrap
}
\description{
  This function will run an initial bootstrap with equal resampling 
  probabilities (if required) and will use the output of the initial run to 
  find resampling probabilities which put the
  value of the statistic at required values.  It then runs an importance
  resampling bootstrap using the calculated probabilities as the resampling
  distribution.
}
\usage{
tilt.boot(data, statistic, R, sim = "ordinary", stype = "i", 
          strata = rep(1, n), L = NULL, theta = NULL, 
          alpha = c(0.025, 0.975), tilt = TRUE, width = 0.5, 
          index = 1, \dots)
}
\arguments{
  \item{data}{
    The data as a vector, matrix or data frame.  If it is a matrix or
    data frame then each row is considered as one (multivariate)
    observation.
  }
  \item{statistic}{
    A function which when applied to data returns a vector containing the
    statistic(s) of interest.  It must take at least two arguments.  The first
    argument will always be \code{data} and the second should be a
    vector of indices, weights or frequencies describing the bootstrap
    sample.  Any other arguments must be supplied to \code{tilt.boot}
    and will be passed unchanged to statistic each time it is called.
  }
  \item{R}{
    The number of bootstrap replicates required.  This will generally be
    a vector, the first value stating how many uniform bootstrap
    simulations are to be performed at the initial stage.  The remaining
    values of \code{R} are the number of simulations to be performed
    resampling from each reweighted distribution. The first value of
    \code{R} must always be present, a value of 0 implying that no
    uniform resampling is to be carried out.  Thus \code{length(R)}
    should always equal \code{1+length(theta)}.
  }
  \item{sim}{
    This is a character string indicating the type of bootstrap
    simulation required.  There are only two possible values that this
    can take: \code{"ordinary"} and \code{"balanced"}.  If other
    simulation types are required for the initial un-weighted bootstrap
    then it will be necessary to run \code{boot}, calculate the weights
    appropriately, and run \code{boot} again using the calculated
    weights.
  }
  \item{stype}{
    A character string indicating the type of second argument expected
    by \code{statistic}.  The possible values that \code{stype} can take
    are \code{"i"} (indices), \code{"w"} (weights) and \code{"f"}
    (frequencies).
  }
  \item{strata}{
    An integer vector or factor representing the strata for multi-sample
    problems.
  }
  \item{L}{
    The empirical influence values for the statistic of interest.  They
    are used only for exponential tilting when \code{tilt} is
    \code{TRUE}.  If \code{tilt} is \code{TRUE} and they are not
    supplied then \code{tilt.boot} uses \code{empinf} to calculate
    them.
  }
  \item{theta}{
    The required parameter value(s) for the tilted distribution(s).
    There should be one value of \code{theta} for each of the
    non-uniform distributions.  If \code{R[1]} is 0 \code{theta} is a
    required argument.  Otherwise \code{theta} values can be estimated
    from the initial uniform bootstrap and the values in \code{alpha}.
  }
  \item{alpha}{
    The alpha level to which tilting is required.  This parameter is
    ignored if \code{R[1]} is 0 or if \code{theta} is supplied,
    otherwise it is used to find the values of \code{theta} as quantiles
    of the initial uniform bootstrap.  In this case \code{R[1]} should
    be large enough that \code{min(c(alpha, 1-alpha))*R[1] > 5}, if this
    is not the case then a warning is generated to the effect that the
    \code{theta} are extreme values and so the tilted output may be
    unreliable. 
  }
  \item{tilt}{
    A logical variable which if \code{TRUE} (the default) indicates that
    exponential tilting should be used, otherwise local frequency
    smoothing (\code{smooth.f}) is used.  If \code{tilt} is \code{FALSE}
    then \code{R[1]} must be positive.  In fact in this case the value
    of \code{R[1]} should be fairly large (in the region of 500 or
    more).
  }
  \item{width}{
    This argument is used only if \code{tilt} is \code{FALSE}, in which
    case it is passed unchanged to \code{smooth.f} as the standardized
    bandwidth for the smoothing operation.  The value should generally
    be in the range (0.2, 1). See \code{smooth.f} for for more details.
  }
  \item{index}{
    The index of the statistic of interest in the output from
    \code{statistic}.  By default the first element of the output of
    \code{statistic} is used.
  }
  \item{\dots}{
    Any additional arguments required by \code{statistic}.  These are
    passed unchanged to \code{statistic} each time it is called.
  }
}
\value{
An object of class \code{"boot"} with the following components

\item{t0}{
  The observed value of the statistic on the original data.
}
\item{t}{
  The values of the bootstrap replicates of the statistic.  There will
  be \code{sum(R)} of these, the first \code{R[1]} corresponding to the
  uniform bootstrap and the remainder to the tilted bootstrap(s).
}
\item{R}{
  The input vector of the number of bootstrap replicates.
}
\item{data}{
  The original data as supplied.
}
\item{statistic}{
  The \code{statistic} function as supplied.
}
\item{sim}{
  The simulation type used in the bootstrap(s), it can either be
  \code{"ordinary"} or \code{"balanced"}.
}
\item{stype}{
  The type of statistic supplied, it is the same as the input value
  \code{stype}.
}
\item{call}{
  A copy of the original call to \code{tilt.boot}.
}
\item{strata}{
  The strata as supplied.
}
\item{weights}{
  The matrix of weights used.  If \code{R[1]} is greater than 0 then the
  first row will be the uniform weights and each subsequent row the
  tilted weights. If \code{R[1]} equals 0 then the uniform weights are
  omitted and only the tilted weights are output.
}
\item{theta}{
  The values of \code{theta} used for the tilted distributions.  These
  are either the input values or the values derived from the uniform
  bootstrap and \code{alpha}.
}
}
\references{
Booth, J.G., Hall, P. and Wood, A.T.A. (1993) Balanced importance resampling 
for the bootstrap. \emph{Annals of Statistics}, \bold{21}, 286--298.

Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.

Hinkley, D.V. and Shi, S. (1989) Importance sampling and the nested bootstrap. 
\emph{Biometrika}, \bold{76}, 435--446.
}
\seealso{
\code{\link{boot}}, \code{\link{exp.tilt}}, \code{\link{Imp.Estimates}}, \code{\link{imp.weights}}, \code{\link{smooth.f}}
}
\examples{
# Note that these examples can take a while to run.

# Example 9.9 of Davison and Hinkley (1997).
grav1 <- gravity[as.numeric(gravity[,2]) >= 7, ]
grav.fun <- function(dat, w, orig) {
     strata <- tapply(dat[, 2], as.numeric(dat[, 2]))
     d <- dat[, 1]
     ns <- tabulate(strata)
     w <- w/tapply(w, strata, sum)[strata]
     mns <- as.vector(tapply(d * w, strata, sum)) # drop names
     mn2 <- tapply(d * d * w, strata, sum)
     s2hat <- sum((mn2 - mns^2)/ns)
     c(mns[2]-mns[1],s2hat,(mns[2]-mns[1]-orig)/sqrt(s2hat))
}
grav.z0 <- grav.fun(grav1, rep(1, 26), 0)
tilt.boot(grav1, grav.fun, R = c(249, 375, 375), stype = "w", 
          strata = grav1[,2], tilt = TRUE, index = 3, orig = grav.z0[1]) 


#  Example 9.10 of Davison and Hinkley (1997) requires a balanced 
#  importance resampling bootstrap to be run.  In this example we 
#  show how this might be run.  
acme.fun <- function(data, i, bhat) {
     d <- data[i,]
     n <- nrow(d)
     d.lm <- glm(d$acme~d$market)
     beta.b <- coef(d.lm)[2]
     d.diag <- boot::glm.diag(d.lm)
     SSx <- (n-1)*var(d$market)
     tmp <- (d$market-mean(d$market))*d.diag$res*d.diag$sd
     sr <- sqrt(sum(tmp^2))/SSx
     c(beta.b, sr, (beta.b-bhat)/sr)
}
acme.b <- acme.fun(acme, 1:nrow(acme), 0)
acme.boot1 <- tilt.boot(acme, acme.fun, R = c(499, 250, 250), 
                        stype = "i", sim = "balanced", alpha = c(0.05, 0.95), 
                        tilt = TRUE, index = 3, bhat = acme.b[1])
}
\keyword{nonparametric}
