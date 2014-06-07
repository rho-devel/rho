\name{boot}
\alias{boot}
\alias{boot.return}
\alias{c.boot}
\title{
Bootstrap Resampling
}
\description{
  Generate \code{R} bootstrap replicates of a statistic applied to data.  Both
  parametric and nonparametric resampling are possible.  For the nonparametric
  bootstrap, possible resampling methods are the ordinary bootstrap, the 
  balanced bootstrap, antithetic resampling, and permutation.
  For nonparametric multi-sample problems stratified resampling is used:
  this is specified by including a vector of strata in the call to boot.
  Importance resampling weights may be specified.
}
\usage{
boot(data, statistic, R, sim = "ordinary", stype = c("i", "f", "w"), 
     strata = rep(1,n), L = NULL, m = 0, weights = NULL, 
     ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ...,
     parallel = c("no", "multicore", "snow"),
     ncpus = getOption("boot.ncpus", 1L), cl = NULL)
}
\arguments{
  \item{data}{
    The data as a vector, matrix or data frame.  If it is a matrix or
    data frame then each row is considered as one multivariate
    observation.
  }
  \item{statistic}{
    A function which when applied to data returns a vector containing
    the statistic(s) of interest.  When \code{sim = "parametric"}, the
    first argument to \code{statistic} must be the data.  For each
    replicate a simulated dataset returned by \code{ran.gen} will be
    passed.  In all other cases \code{statistic} must take at least two
    arguments.  The first argument passed will always be the original
    data.  The second will be a vector of indices, frequencies or weights
    which define the bootstrap sample.  Further, if predictions are
    required, then a third argument is required which would be a vector
    of the random indices used to generate the bootstrap predictions.
    Any further arguments can be passed to \code{statistic} through the
    \code{\dots} argument.
  }
  \item{R}{
    The number of bootstrap replicates.  Usually this will be a single
    positive integer.  For importance resampling, some resamples may use
    one set of weights and others use a different set of weights.  In
    this case \code{R} would be a vector of integers where each
    component gives the number of resamples from each of the rows of
    weights.
  }
  \item{sim}{
    A character string indicating the type of simulation required.
    Possible values are \code{"ordinary"} (the default),
    \code{"parametric"}, \code{"balanced"}, \code{"permutation"}, or
    \code{"antithetic"}.  Importance resampling is specified by
    including importance weights; the type of importance resampling must
    still be specified but may only be \code{"ordinary"} or
    \code{"balanced"} in this case.
  }
  \item{stype}{
    A character string indicating what the second argument of \code{statistic}
    represents.  Possible values of stype are \code{"i"} (indices - the
    default), \code{"f"} (frequencies), or \code{"w"} (weights).  Not
    used for \code{sim = "parametric"}.
  }
  \item{strata}{
    An integer vector or factor specifying the strata for multi-sample
    problems.  This may be specified for any simulation, but is ignored
    when \code{sim = "parametric"}.  When \code{strata} is
    supplied for a nonparametric bootstrap, the simulations are done
    within the specified strata.
  }
  \item{L}{
    Vector of influence values evaluated at the observations.  This is
    used only when \code{sim} is \code{"antithetic"}.  If not supplied,
    they are calculated through a call to \code{empinf}.  This will use
    the infinitesimal jackknife provided that \code{stype} is
    \code{"w"}, otherwise the usual jackknife is used.
  }
  \item{m}{
    The number of predictions which are to be made at each bootstrap
    replicate.  This is most useful for (generalized) linear models.
    This can only be used when \code{sim} is \code{"ordinary"}.
    \code{m} will usually be a single integer but, if there are strata,
    it may be a vector with length equal to the number of strata,
    specifying how many of the errors for prediction should come from
    each strata.  The actual predictions should be returned as the final
    part of the output of \code{statistic}, which should also take an
    argument giving the vector of indices of the errors to be used for
    the predictions.
  }
  \item{weights}{
    Vector or matrix of importance weights.  If a vector then it should
    have as many elements as there are observations in \code{data}.
    When simulation from more than one set of weights is required,
    \code{weights} should be a matrix where each row of the matrix is
    one set of importance weights.  If \code{weights} is a matrix then
    \code{R} must be a vector of length \code{nrow(weights)}.  This
    parameter is ignored if \code{sim} is not \code{"ordinary"} or
    \code{"balanced"}.
  }
  \item{ran.gen}{
    This function is used only when \code{sim = "parametric"}
    when it describes how random values are to be generated.  It should
    be a function of two arguments.  The first argument should be the
    observed data and the second argument consists of any other
    information needed (e.g. parameter estimates).  The second argument
    may be a list, allowing any number of items to be passed to
    \code{ran.gen}.  The returned value should be a simulated data set
    of the same form as the observed data which will be passed to
    \code{statistic} to get a bootstrap replicate.  It is important that the
    returned value be of the same shape and type as the original
    dataset.  If \code{ran.gen} is not specified, the default is a
    function which returns the original \code{data} in which case all
    simulation should be included as part of \code{statistic}.  Use of
    \code{sim = "parametric"} with a suitable \code{ran.gen} allows the
    user to implement any types of nonparametric resampling which are
    not supported directly.
  }
  \item{mle}{
    The second argument to be passed to \code{ran.gen}.  Typically these
    will be maximum likelihood estimates of the parameters.  For
    efficiency \code{mle} is often a list containing all of the objects
    needed by \code{ran.gen} which can be calculated using the original
    data set only.
  }
  \item{simple}{logical, only allowed to be \code{TRUE} for
    \code{sim = "ordinary", stype = "i", n = 0} (otherwise ignored with a
    warning).  By default a \code{n} by \code{R} index array is created:
    this can be large and if \code{simple = TRUE} this is avoided by
    sampling separately for each replication, which is slower but uses
    less memory.
  }
  \item{\dots}{
    Other named arguments for \code{statistic} which are passed
    unchanged each time it is called.  Any such arguments to
    \code{statistic} should follow the arguments which \code{statistic} is
    required to have for the simulation.  Beware of partial matching to
    arguments of \code{boot} listed above, and that arguments named
    \code{X} and \code{FUN} cause conflicts in some versions of
    \pkg{boot} (but not this one).
  }
  \item{parallel}{
    The type of parallel operation to be used (if any).  If missing, the
    default is taken from the option \code{"boot.parallel"} (and if that
    is not set, \code{"no"}).
  }
  \item{ncpus}{
    integer: number of processes to be used in parallel operation:
    typically one would chose this to the number of available CPUs.
  }
  \item{cl}{
    An optional \pkg{parallel} or \pkg{snow} cluster for use if
    \code{parallel = "snow"}.  If not supplied, a cluster on the
    local machine is created for the duration of the \code{boot} call.
  }
}
\value{
  The returned value is an object of class \code{"boot"}, containing the
  following components:

  \item{t0}{
    The observed value of \code{statistic} applied to \code{data}. 
  }
  \item{t}{
    A matrix with \code{sum(R)} rows each of which is a bootstrap replicate
    of the result of calling \code{statistic}.
  }
  \item{R}{
    The value of \code{R} as passed to \code{boot}.
  }
  \item{data}{
    The \code{data} as passed to \code{boot}.
  }
  \item{seed}{
    The value of \code{.Random.seed} when \code{boot} was called.  
  }
  \item{statistic}{
    The function \code{statistic} as passed to \code{boot}.
  }
  \item{sim}{
    Simulation type used.
  }
  \item{stype}{
    Statistic type as passed to \code{boot}.
  }
  \item{call}{
    The original call to \code{boot}.
  }
  \item{strata}{
    The strata used.  This is the vector passed to \code{boot}, if it
    was supplied or a vector of ones if there were no strata.  It is not
    returned if \code{sim} is \code{"parametric"}.
  }
  \item{weights}{
    The importance sampling weights as passed to \code{boot} or the empirical 
    distribution function weights if no importance sampling weights were
    specified.  It is omitted if \code{sim} is not one of
    \code{"ordinary"} or \code{"balanced"}.
  }
  \item{pred.i}{
    If predictions are required (\code{m > 0}) this is the matrix of
    indices at which predictions were calculated as they were passed to
    statistic.  Omitted if \code{m} is \code{0} or \code{sim} is not
    \code{"ordinary"}. 
  }
  \item{L}{
    The influence values used when \code{sim} is \code{"antithetic"}.
    If no such values were specified and \code{stype} is not \code{"w"}
    then \code{L} is returned as consecutive integers corresponding to
    the assumption that data is ordered by influence values. This
    component is omitted when \code{sim} is not \code{"antithetic"}.
  }
  \item{ran.gen}{
    The random generator function used if \code{sim} is
    \code{"parametric"}. This component is omitted for any other value
    of \code{sim}.
  }
  \item{mle}{
    The parameter estimates passed to \code{boot} when \code{sim} is
    \code{"parametric"}.  It is omitted for all other values of
    \code{sim}.
  }
  
  There are \code{c}, \code{plot} and \code{print} methods for this class.
}
\details{
  The statistic to be bootstrapped can be as simple or complicated as
  desired as long as its arguments correspond to the dataset and (for
  a nonparametric bootstrap) a vector of indices, frequencies or
  weights.  \code{statistic} is treated as a black box by the
  \code{boot} function and is not checked to ensure that these
  conditions are met.
    
  The first order balanced bootstrap is described in Davison, Hinkley
  and Schechtman (1986).  The antithetic bootstrap is described by
  Hall (1989) and is experimental, particularly when used with strata.
  The other non-parametric simulation types are the ordinary bootstrap
  (possibly with unequal probabilities), and permutation which returns
  random permutations of cases. All of these methods work
  independently within strata if that argument is supplied.
    
  For the parametric bootstrap it is necessary for the user to specify
  how the resampling is to be conducted.  The best way of
  accomplishing this is to specify the function \code{ran.gen} which
  will return a simulated data set from the observed data set and a
  set of parameter estimates specified in \code{mle}.
}

\section{Parallel operation}{
  When \code{parallel = "multicore"} is used (not available on Windows),
  each worker process inherits the environment of the current session,
  including the workspace and the loaded namespaces and attached
  packages (but not the random number seed: see below).

  More work is needed when \code{parallel = "snow"} is used: the worker
  processes are newly created \R processes, and \code{statistic} needs
  to arrange to set up the environment it needs: often a good way to do
  that is to make use of lexical scoping since when \code{statistic} is
  sent to the worker processes its enclosing environment is also sent.
  (E.g. see the example for \code{\link{jack.after.boot}} where
  ancillary functions are nested inside the \code{statistic} function.)
  \code{parallel = "snow"} is primarily intended to be used on
  multi-core Windows machine where \code{parallel = "multicore"} is not
  available.

  For most of the \code{boot} methods the resampling is done in the
  master process, but not if \code{simple = TRUE} nor \code{sim =
  "parametric"}.  In those cases (or where \code{statistic} itself uses
  random numbers), more care is needed if the results need to be
  reproducible.  Resampling is done in the worker processes by
  \code{\link{censboot}(sim = "wierd")} and by most of the schemes in
  \code{\link{tsboot}} (the exceptions being \code{sim == "fixed"} and
  \code{sim == "geom"} with the default \code{ran.gen}).

  Where random-number generation is done in the worker processes, the
  default behaviour is that each worker chooses a separate seed,
  non-reproducibly.  However, with \code{parallel = "multicore"} or
  \code{parallel = "snow"} using the default cluster, a second approach
  is used if \code{\link{RNGkind}("L'Ecuyer-CMRG")} has been selected.
  In that approach each worker gets a different subsequence of the RNG
  stream based on the seed at the time the worker is spawned and so the
  results will be reproducible if \code{ncpus} is unchanged, and for
  \code{parallel = "multicore"} if \code{parallel::\link{mc.reset.stream}()} is
  called: see the examples for \code{\link{mclapply}}.
}
\references{
  There are many references explaining the bootstrap and its variations.
  Among them are :

Booth, J.G., Hall, P. and Wood, A.T.A. (1993) Balanced importance resampling 
for the bootstrap. \emph{Annals of Statistics}, \bold{21}, 286--298.

Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.

Davison, A.C., Hinkley, D.V. and Schechtman, E. (1986) Efficient bootstrap 
simulation. \emph{Biometrika}, \bold{73}, 555--566.

Efron, B. and Tibshirani, R. (1993) \emph{An Introduction to the Bootstrap}.
Chapman & Hall.

Gleason, J.R. (1988) Algorithms for balanced bootstrap simulations.
\emph{ American Statistician}, \bold{42}, 263--266.

Hall, P. (1989) Antithetic resampling for the bootstrap. \emph{Biometrika},
\bold{73}, 713--724.

Hinkley, D.V. (1988) Bootstrap methods (with Discussion). 
\emph{Journal of the  Royal Statistical Society, B}, \bold{50},
312--337, 355--370.

Hinkley, D.V. and Shi, S. (1989) Importance sampling and the nested bootstrap.
\emph{Biometrika}, \bold{76}, 435--446.

Johns M.V. (1988) Importance sampling for bootstrap confidence intervals.
\emph{Journal of the American Statistical Association}, \bold{83}, 709--714.

Noreen, E.W. (1989) \emph{Computer Intensive Methods for Testing Hypotheses}. 
John Wiley & Sons.
}
\seealso{
  \code{\link{boot.array}}, \code{\link{boot.ci}},
  \code{\link{censboot}}, \code{\link{empinf}},
  \code{\link{jack.after.boot}}, \code{\link{tilt.boot}},
  \code{\link{tsboot}}.
}
\examples{
# Usual bootstrap of the ratio of means using the city data
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
boot(city, ratio, R = 999, stype = "w")


# Stratified resampling for the difference of means.  In this
# example we will look at the difference of means between the final
# two series in the gravity data.
diff.means <- function(d, f)
{    n <- nrow(d)
     gp1 <- 1:table(as.numeric(d$series))[1]
     m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
     m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
     ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
     ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
     c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
grav1 <- gravity[as.numeric(gravity[,2]) >= 7,]
boot(grav1, diff.means, R = 999, stype = "f", strata = grav1[,2])

# In this example we show the use of boot in a prediction from
# regression based on the nuclear data.  This example is taken
# from Example 6.8 of Davison and Hinkley (1997).  Notice also
# that two extra arguments to 'statistic' are passed through boot.
nuke <- nuclear[, c(1, 2, 5, 7, 8, 10, 11)]
nuke.lm <- glm(log(cost) ~ date+log(cap)+ne+ct+log(cum.n)+pt, data = nuke)
nuke.diag <- glm.diag(nuke.lm)
nuke.res <- nuke.diag$res * nuke.diag$sd
nuke.res <- nuke.res - mean(nuke.res)

# We set up a new data frame with the data, the standardized 
# residuals and the fitted values for use in the bootstrap.
nuke.data <- data.frame(nuke, resid = nuke.res, fit = fitted(nuke.lm))

# Now we want a prediction of plant number 32 but at date 73.00
new.data <- data.frame(cost = 1, date = 73.00, cap = 886, ne = 0,
                       ct = 0, cum.n = 11, pt = 1)
new.fit <- predict(nuke.lm, new.data)

nuke.fun <- function(dat, inds, i.pred, fit.pred, x.pred)
{
     lm.b <- glm(fit+resid[inds] ~ date+log(cap)+ne+ct+log(cum.n)+pt,
                 data = dat)
     pred.b <- predict(lm.b, x.pred)
     c(coef(lm.b), pred.b - (fit.pred + dat$resid[i.pred]))
}

nuke.boot <- boot(nuke.data, nuke.fun, R = 999, m = 1, 
                  fit.pred = new.fit, x.pred = new.data)
# The bootstrap prediction squared error would then be found by
mean(nuke.boot$t[, 8]^2)
# Basic bootstrap prediction limits would be
new.fit - sort(nuke.boot$t[, 8])[c(975, 25)]


# Finally a parametric bootstrap.  For this example we shall look 
# at the air-conditioning data.  In this example our aim is to test 
# the hypothesis that the true value of the index is 1 (i.e. that 
# the data come from an exponential distribution) against the 
# alternative that the data come from a gamma distribution with
# index not equal to 1.
air.fun <- function(data) {
     ybar <- mean(data$hours)
     para <- c(log(ybar), mean(log(data$hours)))
     ll <- function(k) {
          if (k <= 0) 1e200 else lgamma(k)-k*(log(k)-1-para[1]+para[2])
     }
     khat <- nlm(ll, ybar^2/var(data$hours))$estimate
     c(ybar, khat)
}

air.rg <- function(data, mle) {
    # Function to generate random exponential variates.
    # mle will contain the mean of the original data
    out <- data
    out$hours <- rexp(nrow(out), 1/mle)
    out
}

air.boot <- boot(aircondit, air.fun, R = 999, sim = "parametric",
                 ran.gen = air.rg, mle = mean(aircondit$hours))

# The bootstrap p-value can then be approximated by
sum(abs(air.boot$t[,2]-1) > abs(air.boot$t0[2]-1))/(1+air.boot$R)
}
\keyword{nonparametric}
\keyword{htest}
