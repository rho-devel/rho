\name{empinf}
\alias{empinf}
\title{
  Empirical Influence Values
}
\description{
  This function calculates the empirical influence values for a
  statistic applied to a data set.  It allows four types of calculation,
  namely the infinitesimal jackknife (using numerical differentiation),
  the usual jackknife estimates, the \sQuote{positive} jackknife
  estimates and a method which estimates the empirical influence values
  using regression of bootstrap replicates of the statistic.  All
  methods can be used with one or more samples.
}
\usage{
empinf(boot.out = NULL, data = NULL, statistic = NULL,
       type = NULL, stype = NULL ,index = 1, t = NULL,
       strata = rep(1, n), eps = 0.001, ...)
}
\arguments{
  \item{boot.out}{
    A bootstrap object created by the function \code{boot}.  If
    \code{type} is \code{"reg"} then this argument is required.  For any
    of the other types it is an optional argument.  If it is included
    when optional then the values of \code{data}, \code{statistic},
    \code{stype}, and \code{strata} are taken from the components of
    \code{boot.out} and any values passed to \code{empinf} directly are
    ignored.
  }
  \item{data}{
    A vector, matrix or data frame containing the data for which
    empirical influence values are required.  It is a required argument
    if \code{boot.out} is not supplied.  If \code{boot.out} is supplied
    then \code{data} is set to \code{boot.out$data} and any value
    supplied is ignored.
  }
  \item{statistic}{
    The statistic for which empirical influence values are required.  It
    must be a function of at least two arguments, the data set and a
    vector of weights, frequencies or indices.  The nature of the second
    argument is given by the value of \code{stype}.  Any other arguments
    that it takes must be supplied to \code{empinf} and will be passed
    to \code{statistic} unchanged. This is a required argument if
    \code{boot.out} is not supplied, otherwise its value is taken from
    \code{boot.out} and any value supplied here will be ignored.
  }
  \item{type}{
    The calculation type to be used for the empirical influence
    values. Possible values of \code{type} are \code{"inf"}
    (infinitesimal jackknife), \code{"jack"} (usual jackknife),
    \code{"pos"} (positive jackknife), and \code{"reg"} (regression
    estimation).  The default value depends on the other arguments.  If
    \code{t} is supplied then the default value of \code{type} is
    \code{"reg"} and \code{boot.out} should be present so that its
    frequency array can be found.  It \code{t} is not supplied then if
    \code{stype} is \code{"w"}, the default value of \code{type} is
    \code{"inf"}; otherwise, if \code{boot.out} is present the default
    is \code{"reg"}.  If none of these conditions apply then the default
    is \code{"jack"}.  Note that it is an error for \code{type} to be
    \code{"reg"} if \code{boot.out} is missing or to be  \code{"inf"} if
    \code{stype} is not \code{"w"}.
  }
  \item{stype}{
    A character variable giving the nature of the second argument to
    \code{statistic}. It can take on three values: \code{"w"} (weights),
    \code{"f"} (frequencies), or \code{"i"} (indices).  If
    \code{boot.out} is supplied the value of \code{stype} is set to
    \code{boot.out$stype} and any value supplied here is ignored.
    Otherwise it is an optional argument which defaults to \code{"w"}.
    If \code{type} is \code{"inf"} then \code{stype} MUST be
    \code{"w"}.
  }
  \item{index}{
    An integer giving the position of the variable of interest in the
    output of \code{statistic}.
  }
  \item{t}{
    A vector of length \code{boot.out$R} which gives the bootstrap
    replicates of the statistic of interest.  \code{t} is used only when
    \code{type} is \code{reg} and it defaults to
    \code{boot.out$t[,index]}.
  }
  \item{strata}{
    An integer vector or a factor specifying the strata for multi-sample
    problems. If \code{boot.out} is supplied  the value of \code{strata}
    is set to \code{boot.out$strata}. Otherwise it is an optional
    argument which has default corresponding to the single sample
    situation.
  }
  \item{eps}{
    This argument is used only if \code{type} is \code{"inf"}.  In that
    case the value of epsilon to be used for numerical differentiation
    will be \code{eps} divided by the number of observations in
    \code{data}.
  }
  \item{\dots}{
    Any other arguments that \code{statistic} takes.  They will be
    passed unchanged to \code{statistic} every time that it is called.
  }
}
\section{Warning}{
  All arguments to \code{empinf} must be passed using the \code{name =
    value} convention.  If this is not followed then unpredictable
  errors can occur.
}
\value{
  A vector of the empirical influence values of \code{statistic} applied
  to \code{data}.  The values will be in the same order as the
  observations in data.
}
\details{
  If \code{type} is \code{"inf"} then numerical differentiation is used
  to approximate the empirical influence values.  This makes sense only
  for statistics which are written in weighted form (i.e. \code{stype}
  is \code{"w"}).  If \code{type} is \code{"jack"} then the usual
  leave-one-out jackknife estimates of the empirical influence are
  returned.  If \code{type} is \code{"pos"} then the positive
  (include-one-twice) jackknife values are used.  If \code{type} is
  \code{"reg"} then a bootstrap object must be supplied. The regression
  method then works by regressing the bootstrap replicates of
  \code{statistic} on the frequency array from which they were derived.
  The bootstrap frequency array is obtained through a call to
  \code{boot.array}.  Further details of the methods are given in
  Section 2.7 of Davison and Hinkley (1997).

  Empirical influence values are often used frequently in nonparametric
  bootstrap applications.  For this reason many other functions call
  \code{empinf} when they are required.  Some examples of their use are
  for nonparametric delta estimates of variance, BCa intervals and
  finding linear approximations to statistics for use as control
  variates.  They are also used for antithetic bootstrap resampling.
}
\references{
  Davison, A.C. and Hinkley, D.V. (1997)
  \emph{Bootstrap Methods and Their Application}. Cambridge University Press.

  Efron, B. (1982) \emph{The Jackknife, the Bootstrap and Other
    Resampling Plans}. CBMS-NSF Regional Conference Series in Applied
  Mathematics, \bold{38}, SIAM.

  Fernholtz, L.T. (1983) \emph{von Mises Calculus for Statistical Functionals}.
  Lecture Notes in Statistics, \bold{19}, Springer-Verlag.
}
\seealso{
  \code{\link{boot}}, \code{\link{boot.array}}, \code{\link{boot.ci}},
  \code{\link{control}}, \code{\link{jack.after.boot}},
  \code{\link{linear.approx}}, \code{\link{var.linear}}
}
\examples{
# The empirical influence values for the ratio of means in
# the city data.
ratio <- function(d, w) sum(d$x *w)/sum(d$u*w)
empinf(data = city, statistic = ratio)
city.boot <- boot(city, ratio, 499, stype="w")
empinf(boot.out = city.boot, type = "reg")

# A statistic that may be of interest in the difference of means
# problem is the t-statistic for testing equality of means.  In
# the bootstrap we get replicates of the difference of means and
# the variance of that statistic and then want to use this output
# to get the empirical influence values of the t-statistic.
grav1 <- gravity[as.numeric(gravity[,2]) >= 7,]
grav.fun <- function(dat, w) {
     strata <- tapply(dat[, 2], as.numeric(dat[, 2]))
     d <- dat[, 1]
     ns <- tabulate(strata)
     w <- w/tapply(w, strata, sum)[strata]
     mns <- as.vector(tapply(d * w, strata, sum)) # drop names
     mn2 <- tapply(d * d * w, strata, sum)
     s2hat <- sum((mn2 - mns^2)/ns)
     c(mns[2] - mns[1], s2hat)
}

grav.boot <- boot(grav1, grav.fun, R = 499, stype = "w",
                  strata = grav1[, 2])

# Since the statistic of interest is a function of the bootstrap
# statistics, we must calculate the bootstrap replicates and pass
# them to empinf using the t argument.
grav.z <- (grav.boot$t[,1]-grav.boot$t0[1])/sqrt(grav.boot$t[,2])
empinf(boot.out = grav.boot, t = grav.z)
}
\keyword{nonparametric}
\keyword{math}
