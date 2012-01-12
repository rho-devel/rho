\name{tsboot}
\alias{tsboot}
\alias{ts.return}
\title{
Bootstrapping of Time Series
}
\description{
  Generate \code{R} bootstrap replicates of a statistic applied to a
  time series.  The replicate time series can be generated using fixed
  or random block lengths or can be model based replicates.
}
\usage{
tsboot(tseries, statistic, R, l = NULL, sim = "model",
       endcorr = TRUE, n.sim = NROW(tseries), orig.t = TRUE,
       ran.gen, ran.args = NULL, norm = TRUE, \dots,
       parallel = c("no", "multicore", "snow"),
       ncpus = getOption("boot.ncpus", 1L), cl = NULL)
}
\arguments{
  \item{tseries}{
    A univariate or multivariate time series.
  }
  \item{statistic}{
    A function which when applied to \code{tseries} returns a vector
    containing the statistic(s) of interest.  Each time \code{statistic} is
    called it is passed a time series of length \code{n.sim} which is of the
    same class as the original \code{tseries}.  Any other arguments which
    \code{statistic} takes must remain constant for each bootstrap replicate
    and should be supplied through the \dots{} argument to \code{tsboot}.
  }
  \item{R}{
    A positive integer giving the number of bootstrap replicates required.  
  }
  \item{sim}{
    The type of simulation required to generate the replicate time series.  The
    possible input values are \code{"model"} (model based resampling),
    \code{"fixed"} (block resampling with fixed block lengths of
    \code{l}), \code{"geom"} (block resampling with block lengths
    having a geometric distribution with mean \code{l}) or
    \code{"scramble"} (phase scrambling).
  }
  \item{l}{
    If \code{sim} is \code{"fixed"} then \code{l} is the fixed block
    length used in generating the replicate time series.  If \code{sim} is
    \code{"geom"} then \code{l} is the mean of the geometric distribution
    used to generate the block lengths. \code{l} should be a positive
    integer less than the length of \code{tseries}.  This argument is not
    required when \code{sim} is \code{"model"} but it is required for all
    other simulation types.
  }
  \item{endcorr}{
    A logical variable indicating whether end corrections are to be
    applied when \code{sim} is \code{"fixed"}.  When \code{sim} is
    \code{"geom"}, \code{endcorr} is automatically set to \code{TRUE};
    \code{endcorr} is not used when \code{sim} is \code{"model"} or
    \code{"scramble"}.
  }
  \item{n.sim}{
    The length of the simulated time series.  Typically this will be equal
    to the length of the original time series but there are situations when
    it will be larger.  One obvious situation is if prediction is required.
    Another situation in which \code{n.sim} is larger than the original
    length is if \code{tseries} is a residual time series from fitting some
    model to the original time series. In this case, \code{n.sim} would
    usually be the length of the original time series.
  }
  \item{orig.t}{
    A logical variable which indicates whether \code{statistic} should be
    applied to \code{tseries} itself as well as the bootstrap replicate
    series.  If \code{statistic} is expecting a longer time series than
    \code{tseries} or if applying \code{statistic} to \code{tseries} will
    not yield any useful information then \code{orig.t} should be set to
    \code{FALSE}.
  }
  \item{ran.gen}{
    This is a function of three arguments.  The first argument is a time
    series.  If \code{sim} is \code{"model"} then it will always be
    \code{tseries} that is passed.  For other simulation types it is the
    result of selecting \code{n.sim} observations from \code{tseries} by
    some scheme and converting the result back into a time series of the
    same form as \code{tseries} (although of length \code{n.sim}).  The
    second argument to \code{ran.gen} is always the value \code{n.sim}, and
    the third argument is \code{ran.args}, which is used to supply any other
    objects needed by \code{ran.gen}.  If \code{sim} is \code{"model"} then
    the generation of the replicate time series will be done in
    \code{ran.gen} (for example through use of \code{\link{arima.sim}}).
    For the other simulation types \code{ran.gen} is used for
    \sQuote{post-blackening}.  The default is that the function simply returns
    the time series passed to it.
  }
  \item{ran.args}{
    This will be supplied to \code{ran.gen} each time it is called.  If
    \code{ran.gen} needs any extra arguments then they should be
    supplied as components of \code{ran.args}. Multiple arguments may be
    passed by making \code{ran.args} a list.  If \code{ran.args} is
    \code{NULL} then it should not be used within \code{ran.gen} but
    note that \code{ran.gen} must still have its third argument.
  }
  \item{norm}{
    A logical argument indicating whether normal margins should be used
    for phase scrambling.  If \code{norm} is \code{FALSE} then margins
    corresponding to the exact empirical margins are used.
  }
  \item{...}{
    Extra named arguments to \code{statistic} may be supplied here.
    Beware of partial matching to the arguments of \code{tsboot} listed above.
  }
  \item{parallel, ncpus, cl}{
    See the help for \code{\link{boot}}.
  }
}
\value{
  An object of class \code{"boot"} with the following components.

  \item{t0}{
    If \code{orig.t} is \code{TRUE} then \code{t0} is the result of
    \code{statistic(tseries,\dots{})} otherwise it is \code{NULL}.
  }
  \item{t}{
    The results of applying \code{statistic} to the replicate time series. 
  }
  \item{R}{
    The value of \code{R} as supplied to \code{tsboot}.
  }
  \item{tseries}{
    The original time series.
  }
  \item{statistic}{
    The function \code{statistic} as supplied.
  }
  \item{sim}{
    The simulation type used in generating the replicates.
  }
  \item{endcorr}{
    The value of \code{endcorr} used.  The value is meaningful only when
    \code{sim} is \code{"fixed"}; it is ignored for model based simulation
    or phase scrambling and is always set to \code{TRUE} if \code{sim} is
    \code{"geom"}.
  }
  \item{n.sim}{
    The value of \code{n.sim} used.
  }
  \item{l}{
    The value of \code{l} used for block based resampling.  This will be
    \code{NULL} if block based resampling was not used.
  }
  \item{ran.gen}{
    The \code{ran.gen} function used for generating the series or for
    \sQuote{post-blackening}.
  }
  \item{ran.args}{
    The extra arguments passed to \code{ran.gen}.
  }
  \item{call}{
    The original call to \code{tsboot}.
  }
}
\details{
  If \code{sim} is \code{"fixed"} then each replicate time series is
  found by taking blocks of length \code{l}, from the original time
  series and putting them end-to-end until a new series of length
  \code{n.sim} is created.  When \code{sim} is \code{"geom"} a similar
  approach is taken except that now the block lengths are generated from
  a geometric distribution with mean \code{l}.  Post-blackening can be
  carried out on these replicate time series by including the function
  \code{ran.gen} in the call to \code{tsboot} and having \code{tseries}
  as a time series of residuals.

  Model based resampling is very similar to the parametric bootstrap and
  all simulation must be in one of the user specified functions.  This
  avoids the complicated problem of choosing the block length but relies
  on an accurate model choice being made.

  Phase scrambling is described in Section 8.2.4 of Davison and Hinkley
  (1997).  The types of statistic for which this method produces
  reasonable results is very limited and the other methods seem to do
  better in most situations.  Other types of resampling in the frequency
  domain can be accomplished using the function \code{boot} with the
  argument \code{sim = "parametric"}.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.

Kunsch, H.R. (1989) The jackknife and the bootstrap for general stationary
observations. \emph{Annals of Statistics}, \bold{17}, 1217--1241.

Politis, D.N. and Romano, J.P. (1994) The stationary bootstrap. 
\emph{Journal of the American Statistical Association}, \bold{89}, 1303--1313.
}
\seealso{
  \code{\link{boot}}, \code{\link{arima.sim}}
}
\examples{
lynx.fun <- function(tsb) {
     ar.fit <- ar(tsb, order.max = 25)
     c(ar.fit$order, mean(tsb), tsb)
}

# the stationary bootstrap with mean block length 20
lynx.1 <- tsboot(log(lynx), lynx.fun, R = 99, l = 20, sim = "geom")

# the fixed block bootstrap with length 20
lynx.2 <- tsboot(log(lynx), lynx.fun, R = 99, l = 20, sim = "fixed")

# Now for model based resampling we need the original model
# Note that for all of the bootstraps which use the residuals as their
# data, we set orig.t to FALSE since the function applied to the residual
# time series will be meaningless.
lynx.ar <- ar(log(lynx))
lynx.model <- list(order = c(lynx.ar$order, 0, 0), ar = lynx.ar$ar)
lynx.res <- lynx.ar$resid[!is.na(lynx.ar$resid)]
lynx.res <- lynx.res - mean(lynx.res)

lynx.sim <- function(res,n.sim, ran.args) {
     # random generation of replicate series using arima.sim 
     rg1 <- function(n, res) sample(res, n, replace = TRUE)
     ts.orig <- ran.args$ts
     ts.mod <- ran.args$model
     mean(ts.orig)+ts(arima.sim(model = ts.mod, n = n.sim,
                      rand.gen = rg1, res = as.vector(res)))
}

lynx.3 <- tsboot(lynx.res, lynx.fun, R = 99, sim = "model", n.sim = 114,
                 orig.t = FALSE, ran.gen = lynx.sim, 
                 ran.args = list(ts = log(lynx), model = lynx.model))

#  For "post-blackening" we need to define another function
lynx.black <- function(res, n.sim, ran.args) {
     ts.orig <- ran.args$ts
     ts.mod <- ran.args$model
     mean(ts.orig) + ts(arima.sim(model = ts.mod,n = n.sim,innov = res))
}

# Now we can run apply the two types of block resampling again but this
# time applying post-blackening.
lynx.1b <- tsboot(lynx.res, lynx.fun, R = 99, l = 20, sim = "fixed",
                  n.sim = 114, orig.t = FALSE, ran.gen = lynx.black, 
                  ran.args = list(ts = log(lynx), model = lynx.model))

lynx.2b <- tsboot(lynx.res, lynx.fun, R = 99, l = 20, sim = "geom",
                  n.sim = 114, orig.t = FALSE, ran.gen = lynx.black, 
                  ran.args = list(ts = log(lynx), model = lynx.model))

# To compare the observed order of the bootstrap replicates we
# proceed as follows.
table(lynx.1$t[, 1])
table(lynx.1b$t[, 1])
table(lynx.2$t[, 1])
table(lynx.2b$t[, 1])
table(lynx.3$t[, 1])
# Notice that the post-blackened and model-based bootstraps preserve
# the true order of the model (11) in many more cases than the others.
}
\keyword{nonparametric}
\keyword{ts}
