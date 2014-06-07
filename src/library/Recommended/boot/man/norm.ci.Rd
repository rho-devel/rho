\name{norm.ci}
\alias{norm.ci}
\title{
Normal Approximation Confidence Intervals
}
\description{
Using the normal approximation to a statistic, calculate equi-tailed two-sided 
confidence intervals.
}
\usage{
norm.ci(boot.out = NULL, conf = 0.95, index = 1, var.t0 = NULL, 
        t0 = NULL, t = NULL, L = NULL, h = function(t) t, 
        hdot = function(t) 1, hinv = function(t) t)
}
\arguments{
\item{boot.out}{
  A bootstrap output object returned from a call to \code{boot}.  If
  \code{t0} is missing then \code{boot.out} is a required argument.  It is
  also required if both \code{var.t0} and \code{t} are missing.
}
\item{conf}{
  A scalar or vector containing the confidence level(s) of the required 
  interval(s).
}
\item{index}{
  The index of the statistic of interest within the output of a call to
  \code{boot.out$statistic}.  It is not used if \code{boot.out} is
  missing, in which case \code{t0} must be supplied.
}
\item{var.t0}{
  The variance of the statistic of interest.  If it is not supplied then 
  \code{var(t)} is used.
}
\item{t0}{
  The observed value of the statistic of interest.  If it is missing then it is
  taken from \code{boot.out} which is required in that case.
}
\item{t}{
  Bootstrap replicates of the variable of interest.  These are used to estimate 
  the variance of the statistic of interest if \code{var.t0} is not
  supplied.  The default value is \code{boot.out$t[,index]}.
}
\item{L}{
  The empirical influence values for the statistic of interest.  These are
  used to calculate \code{var.t0} if neither \code{var.t0} nor
  \code{boot.out} are supplied.  If a transformation is supplied through
  \code{h} then the influence values must be for the untransformed
  statistic \code{t0}.
}
\item{h}{
  A function defining a monotonic transformation,  the intervals are
  calculated on the scale of \code{h(t)} and the inverse function
  \code{hinv} is applied to the resulting intervals.  \code{h} must be a
  function of one variable only and must be vectorized. The default is
  the identity function.
}
\item{hdot}{
  A function of one argument returning the derivative of \code{h}.  It
  is a required argument if \code{h} is supplied and is used for
  approximating the variance of \code{h(t0)}.  The default is the
  constant function 1.
}
\item{hinv}{
  A function, like \code{h}, which returns the inverse of \code{h}.  It is
  used to transform the intervals calculated on the scale of \code{h(t)}
  back to the original scale. The default is the identity function.  If
  \code{h} is supplied but \code{hinv} is not, then the intervals returned
  will be on the transformed scale. 
}
}
\value{
  If \code{length(conf)} is 1 then a vector containing the confidence
  level and the endpoints of the interval is returned.  Otherwise, the
  returned value is a matrix where each row corresponds to a different
  confidence level.
}
\details{
  It is assumed that the statistic of interest has an approximately
  normal distribution with variance \code{var.t0} and so a confidence
  interval of length \code{2*qnorm((1+conf)/2)*sqrt(var.t0)} is found.
  If \code{boot.out} or \code{t} are supplied then the interval is
  bias-corrected using the bootstrap bias estimate, and so the interval
  would be centred at \code{2*t0-mean(t)}.  Otherwise the interval is
  centred at \code{t0}.
}
\note{
  This function is primarily designed to be called by \code{boot.ci} to
  calculate the normal approximation after a bootstrap but it can also be
  used without doing any bootstrap calculations as long as \code{t0} and
  \code{var.t0} can be supplied.  See the examples below.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\seealso{
\code{\link{boot.ci}}
}
\examples{
#  In Example 5.1 of Davison and Hinkley (1997), normal approximation 
#  confidence intervals are found for the air-conditioning data.
air.mean <- mean(aircondit$hours)
air.n <- nrow(aircondit)
air.v <- air.mean^2/air.n
norm.ci(t0 = air.mean, var.t0 = air.v)
exp(norm.ci(t0 = log(air.mean), var.t0 = 1/air.n)[2:3])

# Now a more complicated example - the ratio estimate for the city data.
ratio <- function(d, w)
     sum(d$x * w)/sum(d$u *w)
city.v <- var.linear(empinf(data = city, statistic = ratio))
norm.ci(t0 = ratio(city,rep(0.1,10)), var.t0 = city.v)
}
\keyword{htest}
% Converted by Sd2Rd version 1.15.
