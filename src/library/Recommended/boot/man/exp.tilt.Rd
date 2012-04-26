\name{exp.tilt}
\alias{exp.tilt}
\title{
Exponential Tilting
}
\description{
This function calculates exponentially tilted multinomial distributions 
such that the resampling distributions of the linear approximation to a
statistic have the required means.
}
\usage{
exp.tilt(L, theta = NULL, t0 = 0, lambda = NULL,
         strata = rep(1, length(L)))
}
\arguments{
\item{L}{
The empirical influence values for the statistic of interest based on the 
observed data.  The length of \code{L} should be the same as the size of the 
original data set.  Typically \code{L} will be calculated by a call to \code{empinf}.
}
\item{theta}{
The value at which the tilted distribution is to be centred.  This is not
required if \code{lambda} is supplied but is needed otherwise.
}
\item{t0}{
The current value of the statistic.  The default is that the statistic equals 0.
}
\item{lambda}{
The Lagrange multiplier(s).  For each value of \code{lambda} a multinomial 
distribution
is found with probabilities proportional to \code{exp(lambda * L)}.  In general
\code{lambda} is not known and so \code{theta} would be supplied, and the corresponding
value of \code{lambda} found.  If both \code{lambda} and \code{theta} are supplied then
\code{lambda} is ignored and the multipliers for tilting to \code{theta} are found.
}
\item{strata}{
A vector or factor of the same length as \code{L} giving the strata for the
observed data and the empirical influence values \code{L}.
}}
\value{
A list with the following components :



\item{p}{
The tilted probabilities.  There will be \code{m} distributions where \code{m} is the
length of \code{theta} (or \code{lambda} if supplied).  If \code{m} is 1 then \code{p} is a vector
of \code{length(L)} probabilities.  If \code{m} is greater than 1 then \code{p} is a matrix
with \code{m} rows, each of which contain \code{length(L)} probabilities.  In this case
the vector \code{p[i,]} is the distribution tilted to \code{theta[i]}.  \code{p} is
in the form required by the argument \code{weights} of the function \code{boot} for
importance resampling.
}
\item{lambda}{
The Lagrange multiplier used in the equation to determine the tilted
probabilities.  \code{lambda} is a vector of the same length as \code{theta}.
}
\item{theta}{
The values of \code{theta} to which the distributions have been tilted.  In general
this will be the input value of \code{theta} but if \code{lambda} was supplied then 
this is the vector of the corresponding \code{theta} values.
}}
\details{
Exponential tilting involves finding a set of weights for a data set to
ensure that the bootstrap distribution of the linear approximation to a 
statistic of interest has mean \code{theta}.  The weights chosen to achieve this
are given by \code{p[j]} proportional to 
\code{exp(lambda*L[j]/n)}, where \code{n} is the number of data points.  
\code{lambda} is then 
chosen to make the mean of the bootstrap
distribution, of the linear approximation to the statistic of interest, equal
to the required value \code{theta}.  Thus \code{lambda} is defined as the 
solution of a nonlinear equation.   
The equation is solved by minimizing the Euclidean distance between 
the left and right hand sides of the equation using the function \code{nlmin}.
If this minimum is not equal to zero then the method fails.


Typically exponential tilting is used to find suitable weights for importance
resampling.  If a small tail probability or quantile of the distribution of
the statistic of interest is required then a more efficient simulation is to
centre the resampling distribution close to the point of interest and
then use the functions \code{imp.prob} or \code{imp.quantile} to estimate the required
quantity.


Another method of achieving a similar shifting of the distribution is through
the use of \code{smooth.f}.  The function \code{tilt.boot} uses \code{exp.tilt} or \code{smooth.f}
to find the weights for a tilted bootstrap.
}
\references{
Davison, A. C. and Hinkley, D. V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Efron, B. (1981) Nonparametric standard errors and confidence intervals 
(with Discussion). \emph{Canadian Journal of Statistics}, \bold{9}, 139--172.
}
\seealso{
\code{\link{empinf}}, \code{\link{imp.prob}}, \code{\link{imp.quantile}}, \code{\link{optim}}, \code{\link{smooth.f}}, \code{\link{tilt.boot}}
}
\examples{
# Example 9.8 of Davison and Hinkley (1997) requires tilting the resampling
# distribution of the studentized statistic to be centred at the observed
# value of the test statistic 1.84.  This can be achieved as follows.
grav1 <- gravity[as.numeric(gravity[,2]) >=7 , ]
grav.fun <- function(dat, w, orig) {
     strata <- tapply(dat[, 2], as.numeric(dat[, 2]))
     d <- dat[, 1]
     ns <- tabulate(strata)
     w <- w/tapply(w, strata, sum)[strata]
     mns <- as.vector(tapply(d * w, strata, sum)) # drop names
     mn2 <- tapply(d * d * w, strata, sum)
     s2hat <- sum((mn2 - mns^2)/ns)
     c(mns[2]-mns[1], s2hat, (mns[2]-mns[1]-orig)/sqrt(s2hat))
}
grav.z0 <- grav.fun(grav1, rep(1, 26), 0)
grav.L <- empinf(data = grav1, statistic = grav.fun, stype = "w", 
                 strata = grav1[,2], index = 3, orig = grav.z0[1])
grav.tilt <- exp.tilt(grav.L, grav.z0[3], strata = grav1[,2])
boot(grav1, grav.fun, R = 499, stype = "w", weights = grav.tilt$p,
     strata = grav1[,2], orig = grav.z0[1])
}
\keyword{nonparametric}
\keyword{smooth}
% Converted by Sd2Rd version 1.15.
