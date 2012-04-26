\name{smooth.f}
\alias{smooth.f}
\title{
Smooth Distributions on Data Points
}
\description{
This function uses the method of frequency smoothing to find a distribution 
on a data set which has a required value, \code{theta},  of the statistic of 
interest.  The method results in distributions which vary smoothly with 
\code{theta}.
}
\usage{
smooth.f(theta, boot.out, index = 1, t = boot.out$t[, index],
         width = 0.5)
}
\arguments{
\item{theta}{
The required value for the statistic of interest.  If \code{theta} is a vector,
a separate distribution will be found for each element of \code{theta}.
}
\item{boot.out}{
A bootstrap output object returned by a call to \code{boot}.  
}
\item{index}{
The index of the variable of interest in the output of \code{boot.out$statistic}.
This argument is ignored if \code{t} is supplied.  \code{index} must be a scalar.
}
\item{t}{
The bootstrap values of the statistic of interest.  This must be a vector of
length \code{boot.out$R} and the values must be in the same order as the bootstrap
replicates in \code{boot.out}.
}
\item{width}{
The standardized width for the kernel smoothing.  The smoothing uses a
value of \code{width*s} for epsilon, where \code{s} is the bootstrap estimate of the 
standard error of the statistic of interest.  \code{width} should take a value in 
the range (0.2, 1) to produce a reasonable
smoothed distribution.  If \code{width} is too large then the distribution becomes
closer to uniform. 
}}
\value{
If \code{length(theta)} is 1 then a vector with the same length as the data set
\code{boot.out$data} is returned.  The value in position \code{i} is the probability 
to be given to the data point in position \code{i} so that the distribution has 
parameter value approximately equal to \code{theta}.
If \code{length(theta)} is bigger than 1 then the returned value is a matrix with 
\code{length(theta)} rows each of which corresponds to a distribution with the 
parameter value approximately equal to the corresponding value of \code{theta}.
}
\details{
The new distributional weights are found by applying a normal kernel smoother
to the observed values of \code{t} weighted by the observed frequencies in the
bootstrap simulation.  The resulting distribution may not have
parameter value exactly equal to the required value \code{theta} but it will 
typically have a value which is close to \code{theta}.  The details of how this
method works can be found in Davison, Hinkley and Worton (1995) and Section
3.9.2 of Davison and Hinkley (1997).
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Davison, A.C., Hinkley, D.V. and Worton, B.J. (1995) Accurate and efficient 
construction of bootstrap likelihoods. \emph{Statistics and Computing}, 
\bold{5}, 257--264.
}
\seealso{
\code{\link{boot}}, \code{\link{exp.tilt}}, \code{\link{tilt.boot}}
}
\examples{
# Example 9.8 of Davison and Hinkley (1997) requires tilting the resampling
# distribution of the studentized statistic to be centred at the observed
# value of the test statistic 1.84.  In the book exponential tilting was used
# but it is also possible to use smooth.f.
grav1 <- gravity[as.numeric(gravity[, 2]) >= 7, ]
grav.fun <- function(dat, w, orig) {
     strata <- tapply(dat[, 2], as.numeric(dat[, 2]))
     d <- dat[, 1]
     ns <- tabulate(strata)
     w <- w/tapply(w, strata, sum)[strata]
     mns <- as.vector(tapply(d * w, strata, sum)) # drop names
     mn2 <- tapply(d * d * w, strata, sum)
     s2hat <- sum((mn2 - mns^2)/ns)
     c(mns[2] - mns[1], s2hat, (mns[2]-mns[1]-orig)/sqrt(s2hat))
}
grav.z0 <- grav.fun(grav1, rep(1, 26), 0)
grav.boot <- boot(grav1, grav.fun, R = 499, stype = "w", 
                  strata = grav1[, 2], orig = grav.z0[1])
grav.sm <- smooth.f(grav.z0[3], grav.boot, index = 3)

# Now we can run another bootstrap using these weights
grav.boot2 <- boot(grav1, grav.fun, R = 499, stype = "w", 
                   strata = grav1[, 2], orig = grav.z0[1],
                   weights = grav.sm)

# Estimated p-values can be found from these as follows
mean(grav.boot$t[, 3] >= grav.z0[3])
imp.prob(grav.boot2, t0 = -grav.z0[3], t = -grav.boot2$t[, 3])


# Note that for the importance sampling probability we must 
# multiply everything by -1 to ensure that we find the correct
# probability.  Raw resampling is not reliable for probabilities
# greater than 0.5. Thus
1 - imp.prob(grav.boot2, index = 3, t0 = grav.z0[3])$raw
# can give very strange results (negative probabilities).
}
\keyword{smooth}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.15.
