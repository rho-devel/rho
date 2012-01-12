\name{linear.approx}
\alias{linear.approx}
\title{
Linear Approximation of Bootstrap Replicates
}
\description{
This function takes a bootstrap object and for each bootstrap replicate
it calculates the linear approximation to the statistic of interest for 
that bootstrap sample.
}
\usage{
linear.approx(boot.out, L = NULL, index = 1, type = NULL,
              t0 = NULL, t = NULL, \dots)
}
\arguments{
\item{boot.out}{
An object of class \code{"boot"} representing a nonparametric bootstrap.  It will
usually be created by the function \code{boot}.
}
\item{L}{
A vector containing the empirical influence values for the statistic of
interest.  If it is not supplied then \code{L} is calculated through a call 
to \code{empinf}.
}
\item{index}{
The index of the variable of interest within the output of 
\code{boot.out$statistic}. 
}
\item{type}{
This gives the type of empirical influence values to be calculated.  It is
not used if \code{L} is supplied.  The possible types of empirical influence
values are described in the help for \code{\link{empinf}}.
}
\item{t0}{
The observed value of the statistic of interest.  The input value is used only 
if one of \code{t} or \code{L} is also supplied.  The default value is 
\code{boot.out$t0[index]}.  If \code{t0} is supplied but neither \code{t} nor \code{L} are supplied
then \code{t0} is set to \code{boot.out$t0[index]} and a warning is generated.
}
\item{t}{
A vector of bootstrap replicates of the statistic of interest.  If \code{t0} is 
missing then \code{t} is not used, otherwise it is used to calculate the empirical 
influence values (if they are not supplied in \code{L}).  
}
\item{...}{
Any extra arguments required by \code{boot.out$statistic}.  These are needed if
\code{L} is not supplied as they are used by \code{empinf} to calculate empirical 
influence values.
}}
\value{
A vector of length \code{boot.out$R} with the linear approximations to the
statistic of interest for each of the bootstrap samples.  
}
\details{
The linear approximation to a bootstrap replicate with frequency vector \code{f}
is given by \code{t0 + sum(L * f)/n} in the one sample with an easy extension
to the stratified case.  The frequencies are found by calling \code{boot.array}.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\seealso{
\code{\link{boot}}, \code{\link{empinf}}, \code{\link{control}}
}
\examples{
# Using the city data let us look at the linear approximation to the 
# ratio statistic and its logarithm. We compare these with the 
# corresponding plots for the bigcity data 

ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
city.boot <- boot(city, ratio, R = 499, stype = "w")
bigcity.boot <- boot(bigcity, ratio, R = 499, stype = "w")
op <- par(pty = "s", mfrow = c(2, 2))

# The first plot is for the city data ratio statistic.
city.lin1 <- linear.approx(city.boot)
lim <- range(c(city.boot$t,city.lin1))
plot(city.boot$t, city.lin1, xlim = lim, ylim = lim, 
     main = "Ratio; n=10", xlab = "t*", ylab = "tL*")
abline(0, 1)

# Now for the log of the ratio statistic for the city data.
city.lin2 <- linear.approx(city.boot,t0 = log(city.boot$t0), 
                           t = log(city.boot$t))
lim <- range(c(log(city.boot$t),city.lin2))
plot(log(city.boot$t), city.lin2, xlim = lim, ylim = lim, 
     main = "Log(Ratio); n=10", xlab = "t*", ylab = "tL*")
abline(0, 1)

# The ratio statistic for the bigcity data.
bigcity.lin1 <- linear.approx(bigcity.boot)
lim <- range(c(bigcity.boot$t,bigcity.lin1))
plot(bigcity.lin1, bigcity.boot$t, xlim = lim, ylim = lim,
     main = "Ratio; n=49", xlab = "t*", ylab = "tL*")
abline(0, 1)

# Finally the log of the ratio statistic for the bigcity data.
bigcity.lin2 <- linear.approx(bigcity.boot,t0 = log(bigcity.boot$t0), 
                              t = log(bigcity.boot$t))
lim <- range(c(log(bigcity.boot$t),bigcity.lin2))
plot(bigcity.lin2, log(bigcity.boot$t), xlim = lim, ylim = lim,
     main = "Log(Ratio); n=49", xlab = "t*", ylab = "tL*")
abline(0, 1)

par(op)
}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.15.
