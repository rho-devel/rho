\name{envelope}
\alias{envelope}
\title{
Confidence Envelopes for Curves
}
\description{
This function calculates overall and pointwise confidence envelopes for a 
curve based on bootstrap replicates of the curve evaluated at a number of
fixed points.
}
\usage{
envelope(boot.out = NULL, mat = NULL, level = 0.95, index = 1:ncol(mat))
}
\arguments{
\item{boot.out}{
An object of class \code{"boot"} for which \code{boot.out$t} contains the replicates
of the curve at a number of fixed points.
}
\item{mat}{
A matrix of bootstrap replicates of the values of the curve at a number of
fixed points.  This is a required argument if \code{boot.out} is not supplied
and is set to \code{boot.out$t} otherwise.
}
\item{level}{
The confidence level of the envelopes required.  The default is to
find 95\% confidence envelopes.  It can be a scalar or a vector of length 2.  
If it is scalar then both the pointwise and the overall
envelopes are found at that level.  If is a vector then the first element gives 
the level for the pointwise envelope and the second gives the level for the
overall envelope.
}
\item{index}{
The numbers of the columns of \code{mat} which contain the bootstrap replicates.
This can be used to ensure that other statistics which may have been calculated
in the bootstrap are not considered as values of the function.
}}
\value{
A list with the following components :



\item{point}{
A matrix with two rows corresponding to the values of the upper and lower
pointwise confidence envelope at the same points as the bootstrap replicates
were calculated.
}
\item{overall}{
A matrix similar to \code{point} but containing the envelope which controls the
overall error.
}
\item{k.pt}{
The quantiles used for the pointwise envelope.
}
\item{err.pt}{
A vector with two components, the first gives the pointwise error rate for the
pointwise envelope, and the second the overall error rate for that envelope.
}
\item{k.ov}{
The quantiles used for the overall envelope.
}
\item{err.ov}{
A vector with two components, the first gives the pointwise error rate for the
overall envelope, and the second the overall error rate for that envelope.
}
\item{err.nom}{
A vector of length 2 giving the nominal error rates for the pointwise and the
overall envelopes.
}}
\details{
The pointwise envelope is found by simply looking at the quantiles of the
replicates at each point.  The overall error for that envelope is then
calculated using equation (4.17) of Davison and Hinkley (1997).  A sequence
of pointwise envelopes is then found until one of them has overall error
approximately equal to the level required.  If no such envelope can be
found then the envelope returned will just contain the extreme values of each
column of \code{mat}.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\seealso{
\code{\link{boot}}, \code{\link{boot.ci}}
}
\examples{
# Testing whether the final series of measurements of the gravity data
# may come from a normal distribution.  This is done in Examples 4.7 
# and 4.8 of Davison and Hinkley (1997).
grav1 <- gravity$g[gravity$series == 8]
grav.z <- (grav1 - mean(grav1))/sqrt(var(grav1))
grav.gen <- function(dat, mle) rnorm(length(dat))
grav.qqboot <- boot(grav.z, sort, R = 999, sim = "parametric",
                    ran.gen = grav.gen)
grav.qq <- qqnorm(grav.z, plot.it = FALSE)
grav.qq <- lapply(grav.qq, sort)
plot(grav.qq, ylim = c(-3.5, 3.5), ylab = "Studentized Order Statistics",
     xlab = "Normal Quantiles")
grav.env <- envelope(grav.qqboot, level = 0.9)
lines(grav.qq$x, grav.env$point[1, ], lty = 4)
lines(grav.qq$x, grav.env$point[2, ], lty = 4)
lines(grav.qq$x, grav.env$overall[1, ], lty = 1)
lines(grav.qq$x, grav.env$overall[2, ], lty = 1)
}
\keyword{dplot}
\keyword{htest}
% Converted by Sd2Rd version 1.15.
