\name{jack.after.boot}
\alias{jack.after.boot}
\title{
Jackknife-after-Bootstrap Plots
}
\description{
This function calculates the jackknife influence values from a bootstrap
output object and plots the corresponding jackknife-after-bootstrap plot.
}
\usage{
jack.after.boot(boot.out, index = 1, t = NULL, L = NULL,
                useJ = TRUE, stinf = TRUE, alpha = NULL,
                main = "", ylab = NULL, \dots)
}
\arguments{
\item{boot.out}{
An object of class \code{"boot"} which would normally be created by a call
to \code{\link{boot}}. It should represent a nonparametric bootstrap.
For reliable results \code{boot.out$R} should be reasonably large.
}
\item{index}{
The index of the statistic of interest in the output of \code{boot.out$statistic}.
}
\item{t}{
A vector of length \code{boot.out$R} giving the bootstrap replicates of the statistic
of interest.  This is useful if the statistic of interest is a function of 
the calculated bootstrap output.  If it is not supplied then the default is
\code{boot.out$t[,index]}.
}
\item{L}{
The empirical influence values for the statistic of interest.  These are used
only if \code{useJ} is \code{FALSE}.  If they are not supplied and are needed, they are
calculated by a call to \code{empinf}.  If \code{L} is supplied then it is assumed that
they are the infinitesimal jackknife values.
}
\item{useJ}{
A logical variable indicating if the jackknife influence values calculated from
the bootstrap replicates should be used.  If \code{FALSE} the empirical influence
values are used.  The default is \code{TRUE}.
}
\item{stinf}{
A logical variable indicating whether to standardize the jackknife values
before plotting them.  If \code{TRUE} then the jackknife values used are divided by 
their standard error.
}
\item{alpha}{
The quantiles at which the plots are required.  The default is 
\code{c(0.05, 0.1, 0.16, 0.5, 0.84, 0.9, 0.95)}.  
}
\item{main}{
A character string giving the main title for the plot.
}
\item{ylab}{
The label for the Y axis.  If the default values of \code{alpha} are used and \code{ylab}
is not supplied then a label indicating which percentiles are plotted is used.
If \code{alpha} is supplied then the default label will not say which percentiles
were used.
}
\item{...}{
Any extra arguments required by \code{boot.out$statistic}.  These are required only
if \code{useJ} is \code{FALSE} and \code{L} is not supplied, in which case they are passed to
\code{empinf} for use in calculation of the empirical influence values.
}}
\value{
There is no returned value but a plot is generated on the current graphics
display.
}
\section{Side Effects}{
A plot is created on the current graphics device.
}
\details{
The centred jackknife quantiles for each observation are estimated from those 
bootstrap samples in which  the particular observation did not appear.  These 
are then plotted against the influence values.  If \code{useJ} is \code{TRUE} then the
influence values are found in the same way as the difference between the 
mean of the statistic in the samples excluding the observations and the mean in 
all samples.  If \code{useJ} is \code{FALSE} then empirical influence values are 
calculated by calling \code{empinf}.


The resulting plots are useful diagnostic tools for looking at the way
individual observations affect the bootstrap output.


The plot will consist of a number of horizontal dotted lines which correspond
to the quantiles of the centred bootstrap distribution.  For each data point
the quantiles of the bootstrap distribution calculated by omitting that point
are plotted against the (possibly standardized) jackknife values.  The 
observation number is printed below the plots.  To make it easier to see 
the effect of omitting points on quantiles, the plotted quantiles are joined
by line segments.  These plots provide a useful diagnostic tool in
establishing the effect of individual observations on the bootstrap 
distribution.  See the references below for some guidelines on the 
interpretation of the plots.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Efron, B. (1992) Jackknife-after-bootstrap standard errors and influence 
functions (with Discussion). 
\emph{Journal of the Royal Statistical Society, B}, \bold{54}, 83--127.
}
\seealso{
\code{\link{boot}}, \code{\link{empinf}}
}
\examples{
#  To draw the jackknife-after-bootstrap plot for the head size data as in
#  Example 3.24 of Davison and Hinkley (1997)
frets.fun <- function(data, i) {
    pcorr <- function(x) { 
    #  Function to find the correlations and partial correlations between
    #  the four measurements.
         v <- cor(x)
         v.d <- diag(var(x))
         iv <- solve(v)
         iv.d <- sqrt(diag(iv))
         iv <- - diag(1/iv.d) \%*\% iv \%*\% diag(1/iv.d)
         q <- NULL
         n <- nrow(v)
         for (i in 1:(n-1)) 
              q <- rbind( q, c(v[i, 1:i], iv[i,(i+1):n]) )
         q <- rbind( q, v[n, ] )
         diag(q) <- round(diag(q))
         q
    }
    d <- data[i, ]
    v <- pcorr(d)
    c(v[1,], v[2,], v[3,], v[4,])
}
frets.boot <- boot(log(as.matrix(frets)), frets.fun, R = 999)
#  we will concentrate on the partial correlation between head breadth
#  for the first son and head length for the second.  This is the 7th
#  element in the output of frets.fun so we set index = 7
jack.after.boot(frets.boot, useJ = FALSE, stinf = FALSE, index = 7)
}
\keyword{hplot}
\keyword{nonparametric}
