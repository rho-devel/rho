\name{abc.ci}
\alias{abc.ci}
\title{
Nonparametric ABC Confidence Intervals
}
\description{
Calculate equi-tailed two-sided nonparametric approximate bootstrap confidence 
intervals for a parameter, given a set of data and an estimator of the 
parameter, using numerical differentiation.  
}
\usage{
abc.ci(data, statistic, index=1, strata=rep(1, n), conf=0.95, 
       eps=0.001/n, \dots)
}
\arguments{
\item{data}{
A data set expressed as a vector, matrix or data frame.
}
\item{statistic}{
A function which returns the statistic of interest.  The function must
take at least 2 arguments; the first argument should be the data and the
second a vector of weights.  The weights passed to \code{statistic} will be 
normalized to sum to 1 within each stratum.  Any other arguments should be 
passed to \code{abc.ci} as part of the \code{\dots{}} argument.  
}
\item{index}{
If \code{statistic} returns a vector of length greater than 1, then this indicates
the position of the variable of interest within that vector.
}
\item{strata}{
A factor or numerical vector indicating to which sample each
observation belongs in multiple sample problems.  The default 
is the one-sample case.  
}
\item{conf}{
A scalar or vector containing the confidence level(s) of the required
interval(s).
}
\item{eps}{
The value of epsilon to be used for the numerical differentiation.
}
\item{...}{
Any other arguments for \code{statistic}.  These will be passed unchanged to
\code{statistic} each time it is called within \code{abc.ci}.
}}
\value{
A \code{length(conf)} by 3 matrix where each row contains the confidence level 
followed by the lower and upper end-points of the ABC interval at that
level.
}
\details{
This function is based on the function \code{abcnon} written by R. Tibshirani.
A listing of the original function is available in DiCiccio and Efron (1996).
The function uses numerical differentiation for the first and second
derivatives of the statistic and then uses these values to approximate
the bootstrap BCa intervals.  The total number of evaluations of the
statistic is \code{2*n+2+2*length(conf)} where \code{n} is the number of data points
(plus calculation of the original value of the statistic).  The function 
works for the multiple sample case
without the need to rewrite the statistic in an artificial form since
the stratified normalization is done internally by the function.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}, Chapter 5. 
Cambridge University Press. 


DiCiccio, T. J. and Efron B. (1992) More accurate confidence intervals in 
exponential families. \emph{Biometrika}, \bold{79}, 231--245.


DiCiccio, T. J.  and Efron  B. (1996) Bootstrap confidence intervals (with
Discussion). 
\emph{Statistical Science}, \bold{11}, 189--228.
}
\seealso{
\code{\link{boot.ci}}
}
\examples{
# 90\% and 95\% confidence intervals for the correlation 
# coefficient between the columns of the bigcity data

abc.ci(bigcity, corr, conf=c(0.90,0.95))

# A 95\% confidence interval for the difference between the means of
# the last two samples in gravity
mean.diff <- function(y, w)
{    gp1 <- 1:table(as.numeric(y$series))[1]
     sum(y[gp1, 1] * w[gp1]) - sum(y[-gp1, 1] * w[-gp1])
}
grav1 <- gravity[as.numeric(gravity[, 2]) >= 7, ]
abc.ci(grav1, mean.diff, strata = grav1$series)
}
\keyword{nonparametric}
\keyword{htest}

