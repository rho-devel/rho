\name{EEF.profile}
\alias{EEF.profile}
\alias{EL.profile}
\title{ Empirical Likelihoods}
\description{
 Construct the empirical log likelihood or empirical exponential 
family log likelihood for a mean.}
\usage{
EEF.profile(y, tmin = min(y) + 0.1, tmax = max(y) - 0.1, n.t = 25, 
            u = function(y, t) y - t)
EL.profile(y, tmin  =  min(y) + 0.1, tmax = max(y) - 0.1, n.t  =  25, 
           u = function(y, t) y - t)
}
\arguments{
  \item{y}{A vector or matrix of data}
  \item{tmin}{ The minimum value of the range over which the 
    likelihood should be computed.  This must be larger than 
    \code{min(y)}.}
  \item{tmax}{ The maximum value of the range over which the 
    likelihood should be computed.  This must be smaller than 
    \code{max(y)}.}
  \item{n.t}{ The number of points between \code{tmin} and
    \code{tmax} at which the value of the log-likelihood should be
    computed.}
  \item{u}{A function of the data and the parameter.}
}
\details{
These functions calculate the log likelihood for a mean using either
an empirical likelihood or an empirical exponential family likelihood.
They are supplied as part of the package \code{boot} for demonstration
purposes with the practicals in chapter 10 of Davison and Hinkley (1997).
The functions are not intended for general use and are not supported
as part of the \code{boot}package.  For more general and more robust
code to calculate empirical likelihoods see Professor A. B. Owen's 
empirical likelihood home page at the URL
\url{http://www-stat.stanford.edu/~owen/empirical/}.}
\value{
A matrix with \code{n.t} rows.  The first column contains the
values of the parameter used.  The second column of the output
of \code{EL.profile} contains the values of the empirical
log likelihood. The second and third columns of the output of
\code{EEF.profile} contain two versions of the empirical
exponential family log-likelihood.  The final column of the
output matrix contains the values of the Lagrange multiplier
used in the optimization procedure.
}
\references{ 
Davison, A. C. and Hinkley, D. V. (1997)
\emph{Bootstrap Methods and Their Application}. Cambridge University
Press.
}
\author{Angelo J. Canty}
\keyword{htest}
