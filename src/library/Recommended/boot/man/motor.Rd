\name{motor}
\alias{motor}
\title{
Data from a Simulated Motorcycle Accident
}
\description{
The \code{motor} data frame has 94 rows and 4 columns.  The rows are
obtained by removing replicate values of \code{time} from the dataset 
\code{\link{mcycle}}.  Two extra columns are added to allow for strata with
a different residual variance in each stratum.
}
\usage{
motor
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{times}}{
The time in milliseconds since impact.
}
\item{\code{accel}}{
The recorded head acceleration (in g).
}
\item{\code{strata}}{
A numeric column indicating to which of the three strata (numbered 1, 2 and 3)
the observations belong.
}
\item{\code{v}}{
An estimate of the residual variance for the observation.  \code{v} is constant
within the strata but a different
estimate is used for each of the three strata.
}}}
\source{
The data were obtained from


Silverman, B.W. (1985) Some aspects of the spline smoothing approach to
non-parametric curve fitting. 
\emph{Journal of the Royal Statistical Society, B}, \bold{47}, 1--52.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Venables, W.N. and Ripley, B.D. (1994) 
\emph{Modern Applied Statistics with S-Plus}. Springer-Verlag.
}
\seealso{
\code{\link{mcycle}}
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
