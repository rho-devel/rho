\name{gravity}
\alias{gravity}
\alias{grav}
\title{
Acceleration Due to Gravity
}
\description{
The \code{gravity} data frame has 81 rows and 2 columns.

The \code{grav} data set has 26 rows and 2 columns.

Between May 1934 and July 1935, the National Bureau of Standards in
Washington D.C. conducted a series of experiments to estimate the
acceleration due to gravity, \emph{g}, at Washington.  Each experiment
produced a number of replicate estimates of \emph{g} using the same
methodology.  Although the basic method remained the same for all
experiments, that of the reversible pendulum, there were changes in
configuration.

The \code{gravity} data frame contains the data from all eight
experiments.  The \code{grav} data frame contains the data from the
experiments 7 and 8.  The data are expressed as deviations from 980.000
in centimetres per second squared.
}
\usage{
gravity
}
\format{
This data frame contains the following columns:

\describe{
\item{\code{g}}{
The deviation of the estimate from 980.000 centimetres per second squared.
}
\item{\code{series}}{
A factor describing from which experiment the estimate was derived.
}}}
\source{
The data were obtained from

Cressie, N. (1982) Playing safe with misweighted means. 
\emph{Journal of the American Statistical Association}, \bold{77}, 754--759.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
