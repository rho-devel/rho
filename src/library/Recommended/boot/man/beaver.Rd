\name{beaver}
\alias{beaver}
\title{
Beaver Body Temperature Data
}
\description{
The \code{beaver} data frame has 100 rows and 4 columns.  It is a multivariate
time series of class \code{"ts"} and also inherits from class \code{"data.frame"}.

This data set is part of a long study into body temperature regulation in
beavers.  Four adult female beavers were live-trapped and had a 
temperature-sensitive radio transmitter surgically implanted.  Readings were
taken every 10 minutes.  The location of the beaver was also recorded and
her activity level was dichotomized by whether she was in the retreat or 
outside of it since high-intensity activities only occur outside of the 
retreat.

The data in this data frame are those readings for one of the beavers on a day
in autumn.  
}
\usage{
beaver
}
\format{
  This data frame contains the following columns:
  \describe{
\item{\code{day}}{
The day number.  The data includes only data from day 307 and early 308.
}
\item{\code{time}}{
The time of day formatted as hour-minute.
}
\item{\code{temp}}{
The body temperature in degrees Celsius.
}
\item{\code{activ}}{
The dichotomized activity indicator. \code{1} indicates that the beaver is outside
of the retreat and therefore engaged in high-intensity activity.
}}}
\source{
The data were obtained from

Reynolds, P.S. (1994) Time-series analyses of beaver body temperatures.
In \emph{Case Studies in Biometry}. N. Lange, L. Ryan, L. Billard, 
D. Brillinger, L. Conquest and J. Greenhouse (editors), 211--228. John Wiley.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
