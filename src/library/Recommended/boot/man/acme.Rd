\name{acme}
\alias{acme}
\title{
Monthly Excess Returns
}
\description{
The \code{acme} data frame has 60 rows and 3 columns.


The excess return for the Acme Cleveland Corporation are recorded along with
those for all stocks listed on the New York and American Stock Exchanges were
recorded over a five year period.  These excess returns are relative to the
return on a risk-less investment such a U.S. Treasury bills.  
}
\usage{
acme
}
\format{
This data frame contains the following columns:


\describe{
\item{\code{month}}{
A character string representing the month of the observation.
}
\item{\code{market}}{
The excess return of the market as a whole.
}
\item{\code{acme}}{
The excess return for the Acme Cleveland Corporation.
}}}
\source{
The data were obtained from


Simonoff, J.S. and Tsai, C.-L. (1994) Use of modified profile likelihood for 
improved tests of constancy of variance in regression. 
\emph{Applied Statistics}, \bold{43}, 353--370.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
