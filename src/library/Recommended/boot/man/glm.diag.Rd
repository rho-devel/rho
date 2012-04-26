\name{glm.diag}
\alias{glm.diag}
\title{
Generalized Linear Model Diagnostics
}
\description{
Calculates jackknife deviance residuals, standardized deviance residuals, 
standardized Pearson residuals, approximate Cook statistic, leverage and
estimated dispersion.
}
\usage{
glm.diag(glmfit)
}
\arguments{
\item{glmfit}{
\code{glmfit} is a \code{glm.object} - the result of a call to \code{glm()}
}}
\value{
Returns a list with the following components

\item{res}{
The vector of jackknife deviance residuals.
}
\item{rd}{
The vector of standardized deviance residuals.
}
\item{rp}{
The vector of standardized Pearson residuals.
}
\item{cook}{
The vector of approximate Cook statistics.
}
\item{h}{
The vector of leverages of the observations.
}
\item{sd}{
The value used to standardize the residuals.  This is the estimate of
residual standard deviation in the Gaussian family and is the square root of
the estimated shape parameter in the Gamma family.  In all other cases it is 1.
}}
\references{
Davison, A.C. and Snell, E.J.  (1991)  Residuals and diagnostics.  
In \emph{Statistical Theory and Modelling: In Honour of Sir David Cox}.
D.V. Hinkley, N. Reid and E.J. Snell (editors), 83--106.  Chapman and Hall. 
}
\seealso{
\code{\link{glm}}, \code{\link{glm.diag.plots}}, \code{\link{summary.glm}}
}
\note{
  See the help for \code{\link{glm.diag.plots}} for an example of the
  use of \code{glm.diag}.
}
\keyword{regression}
\keyword{dplot}
% Converted by Sd2Rd version 1.15.
