% File nlme/man/plot.ACF.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.ACF.Rd,v 1.9 2002/04/16 14:28:53 bates Exp $
\name{plot.ACF}
\title{Plot an ACF Object}
\usage{
\method{plot}{ACF}(x, alpha, xlab, ylab, grid, \dots)
}
\alias{plot.ACF}
\arguments{
 \item{x}{an object inheriting from class \code{ACF},
   consisting of a data frame with two columns named \code{lag} and
   \code{ACF}, representing the autocorrelation values and the
   corresponding lags. 
 }
 \item{alpha}{an optional numeric value with the significance level for
   testing if the autocorrelations are zero. Lines corresponding to the
   lower and upper critical values for a test of level \code{alpha} are
   added to the plot. Default is \code{0}, in which case no lines are
   plotted.
 }
 \item{xlab,ylab}{optional character strings with the x- and y-axis
   labels. Default respectively to \code{"Lag"} and 
   \code{"Autocorrelation"}. 
 }
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default is \code{FALSE}.}
 \item{\dots}{optional arguments passed to the \code{xyplot} function.} 
}
\description{
  an \code{xyplot} of the autocorrelations versus the lags, with
  \code{type = "h"}, is produced. If \code{alpha > 0}, curves
  representing the critical limits for a two-sided test of level
  \code{alpha} for the autocorrelations are added to the plot.
}
\value{
  an \code{xyplot} Trellis plot.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{ACF}}, \code{\link{xyplot}}}
\examples{
fm1 <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary)
plot(ACF(fm1, maxLag = 10), alpha = 0.01)
}
\keyword{models}
