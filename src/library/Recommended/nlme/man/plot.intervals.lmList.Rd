% File nlme/man/plot.intervals.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.intervals.lmList.Rd,v 1.9.2.1 2003/04/17 22:28:45 bates Exp $
\name{plot.intervals.lmList}
\title{Plot lmList Confidence Intervals}
\usage{
\method{plot}{intervals.lmList}(x, \dots)
}
\alias{plot.intervals.lmList}
\arguments{
 \item{x}{an object inheriting from class \code{intervals.lmList},
   representing confidence intervals and estimates for the coefficients
   in the \code{lm} components of the \code{lmList} object used to
   produce \code{x}.
 }
 \item{\dots}{optional arguments passed to the Trellis \code{dotplot}
   function.
 } 
}
\description{
  A Trellis dot-plot of the confidence intervals on the linear model
  coefficients is generated, with a different panel for each
  coefficient. Rows in the dot-plot correspond to the names of the
  \code{lm} components of the \code{lmList} object used to produce
  \code{x}. The lower and upper confidence limits are connected by
  a line segment and the estimated coefficients are marked with a
  \code{"+"}. The Trellis function \code{dotplot} is used in this method
  function.  
}

\value{
  a Trellis plot with the confidence intervals on the coefficients of
  the individual \code{lm} components of the \code{lmList} that
  generated \code{x}. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{intervals.lmList}},
  \code{\link{lmList}},
  \code{\link{dotplot}}} 
\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
plot(intervals(fm1))
}
\keyword{models}
