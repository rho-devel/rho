\name{F_3_prepanel.functions}
\alias{prepanel.lmline}
\alias{prepanel.loess}
\alias{prepanel.spline}
\alias{prepanel.qqmathline}
\title{ Useful Prepanel Function for Lattice}
\description{
  These are predefined prepanel functions available in Lattice.
}

\usage{
prepanel.lmline(x, y, \dots)
prepanel.qqmathline(x, y = x, distribution = qnorm,
                    probs = c(0.25, 0.75), qtype = 7,
                    groups, subscripts,
                    \dots) 
prepanel.loess(x, y, span, degree, family, evaluation,
               horizontal = FALSE, \dots)
prepanel.spline(x, y, npoints = 101, 
                horizontal = FALSE, \dots,
                keep.data = FALSE)

}
\arguments{
  \item{x, y}{x and y values, numeric or factor}
  \item{distribution}{ quantile function for theoretical
    distribution. This is automatically passed in when this is used as a
    prepanel function in \code{qqmath}.
  }
  \item{qtype}{ type of \code{\link{quantile}}}
  \item{probs}{
    numeric vector of length two, representing probabilities.  If used
    with \code{aspect="xy"}, the aspect ratio will be chosen to make the
    line passing through the corresponding quantile pairs as close to 45
    degrees as possible.
  }
  \item{span, degree, family, evaluation}{ Arguments controlling the
    underlying \code{\link{loess}} smooth. }

  \item{horizontal, npoints}{ See documentation for corresponding panel
    function.
  }

  \item{keep.data}{ Ignored. Present to capture argument of the same
    name in \code{\link{smooth.spline}}. }
  
  \item{groups, subscripts}{ See \code{\link{xyplot}}.  Whenever
    appropriate, calculations are done separately for each group and
    then combined.  }

  \item{\dots}{ Other arguments.  These are passed on to other functions
    if appropriate (in particular, \code{\link{smooth.spline}}), and
    ignored otherwise. }

}

\details{

  All these prepanel functions compute the limits to be large enough to
  contain all points as well as the relevant smooth.

  In addition, \code{prepanel.lmline} computes the \code{dx} and
  \code{dy} such that it reflects the slope of the linear regression
  line; for \code{prepanel.qqmathline}, this is the slope of the line
  passing through the quantile pairs specified by \code{probs}.  For
  \code{prepanel.loess} and \code{prepanel.spline}, \code{dx} and
  \code{dy} reflect the piecewise slopes of the nonlinear smooth.

}

\value{
  usually a list with components \code{xlim}, \code{ylim}, \code{dx} and
  \code{dy}, the first two being used to calculate panel axes limits,
  the last two for banking computations.  The form of these components
  are described under \code{\link{xyplot}}.  There are also several
  prepanel functions that serve as the default for high level functions,
  see \code{\link{prepanel.default.xyplot}}
}

\seealso{

  \link{Lattice}, \code{\link{xyplot}}, \code{\link{banking}},
  \code{\link{panel.loess}}, \code{\link{panel.spline}}.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

