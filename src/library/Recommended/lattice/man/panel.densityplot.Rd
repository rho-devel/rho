\name{F_1_panel.densityplot}
\alias{panel.densityplot}
\title{ Default Panel Function for densityplot }
\description{
  This is the default panel function for \code{densityplot}.
}
\usage{
panel.densityplot(x, darg, plot.points = "jitter", ref = FALSE,
                  groups = NULL, weights = NULL,
                  jitter.amount, type, \dots,
                  identifier = "density")
}
\arguments{
  \item{x}{ data points for which density is to be estimated }
  \item{darg}{
    list of arguments to be passed to the \code{density}
    function. Typically, this should be a list with zero or more of the
    following components : \code{bw}, \code{adjust}, \code{kernel},
    \code{window}, \code{width}, \code{give.Rkern}, \code{n},
    \code{from}, \code{to}, \code{cut}, \code{na.rm} (see
    \code{\link{density}} for details)
  } 
  \item{plot.points}{
    logical specifying whether or not the data points should be plotted
    along with the estimated density.  Alternatively, a character string
    specifying how the points should be plotted.  Meaningful values are
    \code{"rug"}, in which case \code{\link{panel.rug}} is used to plot
    a \sQuote{rug}, and \code{"jitter"}, in which case the points are
    jittered vertically to better distinguish overlapping points.
  }
  \item{ref}{ logical, whether to draw x-axis }
  \item{groups}{ an optional grouping variable.  If present,
    \code{\link{panel.superpose}} will be used instead to display each
    subgroup }
  \item{weights}{ numeric vector of weights for the density
    calculations.  If this is specified, the \code{\dots} part must
    also include a \code{subscripts} argument that matches the weights
    to \code{x}.  }
  \item{jitter.amount}{
    when \code{plot.points="jitter"}, the value to use as the
    \code{amount} argument to \code{\link{jitter}}.
  }
  \item{type}{\code{type} argument used to plot points, if requested.
    This is not expected to be useful, it is available mostly to protect
    a \code{type} argument, if specified, from affecting the density
    curve.
  }
  \item{\dots}{ extra graphical parameters.  Note that additional
    arguments to \code{\link{panel.rug}} cannot be passed on through
    \code{panel.densityplot}. }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{densityplot}},
  \code{\link{jitter}}
}
\keyword{dplot}
















