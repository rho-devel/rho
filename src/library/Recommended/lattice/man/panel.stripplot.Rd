\name{F_1_panel.stripplot}
\alias{panel.stripplot}
\title{ Default Panel Function for stripplot }
\description{
  This is the default panel function for \code{stripplot}. Also see
  \code{panel.superpose}
}
\usage{
panel.stripplot(x, y, jitter.data = FALSE,
                factor = 0.5, amount = NULL,
                horizontal = TRUE, groups = NULL,
                \dots,
                identifier = "stripplot")
}
\arguments{
  \item{x,y}{ coordinates of points to be plotted}
  \item{jitter.data}{ whether points should be jittered to avoid
    overplotting.  The actual jittering is performed inside
    \code{\link{panel.xyplot}}, using its \code{jitter.x} or
      \code{jitter.y} argument (depending on the value of
      \code{horizontal}).  }
  \item{factor, amount}{amount of jittering, see \code{\link{jitter}}}
  \item{horizontal}{ logical. If FALSE, the plot is \sQuote{transposed}
    in the sense that the behaviours of x and y are switched. x is now
    the \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{groups}{ optional grouping variable }

  \item{\dots}{ additional arguments, passed on to
    \code{\link{panel.xyplot}} }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\details{ Creates stripplot (one dimensional scatterplot) of \code{x}
  for each level of \code{y} (or vice versa, depending on the value of
  \code{horizontal})
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{stripplot}}, \code{\link{jitter}}}
\keyword{dplot}
















