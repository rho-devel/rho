\name{F_2_panel.spline}
\alias{panel.spline}
\title{Panel Function to Add a Spline Smooth}
\description{
  A predefined panel function that can be used to add a spline smooth
  based on the provided data.
}
\usage{
panel.spline(x, y, npoints = 101,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col, col.line = plot.line$col,
             type,
             horizontal = FALSE, \dots,
             keep.data = FALSE,
             identifier = "spline")
}
\arguments{
  \item{x, y}{ Variables defining the data to be used. }

  \item{npoints}{ The number of equally spaced points within the range
    of the predictor at which the fitted model is evaluated for
    plotting.
  }

  \item{lwd, lty, col, col.line}{ Graphical parameters for the added
    line.  \code{col.line} overrides \code{col}. }

  \item{type}{ Ignored.  The argument is present only to make sure that
    an explicitly specified \code{type} argument (perhaps meant for
    another function) does not affect the display.  }

  \item{horizontal}{
    A logical flag controlling which variable is to be treated as the
    predictor (by default \code{x}) and which as the response (by
    default \code{y}).  If \code{TRUE}, the plot is \sQuote{transposed}
    in the sense that \code{y} becomes the predictor and \code{x} the
    response.  (The name \sQuote{horizontal} may seem an odd choice for
    this argument, and originates from similar usage in
    \code{\link{bwplot}}).
  }

  \item{keep.data}{ Passed on to \code{\link{smooth.spline}}.  The
    default here (\code{FALSE}) is different, and results in the
    original data not being retained in the fitted spline model.  It may
    be useful to set this to \code{TRUE} if the return value of
    \code{panel.spline}, which is the fitted model as returned by
    \code{\link{smooth.spline}}, is to be used for subsequent
    computations.  }

  \item{\dots}{ Extra arguments, passed on to
    \code{\link{smooth.spline}} and \code{\link{panel.lines}} as
    appropriate.  }

  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\value{
  The fitted model as returned by \code{\link{smooth.spline}}.
}

\seealso{\link{Lattice},
  \code{\link[stats:scatter.smooth]{smooth.spline}},
  \code{\link{prepanel.spline}}  
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

