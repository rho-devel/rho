\name{F_1_panel.parallel}
\alias{panel.parallel}
\title{ Default Panel Function for parallel }
\description{
  This is the default panel function for \code{parallel}.
}
\usage{
panel.parallel(x, y, z, subscripts,
               groups = NULL,
               col, lwd, lty, alpha,
               common.scale = FALSE,
               lower,
               upper,
               \dots,
               horizontal.axis = TRUE,
               identifier = "parallel")
}
\arguments{
  \item{x, y}{ dummy variables, ignored. }
  \item{z}{
    The data frame used for the plot.  Each column will be coerced to
    numeric before being plotted, and an error will be issued if this
    fails.
  }
  \item{subscripts}{
    The indices of the rows of \code{z} that are to be displyed in this
    panel.
  }
  \item{groups}{
    An optional grouping variable.  If specified, different groups are
    distinguished by use of different graphical parameters (i.e., rows
    of \code{z} in the same group share parameters).
  }
  \item{col, lwd, lty, alpha}{
    graphical parameters (defaults to the settings for
    \code{superpose.line}).  If \code{groups} is non-null, these
    parameters used one for each group.  Otherwise, they are recycled
    and used to distinguish between rows of the data frame \code{z}.
  }
  \item{common.scale}{
    logical, whether a common scale should be used columns of \code{z}.
    Defaults to \code{FALSE}, in which case the horizontal range for
    each column is different (as determined by \code{lower} and
    \code{upper}).
  }
  \item{lower, upper}{
    numeric vectors replicated to be as long as the number of columns in
    \code{z}.  Determines the lower and upper bounds to be used for
    scaling the corresponding columns of \code{z} after coercing them to
    numeric.  Defaults to the minimum and maximum of each column.
    Alternatively, these could be functions (to be applied on each
    column) that return a scalar.
  }
  \item{\dots}{ other arguments (ignored) }
  \item{horizontal.axis}{logical indicating whether the parallel axes should
    be laid out horizontally (\code{TRUE}) or vertically (\code{FALSE}).
  }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\details{ Produces parallel coordinate plots, which are easier to
  understand from an example than through a verbal description.  See
  example for \code{\link{parallel}}
}

\references{

  Inselberg, Alfred (2009) \emph{Parallel Coordinates: Visual
  Multidimensional Geometry and Its Applications}, Springer. ISBN:
  978-0-387-21507-5.

  Inselberg, A. (1985) \dQuote{The Plane with Parallel Coordinates},
  \emph{The Visual Computer}.
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{parallel}}}
\keyword{dplot}
