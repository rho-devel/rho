\name{F_3_prepanel.default}
\alias{prepanel.default.bwplot}
\alias{prepanel.default.histogram}
\alias{prepanel.default.qq}
\alias{prepanel.default.xyplot}
\alias{prepanel.default.cloud}
\alias{prepanel.default.levelplot}
\alias{prepanel.default.qqmath}
\alias{prepanel.default.densityplot}
\alias{prepanel.default.parallel}
\alias{prepanel.default.splom}

\title{ Default Prepanel Functions }

\description{
  These prepanel functions are used as fallback defaults in various
  high level plot functions in Lattice.  These are rarely useful to
  normal users but may be helpful in developing new displays.
}

\usage{

prepanel.default.bwplot(x, y, horizontal, nlevels, origin, stack, \dots)
prepanel.default.histogram(x, breaks, equal.widths, type, nint, \dots)
prepanel.default.qq(x, y, \dots)
prepanel.default.xyplot(x, y, type, subscripts, groups, \dots)
prepanel.default.cloud(perspective, distance,
                       xlim, ylim, zlim,
                       screen = list(z = 40, x = -60),
                       R.mat = diag(4),
                       aspect = c(1, 1), panel.aspect = 1,
                       \dots, zoom = 0.8)
prepanel.default.levelplot(x, y, subscripts, \dots)
prepanel.default.qqmath(x, f.value, distribution, qtype,
                        groups, subscripts, \dots, tails.n = 0)
prepanel.default.densityplot(x, darg, groups, weights, subscripts, \dots)
prepanel.default.parallel(x, y, z, \dots, horizontal.axis)
prepanel.default.splom(z, \dots)

}
\arguments{
  \item{x, y}{x and y values, numeric or factor}
  \item{horizontal}{ logical, applicable when one of the variables is to
    be treated as categorical (factor or shingle).
  }
  \item{horizontal.axis}{ logical indicating whether the
    parallel axes should be laid out horizontally (\code{TRUE}) or
    vertically (\code{FALSE}).
  }
  \item{nlevels}{ number of levels of such a categorical variable. }
  \item{origin, stack}{ for barcharts or the \code{type="h"} plot type}
  \item{breaks, equal.widths, type, nint}{ details of histogram
    calculations.  \code{type} has a different meaning in
    \code{prepanel.default.xyplot} (see \code{\link{panel.xyplot}})
  }
  \item{groups, subscripts}{ See \code{\link{xyplot}}.  Whenever
    appropriate, calculations are done separately for each group and
    then combined.  }
  \item{weights}{ numeric vector of weights for the density
    calculations.  If this is specified, it is subsetted by
    \code{subscripts} to match it to \code{x}.  }
  \item{perspective, distance, xlim, ylim, zlim, screen, R.mat, aspect,
    panel.aspect, zoom}{ see \code{\link{panel.cloud}}}
  \item{f.value, distribution, tails.n}{ see \code{panel.qqmath} }
  \item{darg}{ list of arguments passed to \code{\link{density}}}
  \item{z}{ see \code{\link{panel.parallel}} and \code{\link{panel.pairs}} }

  \item{qtype}{ type of \code{\link{quantile}}}
  \item{\dots}{ other arguments, usually ignored }
}

\value{
  A list with components \code{xlim}, \code{ylim}, \code{dx} and
  \code{dy}, and possibly \code{xat} and \code{yat}, the first two being
  used to calculate panel axes limits, the last two for banking
  computations.  The form of these components are described in the help
  page for \code{\link{xyplot}}.
}

\seealso{

  \code{\link{xyplot}}, \code{\link{banking}}, \code{\link{Lattice}}.
  See documentation of corresponding panel functions for more details
  about the arguments.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

