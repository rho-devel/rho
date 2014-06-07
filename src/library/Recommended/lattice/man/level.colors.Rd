\name{D_level.colors}
\alias{level.colors}
\title{
  A function to compute false colors representing a numeric or
  categorical variable
}
\description{
  Calculates false colors from a numeric variable (including factors,
  using their numeric codes) given a color scheme and breakpoints.
}
\usage{
level.colors(x, at, col.regions, colors = TRUE, ...)
}
\arguments{
  \item{x}{ A numeric or \code{\link{factor}} variable. }
  \item{at}{ A numeric variable of breakpoints defining intervals along
    the range of \code{x}. }
  \item{col.regions}{ A specification of the colors to be assigned to
    each interval defined by \code{at}.  This could be either a vector
    of colors, or a function that produces a vector of colors when
    called with a single argument giving the number of colors.   See
    details below. }
  \item{colors}{ logical indicating whether colors should be computed
    and returned.  If \code{FALSE}, only the indices representing which
    interval (among those defined by \code{at}) each value in \code{x}
    falls into is returned.  }
  \item{\dots}{ Extra arguments, ignored. }
}
\value{
  A vector of the same length as \code{x}.  Depending on the
  \code{colors} argument, this could be either a vector of colors (in
  a form usable by \R), or a vector of integer indices representing
  which interval the values of \code{x} fall in.
}

\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }

\seealso{ \code{\link{levelplot}}, \code{\link{colorRampPalette}}.  }

\details{

  If \code{at} has length n, then it defines n-1 intervals.  Values of
  \code{x} outside the range of \code{at} are not assigned to an
  interval, and the return value is \code{NA} for such values.

  Colors are chosen by assigning a color to each of the n-1 intervals.
  If \code{col.regions} is a palette function (such as
  \code{\link{topo.colors}}, or the result of calling
  \code{\link{colorRampPalette}}), it is called with n-1 as an argument
  to obtain the colors.  Otherwise, if there are exactly n-1 colors in
  \code{col.regions}, these get assigned to the intervals.  If there are
  fewer than n-1 colors, \code{col.regions} gets recycled.  If there are
  more, a more or less equally spaced (along the length of
  \code{col.regions}) subset is chosen.

}

\examples{

depth.col <-
    with(quakes, 
         level.colors(depth, at = do.breaks(range(depth), 30),
                      col.regions = terrain.colors))


xyplot(lat ~ long | equal.count(stations), quakes,
       strip = strip.custom(var.name = "Stations"),
       colours = depth.col,
       panel = function(x, y, colours, subscripts, ...) {
           panel.xyplot(x, y, pch = 21, col = "transparent",
                        fill = colours[subscripts], ...)
       })

}

\keyword{utilities}
