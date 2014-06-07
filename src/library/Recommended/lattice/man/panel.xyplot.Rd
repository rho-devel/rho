\name{F_1_panel.xyplot}
\alias{panel.xyplot}
\alias{panel.splom}
\alias{panel.qq}
\title{ Default Panel Function for xyplot }
\description{
  This is the default panel function for \code{xyplot}. Also see
  \code{panel.superpose}.  The default panel functions for
  \code{splom} and \code{qq} are essentially the same function.
}
\usage{
panel.xyplot(x, y, type = "p",
             groups = NULL,
             pch, col, col.line, col.symbol,
             font, fontfamily, fontface,
             lty, cex, fill, lwd,
             horizontal = FALSE, \dots,
             grid = FALSE, abline = NULL,
             jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL,
             identifier = "xyplot")
panel.splom(\dots, identifier = "splom")
panel.qq(\dots, identifier = "qq")
}
\arguments{
  \item{x,y}{ variables to be plotted in the scatterplot}
  \item{type}{ character vector consisting of one or more of the
    following: \code{"p"}, \code{"l"}, \code{"h"}, \code{"b"},
    \code{"o"}, \code{"s"}, \code{"S"}, \code{"r"}, \code{"a"},
    \code{"g"}, \code{"smooth"}, and \code{"spline"}.  If \code{type}
    has more than one element, an attempt is made to combine the effect
    of each of the components.

    The behaviour if any of the first six are included in \code{type} is
    similar to the effect of \code{type} in \code{\link{plot}} (type
    \code{"b"} is actually the same as \code{"o"}).  \code{"r"} adds a
    linear regression line (same as \code{\link{panel.lmline}}, except
    for default graphical parameters). \code{"smooth"} adds a loess fit
    (same as \code{\link{panel.loess}}).  \code{"spline"} adds a cubic
    smoothing spline fit (same as \code{\link{panel.spline}}).
    \code{"g"} adds a reference grid using \code{\link{panel.grid}} in
    the background (but using the \code{grid} argument is now the
    preferred way to do so).  \code{"a"} has the effect of calling
    \code{\link{panel.average}}, which can be useful for creating
    interaction plots.  The effect of several of these specifications
    depend on the value of \code{horizontal}.

    Type \code{"s"} (and \code{"S"}) sorts the values along one of the
    axes (depending on \code{horizontal}); this is unlike the behavior
    in \code{plot}.  For the latter behavior, use \code{type = "s"} with
    \code{panel = panel.points}.

    See \code{example(xyplot)} and \code{demo(lattice)} for examples.
  }
  \item{groups}{ an optional grouping variable.  If present,
    \code{\link{panel.superpose}} will be used instead to display each
    subgroup }
  \item{col, col.line, col.symbol}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}.
  }
  \item{font, fontface, fontfamily}{
    font used when \code{pch} is a character
  }
  \item{pch, lty, cex, lwd, fill}{
    other graphical parameters.  \code{fill} serves the purpose of
    \code{bg} in \code{\link{points}} for certain values of \code{pch}
  }
  \item{horizontal}{
    A logical flag controlling the orientation for certain
    \code{type}'s, e.g., \code{"h"}, \code{"s"}, ans \code{"S"}.
  }
  \item{\dots}{
    Extra arguments, if any, for \code{panel.xyplot}.  In most cases
    \code{panel.xyplot} ignores these.  For types "r" and "smooth",
    these are passed on to \code{panel.lmline} and \code{panel.loess}
    respectively.
  }
  \item{grid}{
    A logical flag, character string, or list specifying whether and how
    a background grid should be drawn.  This provides the same
    functionality as \code{type="g"}, but is the preferred alternative
    as the effect \code{type="g"} is conceptually different from that of
    other \code{type} values (which are all data-dependent).  Using the
    \code{grid} argument also allows more flexibility.

    Most generally, \code{grid} can be a list of arguments to be
    supplied to \code{\link{panel.grid}}, which is called with those
    arguments.  Three shortcuts are available:
    \describe{
      \item{\code{TRUE}:}{ roughly equivalent to \code{list(h = -1, v = -1)} }
      \item{\code{"h"}:}{ roughly equivalent to \code{list(h = -1, v = 0)} }
      \item{\code{"v"}:}{ roughly equivalent to \code{list(h = 0, v = -1)} }
    }
    No grid is drawn if \code{grid = FALSE}.
  }
  \item{abline}{
    A numeric vector or list, specifying arguments arguments for
    \code{\link{panel.abline}}, which is called with those arguments.
    If specified as a (possibly named) numeric vector, \code{abline} is
    coerced to a list.  This allows arguments of the form \code{abline =
    c(0, 1)}, which adds the diagonal line, or \code{abline = c(h = 0, v
    = 0)}, which adds the x- and y-axes to the plot.  Use the list form
    for finer control; e.g., \code{abline = list(h = 0, v = 0, col =
      "grey")}.

    For more flexibility, use \code{\link{panel.abline}} directly.
  }
  \item{jitter.x, jitter.y}{ logical, whether the data should be
    jittered before being plotted. }
  \item{factor, amount}{ controls amount of jittering. }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}



\details{

  Creates scatterplot of \code{x} and \code{y}, with various
  modifications possible via the type argument. \code{panel.qq} draws a
  45 degree line before calling \code{panel.xyplot}.

  Note that most of the arguments controlling the display can be
  supplied directly to the high-level (e.g. \code{\link{xyplot}}) call.

}

\examples{

types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
types.horiz <- c("s", "S", "h", "a", "smooth")
horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))

types <- c(types.plain, types.horiz)

x <- sample(seq(-10, 10, length.out = 15), 30, TRUE)
y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

xyplot(y ~ x | gl(1, length(types)),
       xlab = "type", 
       ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
       as.table = TRUE, layout = c(5, 3),
       between = list(y = c(0, 1)),
       strip = function(...) {
           panel.fill(trellis.par.get("strip.background")$col[1])
           type <- types[panel.number()]
           grid::grid.text(label = sprintf('"\%s"', type), 
                           x = 0.5, y = 0.5)
           grid::grid.rect()
       },
       scales = list(alternating = c(0, 2), tck = c(0, 0.7), draw = FALSE),
       par.settings = 
       list(layout.widths = list(strip.left = c(1, 0, 0, 0, 0))),
       panel = function(...) {
           type <- types[panel.number()]
           horizontal <- horiz[panel.number()]
           panel.xyplot(..., 
                        type = type,
                        horizontal = horizontal)
       })[rep(1, length(types))]

}


\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{panel.superpose}},
  \code{\link{xyplot}},
  \code{\link{splom}}
}
\keyword{dplot}
