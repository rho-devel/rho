\name{C_03_simpleTheme}
\alias{simpleTheme}
\title{ Function to generate a simple theme }
\description{
  Simple interface to generate a list appropriate as a theme, typically
  used as the \code{par.settings} argument in a high level call
}
\usage{
simpleTheme(col, alpha, 
            cex, pch, lty, lwd, font, fill, border,
            col.points, col.line, 
            alpha.points, alpha.line)
}
\arguments{
  \item{col, col.points, col.line}{
    A color specification.  \code{col} is used for components
    \code{"plot.symbol"}, \code{"plot.line"}, \code{"plot.polygon"},
    \code{"superpose.symbol"}, \code{"superpose.line"}, and
    \code{"superpose.polygon"}.  \code{col.points} overrides
    \code{col}, but is used only for \code{"plot.symbol"} and
    \code{"superpose.symbol"}.  Similarly, \code{col.line} overrides
    \code{col} for \code{"plot.line"} and \code{"superpose.line"}.
    The arguments can be vectors, but only the first component is used
    for scalar targets (i.e., the ones without \code{"superpose"} in
    their name).
  }
  \item{alpha, alpha.points, alpha.line}{
    A numeric alpha transparency specification.  The same rules as
    \code{col}, etc., apply.
  }
  \item{cex, pch, font}{ Parameters for points.  Applicable for
    components \code{plot.symbol} (for which only the first component is
    used) and \code{superpose.symbol} (for which the arguments can be
    vectors).
  }
  \item{lty, lwd}{ Parameters for lines.  Applicable for
    components \code{plot.line} (for which only the first component is
    used) and \code{superpose.line} (for which the arguments can be
    vectors).
  }
  \item{fill}{ fill color, applicable for components \code{plot.symbol},
    \code{plot.polygon}, \code{superpose.symbol}, and
    \code{superpose.polygon}.  }
  \item{border}{ border color, applicable for components
    \code{plot.polygon} and \code{superpose.polygon}.  }

}
\details{
  The appearance of a lattice display depends partly on the
  \dQuote{theme} active when the display is plotted (see
  \code{\link{trellis.device}} for details).  This theme is used to
  obtain defaults for various graphical parameters, and in particular,
  the \code{auto.key} argument works on the premise that the same source
  is used for both the actual graphical encoding and the legend.  The
  easiest way to specify custom settings for a particular display is to
  use the \code{par.settings} argument, which is usually tedious to
  construct as it is a nested list.  The \code{simpleTheme} function can
  be used in such situations as a wrapper that generates a suitable list
  given parameters in simple \code{name=value} form, with the nesting
  made implicit.  This is less flexible, but straightforward and
  sufficient in most situations.

}
\value{
  A list that would work as the \code{theme} argument to
  \code{\link{trellis.device}} and \code{\link{trellis.par.set}}, or
  as the \code{par.settings} argument to any high level lattice function
  such as \code{\link{xyplot}}.
}
\examples{

str(simpleTheme(pch = 16))

dotplot(variety ~ yield | site, data = barley, groups = year,
        auto.key = list(space = "right"),
        par.settings = simpleTheme(pch = 16),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6))

}
\author{
  Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}, based on a
  suggestion from John Maindonald.  
}
\seealso{
  \code{\link{trellis.device}}, \code{\link{xyplot}},
  \code{\link{Lattice}}
}
\keyword{dplot}
