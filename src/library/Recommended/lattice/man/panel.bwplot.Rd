\name{F_1_panel.bwplot}
\alias{panel.bwplot}
\title{ Default Panel Function for bwplot }
\description{
  This is the default panel function for \code{bwplot}.
}
\usage{
panel.bwplot(x, y, box.ratio = 1,
             box.width = box.ratio / (1 + box.ratio),
             horizontal = TRUE,
             pch, col, alpha, cex, 
             font, fontfamily, fontface, 
             fill, varwidth = FALSE,
             notch = FALSE, notch.frac = 0.5,
             \dots,
             levels.fos,
             stats = boxplot.stats,
             coef = 1.5,
             do.out = TRUE,
             identifier = "bwplot")
}
\arguments{
  \item{x, y}{ numeric vector or factor. Boxplots drawn for each unique value of
    \code{y} (\code{x}) if \code{horizontal} is \code{TRUE} (\code{FALSE})}
  \item{box.ratio}{ ratio of box thickness to inter box space}
  \item{box.width}{ thickness of box in absolute units; overrides
    \code{box.ratio}.  Useful for specifying thickness when the
    categorical variable is not a factor, as use of \code{box.ratio}
    alone cannot achieve a thickness greater than 1. }
  \item{horizontal}{
    logical. If FALSE, the plot is \sQuote{transposed} in the sense that
    the behaviours of x and y are switched. x is now the
    \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{pch, col, alpha, cex, font, fontfamily, fontface}{ graphical
    parameters controlling the dot.  \code{pch="|"} is treated
    specially, by replacing the dot with a line (similar to
    \code{\link{boxplot}})
  }
  \item{fill}{ color to fill the boxplot}
  \item{varwidth}{ logical. If TRUE, widths of boxplots are proportional
    to the number of points used in creating it.}
  \item{notch}{ if \code{notch} is \code{TRUE}, a notch is drawn in each
    side of the boxes.  If the notches of two plots do not overlap this
    is \sQuote{strong evidence} that the two medians differ (Chambers et
    al., 1983, p. 62).  See \code{\link{boxplot.stats}} for the
    calculations used. }
  \item{notch.frac}{ numeric in (0,1). When \code{notch=TRUE}, the
    fraction of the box width that the notches should use. }
  \item{stats}{ a function, defaulting to \code{\link{boxplot.stats}},
    that accepts a numeric vector and returns a list similar to the
    return value of \code{boxplot.stats}.  The function must accept
    arguments \code{coef} and \code{do.out} even if they do not use
    them (a \code{\dots} argument is good enough).  This function is
    used to determine the box and whisker plot.  }
  \item{coef, do.out}{ passed to \code{stats}}
  \item{levels.fos}{ numeric values corresponding to positions of the
    factor or shingle variable.  For internal use. }
  \item{\dots}{
    further arguments, ignored.
  }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\details{
  Creates Box and Whisker plot of \code{x} for every level of
  \code{y} (or the other way round if \code{horizontal=FALSE}).  By
  default, the actual boxplot statistics are calculated using
  \code{boxplot.stats}.  Note that most arguments controlling the
  display can be supplied to the high-level \code{bwplot} call directly.

%   Unlike the traditional
%   analog \code{\link{boxplot}}, notches are not implemented.

  Although the graphical parameters for the dot representing the median
  can be controlled by optional arguments, many others can not.  These
  parameters are obtained from the relevant settings parameters
  (\code{"box.rectangle"} for the box, \code{"box.umbrella"} for the
  whiskers and \code{"plot.symbol"} for the outliers).
}

\examples{

bwplot(voice.part ~ height, data = singer,
       xlab = "Height (inches)",
       panel = function(...) {
           panel.grid(v = -1, h = 0)
           panel.bwplot(...)
       },
       par.settings = list(plot.symbol = list(pch = 4)))

bwplot(voice.part ~ height, data = singer,
       xlab = "Height (inches)",
       notch = TRUE, pch = "|")


}

\seealso{
  \code{\link{bwplot}}, \code{\link{boxplot.stats}}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
