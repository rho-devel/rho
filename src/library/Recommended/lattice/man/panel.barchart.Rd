\name{F_1_panel.barchart}
\alias{panel.barchart}
\title{ Default Panel Function for barchart}
\description{
  Default panel function for \code{barchart}.
}
\usage{
panel.barchart(x, y, box.ratio = 1, box.width,
               horizontal = TRUE,
               origin = NULL, reference = TRUE,
               stack = FALSE,
               groups = NULL, 
               col = if (is.null(groups)) plot.polygon$col
                     else superpose.polygon$col,
               border = if (is.null(groups)) plot.polygon$border
                        else superpose.polygon$border,
               lty = if (is.null(groups)) plot.polygon$lty
                     else superpose.polygon$lty, 
               lwd = if (is.null(groups)) plot.polygon$lwd
                     else superpose.polygon$lwd,
               \dots, identifier = "barchart")
}
\arguments{
  \item{x}{ Extent of Bars. By default, bars start at left of panel,
    unless \code{origin} is specified, in which case they start there.
  }
  \item{y}{ Horizontal location of bars. Possibly a factor. }
  \item{box.ratio}{ Ratio of bar width to inter-bar space. }
  \item{box.width}{ Thickness of bars in absolute units; overrides
    \code{box.ratio}.  Useful for specifying thickness when the
    categorical variable is not a factor, as use of \code{box.ratio}
    alone cannot achieve a thickness greater than 1. }
  \item{horizontal}{
    Logical flag. If FALSE, the plot is \sQuote{transposed} in the sense
    that the behaviours of x and y are switched. x is now the
    \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{origin}{ The origin for the bars.  For grouped displays with
    \code{stack = TRUE}, this argument is ignored and the origin set to
    0.  Otherwise, defaults to \code{NULL}, in which case bars start at
    the left (or bottom) end of a panel.  This choice is somewhat
    unfortuntate, as it can be misleading, but is the default for
    historical reasons.  For tabular (or similar) data, \code{origin =
      0} is usually more appropriate; if not, one should reconsider the
    use of a bar chart in the first place (dot plots are often a good
    alternative).
  }
  \item{reference}{ Logical, whether a reference line is to be drawn at
    the origin.
  }
  \item{stack}{ logical, relevant when groups is non-null. If
    \code{FALSE} (the default), bars for different values of the
    grouping variable are drawn side by side, otherwise they are
    stacked.
  }
  \item{groups}{ Optional grouping variable. }
  \item{col, border, lty, lwd}{
    Graphical parameters for the bars. By default, the trellis parameter
    \code{plot.polygon} is used if there is no grouping variable, otherwise
    \code{superpose.polygon} is used.  \code{col} gives the fill color,
    \code{border} the border color, and \code{lty} and \code{lwd} the
    line type and width of the borders.
  }
  \item{\dots}{ Extra arguments will be accepted but ignored. }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\details{
  A barchart is drawn in the panel. Note that most arguments controlling
  the display can be supplied to the high-level \code{barchart} call
  directly.
}

\seealso{
  \code{\link{barchart}}
}

\examples{

barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), origin = 0,
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(abbreviate = TRUE,
                                minlength = 5)))

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

