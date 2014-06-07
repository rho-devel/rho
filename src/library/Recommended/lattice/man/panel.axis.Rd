\name{G_panel.axis}
\alias{panel.axis}
\alias{current.panel.limits}
\title{Panel Function for Drawing Axis Ticks and Labels}
\description{
  \code{panel.axis} is the function used by lattice to draw axes. It is
  typically not used by users, except those wishing to create advanced
  annotation. Keep in mind issues of clipping when trying to use it as
  part of the panel function. \code{current.panel.limits} can be used to
  retrieve a panel's x and y limits.
}
\usage{
panel.axis(side = c("bottom", "left", "top", "right"),
           at,
           labels = TRUE,
           draw.labels = TRUE,
           check.overlap = FALSE,
           outside = FALSE,
           ticks = TRUE,
           half = !outside,
           which.half,
           tck = as.numeric(ticks),
           rot = if (is.logical(labels)) 0 else c(90, 0),
           text.col, text.alpha, text.cex, text.font,
           text.fontfamily, text.fontface, text.lineheight,
           line.col, line.lty, line.lwd, line.alpha)

current.panel.limits(unit = "native")
}
\arguments{
  \item{side}{
    A character string indicating which side axes are to be drawn
    on. Partial specification is allowed.
  }
  \item{at}{ Numeric vector giving location of labels. }
  \item{labels}{
    The labels to go along with \code{at}.  The labels can be a character
    vector or a vector of expressions.  Alternatively, \code{at} can be a
    logical flag: If \code{TRUE}, the labels are derived from \code{at},
    otherwise, labels are empty.
  }
  \item{draw.labels}{
    A logical indicating whether labels are to be drawn.
  }
  \item{check.overlap}{
    A logical, whether to check for overlapping of labels.  This also has
    the effect of removing \code{at} values that are \sQuote{too close}
    to the limits.
  }
  \item{outside}{
    A logical flag, indicating whether to draw the labels outside the
    panel or inside. Note that \code{outside=TRUE} will only have a visible
    effect if clipping is disabled for the viewport (panel).
  }
  \item{ticks}{ Logical flag, whether to draw the tickmarks. }
  \item{half}{
    Logical flag, indicating whether only around half the scales will be
    drawn for each side.  This is primarily used for axis labeling in
    \code{\link{splom}}. 
  }
  \item{which.half}{
    Character string, either \code{"lower"} or \code{"upper"},
    indicating which half is to be used for tick locations if \code{half
    = TRUE}.  Defaults to whichever is suitable for \code{\link{splom}}.
  }
  \item{tck}{
    A numeric scalar multiplier for tick length.  Can be negative, in
    which case the ticks point inwards. 
  }
  \item{rot}{
    Rotation angle(s) for labels in degrees. Can be a vector of length 2
    for x- and y-axes.
  }
  \item{text.col}{
    Color for the axis label text.  See \code{\link[grid]{gpar}} for
    more details on this and the other graphical parameters listed
    below.
  }
  \item{text.alpha}{ Alpha-transparency value for the axis label text. }
  \item{text.cex}{ Size multiplier for the axis label text. }
  \item{text.font, text.fontfamily, text.fontface}{ Font for the axis label text. }
  \item{text.lineheight}{ Line height for the axis label text. }
  \item{line.col}{ Color for the axis label text. }
  \item{line.lty}{ Color for the axis. }
  \item{line.lwd}{ Color for the axis. }
  \item{line.alpha}{ Alpha-transparency value for the axis. }
  \item{unit}{ Which grid \code{\link[grid:unit]{unit}} the values
    should be in. }
}

\details{
  \code{panel.axis} can draw axis tick marks inside or outside a
  panel (more precisely, a grid viewport). It honours the (native) axis
  scales. Used in \code{\link{panel.pairs}} for \code{\link{splom}}, as
  well as for all the usual axis drawing by the print method for
  \code{"trellis"} objects. It can also be used to enhance plots
  \sQuote{after the fact} by adding axes.
}

\value{
  \code{current.panel.limits} returns a list with components \code{xlim}
  and \code{ylim}, which are both numeric vectors of length 2, giving
  the scales of the current panel (viewport).  The values correspond to
  the unit system specified by \code{\link[grid:unit]{unit}}, by default
  \code{"native"}.
}

\seealso{
  \code{\link{Lattice}}, \code{\link{xyplot}},
  \code{\link{trellis.focus}}, \code{\link[grid:unit]{unit}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

