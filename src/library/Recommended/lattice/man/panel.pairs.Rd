\name{F_1_panel.pairs}
\alias{panel.pairs}
\alias{diag.panel.splom}
\title{ Default Superpanel Function for splom}
\description{
  This is the default superpanel function for \code{splom}.
}
\usage{
panel.pairs(z,
            panel = lattice.getOption("panel.splom"),
            lower.panel = panel,
            upper.panel = panel,
            diag.panel = "diag.panel.splom",
            as.matrix = FALSE,
            groups = NULL,
            panel.subscripts,
            subscripts,
            pscales = 5,
            prepanel.limits = scale.limits,
            varnames = colnames(z),
            varname.col, varname.cex, varname.font,
            varname.fontfamily, varname.fontface,
            axis.text.col, axis.text.cex, axis.text.font,
            axis.text.fontfamily, axis.text.fontface,
            axis.text.lineheight,
            axis.line.col, axis.line.lty, axis.line.lwd,
            axis.line.alpha, axis.line.tck,
            \dots)
diag.panel.splom(x = NULL,
                 varname = NULL, limits, at = NULL, labels = NULL,
                 draw = TRUE, tick.number = 5,
                 varname.col, varname.cex,
                 varname.lineheight, varname.font,
                 varname.fontfamily, varname.fontface,
                 axis.text.col, axis.text.alpha,
                 axis.text.cex, axis.text.font, 
                 axis.text.fontfamily, axis.text.fontface,
                 axis.text.lineheight, 
                 axis.line.col, axis.line.alpha,
                 axis.line.lty, axis.line.lwd,
                 axis.line.tck,
                 \dots)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{panel, lower.panel, upper.panel}{
    The panel function used to display each pair of variables. If
    specified, \code{lower.panel} and \code{upper.panel} are used for
    panels below and above the diagonal respectively.

    In addition to extra arguments not recognized by \code{panel.pairs},
    the list of arguments passed to the panel function also includes
    arguments named \code{i} and \code{j}, with values indicating the
    row and column of the scatterplot matrix being plotted.
  }
  \item{diag.panel}{
    The panel function used for the diagonals. See arguments to
    \code{diag.panel.splom} to know what arguments this function is
    passed when called.
  }
  \item{as.matrix}{
    logical. If \code{TRUE}, the layout of the panels will have origin
    on the top left instead of bottom left (similar to
    \code{pairs}). This is in essence the same functionality as provided
    by \code{as.table} for the panel layout
  }
  \item{groups}{ Grouping variable, if any }
  \item{panel.subscripts}{
    logical specifying whether the panel function accepts an argument
    named \code{subscripts}.
  }
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displayed in this (super)panel.
  }
  \item{pscales}{ Controls axis labels, passed down from \code{splom}.
    If \code{pscales} is a single number, it indicates the approximate
    number of equally-spaced ticks that should appear on each axis.  If
    \code{pscales} is a list, it should have one component for each
    column in \code{z}, each of which itself a list with the following
    valid components:

    \code{at}: a numeric vector specifying tick locations

    \code{labels}: character vector labels to go with at

    \code{limits}: numeric 2-vector specifying axis limits (should be
    made more flexible at some point to handle factors)

    These are specifications on a per-variable basis, and used on all
    four sides in the diagonal cells used for labelling.  Factor
    variables are labelled with the factor names.  Use \code{pscales=0}
    to supress the axes entirely.
  }
  \item{prepanel.limits}{
    A function to calculate suitable axis limits given a single argument
    \code{x} containing a data vector. The return value of the function
    should be similar to the \code{xlim} or \code{ylim} argument
    documented in \code{\link{xyplot}}; that is, it should be a numeric
    or DateTime vector of length 2 defining a range, or a character
    vector representing levels of a factor.

    Most high-level lattice plots (such as \code{xyplot}) use the
    \code{prepanel} function for deciding on axis limits from data.
    This function serves a similar function by calculating the
    per-variable limits.  These limits can be overridden by the
    corresponding \code{limits} component in the \code{pscales} list.
  }
  \item{x}{
    data vector corresponding to that row / column (which will be the
    same for diagonal \sQuote{panels}).
  }
  \item{varname}{ (scalar) character string or expression that is to be
    written centred within the panel
  }
  \item{limits}{
    numeric of length 2, or, vector of characters, specifying the scale
    for that panel (used to calculate tick locations when missing)
  }
  \item{at}{ locations of tick marks}
  \item{labels}{ optional labels for tick marks}
  \item{draw}{
    A logical flag specifying whether to draw the tick marks and labels.
    If \code{FALSE}, variable names are shown but axis annotation is
    omitted. 
  }
  \item{tick.number}{
    A Numeric scalar giving the suggested number of tick marks.
  }
  \item{varnames}{ 
    A character or expression vector or giving names to be used for the
    variables in \code{x}. By default, the column names of \code{x}.
  }
  \item{varname.col}{ Color for the variable name in each diagonal
    panel.  See \code{\link[grid]{gpar}} for details on this and the
    other graphical parameters listed below. }
  \item{varname.cex}{
    Size multiplier for the variable name in each diagonal panel.
  } 
  \item{varname.lineheight}{
    Line height for the variable name in each diagonal panel.
  }
  \item{varname.font, varname.fontfamily, varname.fontface}{
    Font specification for the variable name in each diagonal panel.
  }
  \item{axis.text.col}{ Color for axis label text. } 
  \item{axis.text.cex}{ Size multiplier for axis label text. } 
  \item{axis.text.font, axis.text.fontfamily, axis.text.fontface}{
    Font specification for axis label text.
  } 
  \item{axis.text.lineheight}{ Line height for axis label text. } 
  \item{axis.text.alpha}{ Alpha-transparency for axis label text. } 
  \item{axis.line.col}{ Color for the axes. } 
  \item{axis.line.lty}{ Line type for the axes. } 
  \item{axis.line.lwd}{ Line width for the axes. } 
  \item{axis.line.alpha}{ Alpha-transparency for the axes. } 
  \item{axis.line.tck}{
    A numeric multiplier for the length of tick marks in diagonal
    panels.
  }
  \item{\dots}{
    Further arguments, passed on to \code{panel}, \code{lower.panel},
    \code{upper.panel}, and \code{diag.panel} from \code{panel.pairs}.
    Currently ignored by \code{diag.panel.splom}.
  }

}

\details{
  \code{panel.pairs} is the function that is actually used as the panel
  function in a \code{"trellis"} object produced by \code{splom}.
}

\examples{

Cmat <- outer(1:6,1:6,
              function(i,j) rainbow(11, start=.12, end=.5)[i+j-1])

splom(~diag(6), as.matrix = TRUE,
      panel = function(x, y, i, j, ...) {
          panel.fill(Cmat[i,j])
          panel.text(.5,.5, paste("(",i,",",j,")",sep=""))
      })

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{splom}}}
\keyword{dplot}

