\name{F_2_llines}
\alias{llines}
\alias{lpoints}
\alias{ltext}
\alias{llines.default}
\alias{lpoints.default}
\alias{ltext.default}
\alias{lplot.xy}
\alias{larrows}
\alias{lsegments}
\alias{lrect}
\alias{lpolygon}
\alias{panel.arrows}
\alias{panel.lines}
\alias{panel.points}
\alias{panel.segments}
\alias{panel.text}
\alias{panel.rect}
\alias{panel.polygon}
\title{Replacements of traditional graphics functions}

\description{

  These functions are intended to replace common low level traditional
  graphics functions, primarily for use in panel functions.  The
  originals can not be used (at least not easily) because lattice panel
  functions need to use grid graphics.  Low level drawing functions in
  grid can be used directly as well, and is often more flexible.  These
  functions are provided for convenience and portability.

}

\usage{
lplot.xy(xy, type, pch, lty, col, cex, lwd,
         font, fontfamily, fontface,
         col.line, col.symbol, alpha, fill,
         origin = 0, \dots, identifier, name.type)

llines(x, ...)
lpoints(x, ...)
ltext(x, ...)

\method{llines}{default}(x, y = NULL, type = "l",
       col, alpha, lty, lwd, \dots, identifier, name.type)
\method{lpoints}{default}(x, y = NULL, type = "p", col, pch, alpha, fill,
        font, fontfamily, fontface, cex, \dots, identifier, name.type)  
\method{ltext}{default}(x, y = NULL, labels = seq_along(x),
      col, alpha, cex, srt = 0,
      lineheight, font, fontfamily, fontface,
      adj = c(0.5, 0.5), pos = NULL, offset = 0.5, \dots, identifier, name.type)  

lsegments(x0, y0, x1, y1, x2, y2,
          col, alpha, lty, lwd,
          font, fontface, \dots, identifier, name.type)
lrect(xleft, ybottom, xright, ytop,
      x = (xleft + xright) / 2,
      y = (ybottom + ytop) / 2,
      width = xright - xleft,
      height = ytop - ybottom,
      col = "transparent",
      border = "black",
      lty = 1, lwd = 1, alpha = 1,
      just = "center",
      hjust = NULL, vjust = NULL,
      \dots, identifier, name.type)
larrows(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
        angle = 30, code = 2, length = 0.25, unit = "inches",
        ends = switch(code, "first", "last", "both"),
        type = "open",
        col = add.line$col,
        alpha = add.line$alpha,
        lty = add.line$lty,
        lwd = add.line$lwd,
        fill = NULL, \dots, identifier, name.type)
lpolygon(x, y = NULL,
         border = "black", col = "transparent", fill = NULL, 
         font, fontface, \dots, identifier, name.type)

panel.lines(\dots)
panel.points(\dots)
panel.segments(\dots)
panel.text(\dots)
panel.rect(\dots)
panel.arrows(\dots)
panel.polygon(\dots)
}
\arguments{

  \item{ x, y, x0, y0, x1, y1, x2, y2, xy}{ locations. \code{x2} and
    \code{y2} are available for for S compatibility.  }
  \item{length, unit}{
    determines extent of arrow head.  \code{length} specifies the length
    in terms of \code{unit}, which can be any valid grid unit as long as
    it doesn't need a \code{data} argument.  \code{unit} defaults to
    inches, which is the only option in the base version of the
    function, \code{\link{arrows}}.  }
  \item{angle, code, type, labels, srt, adj, pos, offset}{ arguments
    controlling behaviour.  See respective base functions for details.
    For \code{larrows} and \code{panel.larrows}, \code{type} is either
    \code{"open"} or \code{"closed"}, indicating the type of
    arrowhead. }
  \item{ends}{ serves the same function as \code{code}, using
    descriptive names rather than integer codes.  If specified, this
    overrides \code{code}}
  \item{col, alpha, lty, lwd, fill, pch, cex, lineheight, font,
    fontfamily, fontface, col.line, col.symbol, border}{ graphical
    parameters.  \code{fill} applies to points when \code{pch} is in
    \code{21:25} and specifies the fill color, similar to the \code{bg}
    argument in the base graphics function \code{\link{points}}. For
    devices that support alpha-transparency, a numeric argument
    \code{alpha} between 0 and 1 can controls transparency.  Be careful
    with this, since for devices that do not support alpha-transparency,
    nothing will be drawn at all if this is set to anything other than
    0.

    \code{fill}, \code{font} and \code{fontface} are included in
    \code{lpolygon} and \code{lsegments} only to ensure that they are
    not passed down (as \code{\link{gpar}} does not like them).
  }
  \item{origin}{ for \code{type="h"} or \code{type="H"}, the value to
    which lines drop down. }
  \item{xleft, ybottom, xright, ytop}{ see \code{\link{rect}}}
  \item{width, height, just, hjust, vjust}{ finer control over
    rectangles, see \code{\link[grid:grid.rect]{grid.rect}} }
  \item{\dots}{ extra arguments, passed on to lower level functions as
    appropriate.  }
  \item{identifier}{
    A character string that is prepended to the name of the grob
    that is created.
  }
  \item{name.type}{
    A character value indicating whether the name of the grob
    should have panel or strip information added to it.
    Typically either \code{"panel"}, \code{"strip"},
    \code{"strip.left"}, or \code{""} (for no extra information).
  }
}


\details{
  These functions are meant to be grid replacements of the corresponding
  base R graphics functions, to allow existing Trellis code to be used
  with minimal modification.  The functions \code{panel.*} are
  essentally identical to the \code{l*} versions, are recommended for
  use in new code (as opposed to ported code) as they have more readable
  names.

  See the documentation of the base functions for usage. Not all
  arguments are always supported. All these correspond to the default
  methods only.
%   For \code{ltext}, only values 0, .5 and 1 for \code{adj}
%   have any effect.
}
\note{
  There is a new \code{type="H"} option wherever appropriate, which is
  similar to \code{type="h"}, but with horizontal lines.
}

\seealso{\code{\link{points}}, \code{\link{lines}}, \code{\link{rect}},
  \code{\link{text}}, \code{\link{segments}}, \code{\link{arrows}},
  \code{\link{Lattice}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
