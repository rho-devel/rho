\name{D_simpleKey}
\alias{simpleKey}
\title{ Function to generate a simple key }
\description{
  Simple interface to generate a list appropriate for \code{draw.key}
}
\usage{
simpleKey(text, points = TRUE,
          rectangles = FALSE,
          lines = FALSE,
          col, cex, alpha, font,
          fontface, fontfamily, 
          lineheight, \dots)
}
\arguments{
  \item{text}{
    character or expression vector, to be used as labels for levels of
    the grouping variable
  }
  \item{points}{ logical }
  \item{rectangles}{ logical }
  \item{lines}{ logical }
  \item{col, cex, alpha, font, fontface, fontfamily, lineheight}{
    Used as top-level components of the list produced, to be used for
    the text labels. Defaults to the values in
    \code{trellis.par.get("add.text")}
  }
  \item{\dots}{
    further arguments added to the list, eventually passed to
    \code{draw.key}
  }
}
\details{

  A lattice plot can include a legend (key) if an appropriate list is
  specified as the \code{key} argument to a high level Lattice function
  such as \code{xyplot}. This key can be very flexible, but that
  flexibility comes at a cost: this list needs to be fairly complicated
  even in simple situations.  \code{simpleKey} is designed as a useful
  shortcut in the common case of a key drawn in conjunction with a
  grouping variable, using the default graphical settings.

  The \code{simpleKey} function produces a suitable \code{key} argument
  using a simpler interface.  The resulting list will use the
  \code{text} argument as a text component, along with at most one set
  each of points, rectangles, and lines.  The number of entries (rows)
  in the key will be the length of the \code{text} component.  The
  graphical parameters for the additional components will be derived
  from the default graphical settings (wherein lies the simplification,
  as otherwise these would have to be provided explicitly).

  Calling \code{simpleKey} directly is usually unnecessary.  It is most
  commonly invoked (during the plotting of the \code{"trellis"} object)
  when the \code{auto.key} argument is supplied in a high-level plot
  with a \code{groups} argument.  In that case, the \code{text} argument
  of \code{simpleKey} defaults to \code{levels(groups)}, and the
  defaults for the other arguments depend on the relevant high-level
  function.  Note that these defaults can be overridden by supplying
  \code{auto.key} as a list containing the replacement values.

}
\value{
  A list that would work as the \code{key} argument to
  \code{\link{xyplot}}, etc.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{Lattice}}, \code{\link{draw.key}},
  \code{\link{trellis.par.get}}, and
  \code{\link{xyplot}}, specifically the entry for \code{auto.key}.
}
\keyword{dplot}

