\name{F_2_panel.superpose}
\alias{panel.superpose}
\alias{panel.superpose.2}
\alias{panel.superpose.plain}
\title{ Panel Function for Display Marked by groups }
\description{
  These are panel functions for Trellis displays useful when a grouping
  variable is specified for use within panels. The \code{x} (and
  \code{y} where appropriate) variables are plotted with different
  graphical parameters for each distinct value of the grouping variable.
}
\usage{
panel.superpose(x, y = NULL, subscripts, groups,
                panel.groups = "panel.xyplot",
                \dots,
                col, col.line, col.symbol,
                pch, cex, fill, font,
                fontface, fontfamily,
                lty, lwd, alpha,
                type = "p", grid = FALSE,
                distribute.type = FALSE)
panel.superpose.2(\dots, distribute.type = TRUE)

panel.superpose.plain(\dots,
                      col, col.line, col.symbol,
                      pch, cex, fill, font,
                      fontface, fontfamily,
                      lty, lwd, alpha)
}
\arguments{
  \item{x,y}{
    Coordinates of the points to be displayed.  Usually numeric.
  }
  \item{panel.groups}{
    The panel function to be used for each subgroup of points.  Defaults
    to \code{panel.xyplot}.

    To be able to distinguish between different levels of the
    originating group inside \code{panel.groups}, it will be supplied
    two special arguments called \code{group.number} and
    \code{group.value} which will hold the numeric code and factor level
    corresponding to the current level of \code{groups}.  No special
    care needs to be taken when writing a \code{panel.groups} function
    if this feature is not used.
  }

  \item{subscripts}{
    An integer vector of subscripts giving indices of the \code{x} and
    \code{y} values in the original data source.  See the corresponding
    entry in \code{\link{xyplot}} for details.
  }

  \item{groups}{
    A grouping variable.  Different graphical parameters will be used to
    plot the subsets of observations given by each distinct value of
    \code{groups}. The default graphical parameters are obtained from
    the \code{"superpose.symbol"} and \code{"superpose.line"} settings
    using \code{\link{trellis.par.get}} wherever appropriate.
  }

  \item{type}{ Usually a character vector specifying how each group
    should be drawn.  Formally, it is passed on to the
    \code{panel.groups} function, which must know what to do with it.
    By default, \code{panel.groups} is \code{\link{panel.xyplot}}, whose
    help page describes the admissible values.

    The functions \code{panel.superpose} and \code{panel.superpose.2}
    differ only in the default value of \code{distribute.type}, which
    controls the way the \code{type} argument is interpreted.  If
    \code{distribute.type = FALSE}, then the interpretation is the same
    as for \code{panel.xyplot} for each of the unique groups.  In other
    words, if \code{type} is a vector, all the individual components are
    honoured concurrently.  If \code{distribute.type = TRUE},
    \code{type} is replicated to be as long as the number of unique
    values in \code{groups}, and one component used for the points
    corresponding to the each different group.  Even in this case, it is
    possible to request multiple types per group, specifying \code{type}
    as a list, each component being the desired \code{type} vector for
    the corresponding group.

    If \code{distribute.type = FALSE}, any occurrence of \code{"g"} in
    \code{type} causes a grid to be drawn, and all such occurrences are
    removed before \code{type} is passed on to \code{panel.groups}.
  }
  \item{grid}{ Logical flag specifying whether a background reference
    grid should be drawn.  See \code{\link{panel.xyplot}} for details. }

  \item{col}{ A vector color specification.  See Details. }
  \item{col.line}{ A vector color specification.  See Details. }
  \item{col.symbol}{ A vector color specification.  See Details. }
  \item{pch}{ A vector plotting character specification.  See Details. }
  \item{cex}{ A vector size factor specification.  See Details. }
  \item{fill}{ A vector fill color specification.  See Details. }
  \item{font, fontface, fontfamily}{ A vector color specification.  See Details. }
  \item{lty}{ A vector color specification.  See Details. }
  \item{lwd}{ A vector color specification.  See Details. }
  \item{alpha}{ A vector alpha-transparency specification.  See Details. }

  \item{\dots}{ Extra arguments.  Passed down to \code{panel.superpose}
    from \code{panel.superpose.2}, and to \code{panel.groups} from
    \code{panel.superpose}.
  }
  \item{distribute.type}{logical controlling interpretation of the
    \code{type} argument.
  }

}

\details{
  \code{panel.superpose} divides up the \code{x} (and optionally
  \code{y}) variable(s) by the unique values of
  \code{groups[subscripts]}, and plots each subset with different
  graphical parameters.  The graphical parameters (\code{col.symbol},
  \code{pch}, etc.) are usually supplied as suitable atomic vectors, but
  can also be lists.  When \code{panel.groups} is called for the
  \code{i}-th level of \code{groups}, the corresponding element of each
  graphical parameter is passed to it.  In the list form, the individual
  components can themselves be vectors.

  The actual plot for each subgroup is created by the
  \code{panel.groups} function.  With the default \code{panel.groups},
  the \code{col} argument is overridden by \code{col.line} and
  \code{col.symbol} for lines and points respectively, which default to
  the \code{"superpose.line"} and \code{"superpose.symbol"} settings.
  However, \code{col} will still be supplied as an argument to
  \code{panel.groups} functions that make use of it, with a default of
  \code{"black"}.  The defaults of other graphical parameters are also
  taken from the \code{"superpose.line"} and \code{"superpose.symbol"}
  settings as appropriate.  The \code{alpha} parameter takes it default
  from the \code{"superpose.line"} setting.

  \code{panel.superpose} and \code{panel.superpose.2} differ essentially
  in how \code{type} is interpreted by default.  The default behaviour
  in \code{panel.superpose} is the opposite of that in S, which is the
  same as that of \code{panel.superpose.2}.

  \code{panel.superpose.plain} is the same as \code{panel.superpose},
  except that the default settings for the style arguments are the
  same for all groups and are taken from the default plot style.
  It is used in \code{\link{xyplot.ts}}.
}

\seealso{
  Different functions when used as \code{panel.groups} gives different
  types of plots, for example \code{\link{panel.xyplot}},
  \code{\link{panel.dotplot}} and \code{\link{panel.average}} (This can
  be used to produce interaction plots).

  See \code{\link{Lattice}} for an overview of the package, and
  \code{\link{xyplot}} for common arguments (in particular, the
  discussion of the extended formula interface and the \code{groups}
  argument).
}

\author{
  Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}
  (\code{panel.superpose.2} originally contributed by Neil Klepeis)
}
\keyword{dplot}
