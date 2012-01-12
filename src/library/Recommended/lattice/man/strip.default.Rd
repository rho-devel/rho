\name{D_strip.default}
\alias{strip.default}
\alias{strip.custom}
\title{Default Trellis Strip Function}
\usage{
strip.default(which.given,
              which.panel,
              var.name,
              factor.levels,
              shingle.intervals,
              strip.names = c(FALSE, TRUE),
              strip.levels = c(TRUE, FALSE),
              sep = " : ",
              style = 1,
              horizontal = TRUE,
              bg = trellis.par.get("strip.background")$col[which.given],
              fg = trellis.par.get("strip.shingle")$col[which.given],
              par.strip.text = trellis.par.get("add.text"))
strip.custom(\dots)
}
\description{
  \code{strip.default} is the function that draws the strips by default
  in Trellis plots.  Users can write their own strip functions, but most
  commonly this involves calling \code{strip.default} with a slightly
  different arguments.  \code{strip.custom} provides a convenient way to
  obtain new strip functions that differ from \code{strip.default} only
  in the default values of certain arguments.
}

\arguments{
  \item{which.given}{ integer index specifying which of the conditioning
    variables this strip corresponds to. 
  }
  \item{which.panel}{ vector of integers as long as the number of
    conditioning variables. The contents are indices specifying the
    current levels of each of the conditioning variables (thus, this
    would be unique for each distinct packet).  This is identical to the
    return value of \code{\link{which.packet}}, which is a more accurate
    name.
  }
%   \item{panel.number, packet.number}{ integer indices specifying which
%     panel and packet is being drawn (see the entry for \code{panel} in
%     \code{\link{xyplot}} for details).  These arguments are ignored by
%     the \code{strip.default}, but may be used by user supplied
%     replacements.
%   }
  \item{var.name}{ vector of character strings or expressions as long as
    the number of conditioning variables.  The contents are interpreted
    as names for the conditioning variables.  Whether they are shown on
    the strip depends on the values of \code{strip.names} and
    \code{style} (see below).  By default, the names are shown for
    shingles, but not for factors.
  }
  \item{factor.levels}{ vector of character strings or expressions
    giving the levels of the conditioning variable currently being
    drawn.  For more than one conditioning variable, this will vary with
    \code{which.given}.  Whether these levels are shown on the strip
    depends on the values of \code{strip.levels} and \code{style} (see
    below).  \code{factor.levels} may be specified for both factors and
    shingles (despite the name), but by default they are shown only for
    factors.  If shown, the labels may optionally be abbreviated by
    specifying suitable components in \code{par.strip.text} (see
    \code{\link{xyplot}})
  }
  \item{shingle.intervals}{if the current strip corresponds to a shingle,
    this should be a 2-column matrix giving the levels of the shingle.
    (of the form that would be produced by \bold{printing}
    \code{levels(shingle)}). Otherwise, it should be \code{NULL}
  }
  \item{strip.names}{ a logical vector of length 2, indicating whether
    or not the name of the conditioning variable that corresponds to the
    strip being drawn is to be written on the strip. The two components
    give the values for factors and shingles respectively.

    This argument is ignored for a factor when \code{style} is not one
    of 1 and 3.
  }
  \item{strip.levels}{ a logical vector of length 2, indicating whether
    or not the level of the conditioning variable that corresponds to
    the strip being drawn is to be written on the strip.  The two
    components give the values for factors and shingles respectively.
  }
  \item{sep}{ character or expression, serving as a separator if the
    name and level are both to be shown.
  }
  \item{style}{ integer, with values 1, 2, 3, 4 and 5 currently
    supported, controlling how the current level of a factor is encoded.
    Ignored for shingles (actually, when \code{shingle.intervals} is
    non-null.  

    The best way to find out what effect the value of \code{style} has
    is to try them out. Here is a short description: for a style value
    of 1, the strip is colored in the background color with the strip
    text (as determined by other arguments) centered on it. A value of 3
    is the same, except that a part of the strip is colored in the
    foreground color, indicating the current level of the factor. For
    styles 2 and 4, the part corresponding to the current level remains
    colored in the foreground color, however, for style = 2, the
    remaining part is not colored at all, whereas for 4, it is colored
    with the background color. For both these, the names of all the
    levels of the factor are placed on the strip from left to right.
    Styles 5 and 6 produce the same effect (they are subtly different in
    S, this implementation corresponds to 5), they are similar to style
    1, except that the strip text is not centered, it is instead
    positioned according to the current level.

    Note that unlike S-PLUS, the default value of \code{style} is 1.
    \code{strip.names} and \code{strip.levels} have no effect if
    \code{style} is not 1 or 3.
  }
  \item{horizontal}{ logical, specifying whether the labels etc should
    be horizontal.  \code{horizontal=FALSE} is useful for strips on the
    left of panels using \code{strip.left=TRUE} }
  \item{par.strip.text}{ list with parameters controlling the text on
    each strip, with components \code{col}, \code{cex}, \code{font},
    etc.
  }
  \item{bg}{ strip background color. }
  \item{fg}{ strip foreground color. }
  \item{\dots}{
    arguments to be passed on to \code{strip.default}, overriding
    whatever value it would have normally assumed
  }
}

\value{
  \code{strip.default} is called for its side-effect, which is to draw a
  strip appropriate for multi-panel Trellis conditioning plots.
  \code{strip.custom} returns a function that is similar to
  \code{strip.default}, but with different defaults for the arguments
  specified in the call.
}
  

\details{ default strip function for trellis functions. Useful 
  mostly because of the \code{style} argument --- non-default styles
  are often more informative, especially when the names of the levels
  of the factor \code{x} are small. Traditional use is as
  \code{strip = function(\dots) strip.default(style=2,\dots)}, though
  this can be simplified by the use of \code{strip.custom}.
}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}} 
}

\examples{
## Traditional use
xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = function(..., style) strip.default(..., style = 4))

## equivalent call using strip.custom
xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = strip.custom(style = 4))

xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = FALSE,
       strip.left = strip.custom(style = 4, horizontal = FALSE))
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}


