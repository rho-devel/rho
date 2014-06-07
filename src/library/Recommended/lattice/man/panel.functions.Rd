\name{F_2_panel.functions}
\alias{panel.functions}
%% Not data-dependent
\alias{panel.abline}
\alias{panel.refline}
\alias{panel.curve}
\alias{panel.fill}
\alias{panel.grid}
\alias{panel.mathdensity}
%% Data-dependent
\alias{panel.rug}
\alias{panel.lmline}
\alias{panel.average}
\alias{panel.linejoin}
\title{Useful Panel Function Components}
\description{
  These are predefined panel functions available in lattice for use in
  constructing new panel functions (often on-the-fly).
}
\usage{

panel.abline(a = NULL, b = 0,
             h = NULL, v = NULL,
             reg = NULL, coef = NULL,
             col, col.line, lty, lwd, alpha, type,
             \dots,
             reference = FALSE,
             identifier = "abline")
panel.refline(\dots) 

panel.curve(expr, from, to, n = 101,
            curve.type = "l",
            col, lty, lwd, type,
            \dots,
            identifier = "curve")
panel.rug(x = NULL, y = NULL,
          regular = TRUE, 
          start = if (regular) 0 else 0.97,
          end = if (regular) 0.03 else 1,
          x.units = rep("npc", 2),
          y.units = rep("npc", 2),
          col, col.line, lty, lwd, alpha,
          \dots,
          identifier = "rug")
panel.average(x, y, fun = mean, horizontal = TRUE,
              lwd, lty, col, col.line, type,
              \dots,
              identifier = "linejoin")
panel.linejoin(x, y, fun = mean, horizontal = TRUE,
               lwd, lty, col, col.line, type,
               \dots,
               identifier = "linejoin")

% panel.abline(a, b, \dots)
% panel.abline(coef, \dots)
% panel.abline(reg, \dots)
% panel.abline(h= ,v= , \dots)
panel.fill(col, border, \dots, identifier = "fill")
panel.grid(h=3, v=3, col, col.line, lty, lwd, x, y, \dots, identifier = "grid")
panel.lmline(x, y, \dots, identifier = "lmline")
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1),
                  n = 50, col, col.line, lwd, lty, type,
                  \dots, identifier = "mathdensity")
}
\arguments{
  \item{x, y}{ Variables defining the contents of the panel.
    In \code{panel.grid} these are optional and are used only to choose
    an appropriate method of \code{\link{pretty}}.
  }
  \item{a, b}{ Coefficients of the line to be added by
    \code{panel.abline}.  \code{a} can be a vector of length 2,
    representing the coefficients of the line to be added, in which case
    \code{b} should be missing.  \code{a} can also be an appropriate
    \sQuote{regression} object, i.e., an object which has a
    \code{\link{coef}} method that returns a length 2 numeric vector.
    The corresponding line will be plotted.  The \code{reg} argument
    overrides \code{a} if specified.
  }

  \item{coef}{ Coefficients of the line to be added as a vector of
    length 2.
  }

  \item{reg}{ A (linear) regression object, with a \code{\link{coef}}
    method that gives the coefficints of the corresponding regression
    line. }

  \item{h, v}{ For \code{panel.abline}, these are numeric vectors giving
    locations respectively of horizontal and vertical lines to be added
    to the plot, in native coordinates.

    For \code{panel.grid}, these usually specify the number of
    horizontal and vertical reference lines to be added to the plot.
    Alternatively, they can be negative numbers.  \code{h=-1} and
    \code{v=-1} are intended to make the grids aligned with the axis
    labels.  This doesn't always work; all that actually happens is that
    the locations are chosen using \code{pretty}, which is also how the
    label positions are chosen in the most common cases (but not for
    factor variables, for instance).  \code{h} and \code{v} can be
    negative numbers other than \code{-1}, in which case \code{-h} and
    \code{-v} (as appropriate) is supplied as the \code{n} argument to
    \code{\link{pretty}}.

    If \code{x} and/or \code{y} are specified in \code{panel.grid}, they
    will be used to select an appropriate method for
    \code{\link{pretty}}.  This is particularly useful while plotting
    date-time objects.
    
  }

  \item{reference}{ A logical flag determining whether the default
    graphical parameters for \code{panel.abline} should be taken from
    the \dQuote{reference.line} parameter settings.  The default is to
    take them from the \dQuote{add.line} settings.  The
    \code{panel.refline} function is a wrapper around
    \code{panel.abline} that calls it with \code{reference = TRUE}.
  }

  \item{expr}{ An expression considered as a function of \code{x}, or a
    function, to be plotted as a curve.   }
  \item{n}{ The number of points to use for drawing the curve. }
  \item{from, to}{ optional lower and upper x-limits of curve. If
    missing, limits of current panel are used
  }
  \item{curve.type}{ Type of curve (\code{"p"} for points, etc), passed
    to \code{\link{llines}}
  }

  \item{regular}{ A logical flag indicating whether the \sQuote{rug} is
    to be drawn on the \sQuote{regular} side (left / bottom) or not
    (right / top).  }

  \item{start, end}{ endpoints of rug segments, in normalized parent
    coordinates (between 0 and 1).  Defaults depend on value of
    \code{regular}, and cover 3\% of the panel width and height. }

  \item{x.units, y.units}{
    Character vectors, replicated to be of length two.  Specifies the
    (grid) units associated with \code{start} and \code{end} above.
    \code{x.units} and \code{y.units} are for the rug on the x-axis and
    y-axis respectively (and thus are associated with \code{start} and
    \code{end} values on the y and x scales respectively).
  }
  \item{col, col.line, lty, lwd, alpha, border}{ Graphical parameters. }

  \item{type}{ Usually ignored by the panel functions documented here;
    the argument is present only to make sure an explicitly specified
    \code{type} argument (perhaps meant for another function) does not
    affect the display.
  }

  \item{fun}{ The function that will be applied to the subset of
    \code{x} values (or \code{y} if \code{horizontal} is \code{FALSE})
    determined by the unique values of \code{y} (\code{x}). }

  \item{horizontal}{ A logical flag. If \code{FALSE}, the plot is
    \sQuote{transposed} in the sense that the roles of \code{x} and
    \code{y} are switched; \code{x} is now the \sQuote{factor}.
    Interpretation of other arguments change accordingly. See
    documentation of \code{\link{bwplot}} for a fuller explanation.
  }

  \item{dmath}{ A vectorized function that produces density values given
    a numeric vector named \code{x}, e.g., \code{\link{dnorm}}.
  }

  \item{args}{ A list giving additional arguments to be passed to
    \code{dmath}.
  }

  \item{\dots}{ Further arguments, typically graphical parameters,
    passed on to other low-level functions as appropriate.  Color can
    usually be specified by \code{col}, \code{col.line}, and
    \code{col.symbol}, the last two overriding the first for lines and
    points respectively.
  }

  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\details{

  \code{panel.abline} adds a line of the form \code{y = a + b * x}, or
  vertical and/or horizontal lines.  Graphical parameters are obtained
  from the \dQuote{add.line} settings by default.  \code{panel.refline}
  is similar, but uses the \dQuote{reference.line} settings for the
  defaults.

  \code{panel.grid} draws a reference grid.

  \code{panel.curve} adds a curve, similar to what \code{\link{curve}}
  does with \code{add = TRUE}.  Graphical parameters for the curve are
  obtained from the \dQuote{add.line} setting.

  \code{panel.average} treats one of \code{x} and \code{y} as a factor
  (according to the value of \code{horizontal}), calculates \code{fun}
  applied to the subsets of the other variable determined by each unique
  value of the factor, and joins them by a line.  Can be used in
  conjunction with \code{panel.xyplot}, and more commonly with
  \code{\link{panel.superpose}} to produce interaction plots.

  \code{panel.linejoin} is an alias for \code{panel.average}.  It is
  retained for back-compatibility, and may go away in future.

  \code{panel.mathdensity} plots a (usually theoretical) probability
  density function.  This can be useful in conjunction with
  \code{histogram} and \code{densityplot} to visually assess goodness of
  fit (note, however, that \code{qqmath} is more suitable for this).

  \code{panel.rug} adds a \emph{rug} representation of the (marginal)
  data to the panel, much like \code{\link{rug}}.

  \code{panel.lmline(x, y)} is equivalent to
  \code{panel.abline(lm(y ~ x))}.

}

\seealso{
  \link{Lattice}, \code{\link{panel.axis}}, \code{\link{panel.identify}}
  \code{\link{identify}}, \code{\link{trellis.par.set}}.
}

\examples{

## Interaction Plot

bwplot(yield ~ site, barley, groups = year,
       panel = function(x, y, groups, subscripts, ...) {
           panel.grid(h = -1, v = 0)
           panel.stripplot(x, y, ..., jitter.data = TRUE,
                           groups = groups, subscripts = subscripts)
           panel.superpose(x, y, ..., panel.groups = panel.average,
                           groups = groups, subscripts = subscripts)
       },
       auto.key =
       list(points = FALSE, lines = TRUE, columns = 2))

## Superposing a fitted normal density on a Histogram

histogram( ~ height | voice.part, data = singer, layout = c(2, 4),
          type = "density", border = "transparent", col.line = "grey60",
          xlab = "Height (inches)",
          ylab = "Density Histogram\n with Normal Fit",
          panel = function(x, ...) {
              panel.histogram(x, ...)
              panel.mathdensity(dmath = dnorm,
                                args = list(mean=mean(x),sd=sd(x)), ...)
          } )


}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}




