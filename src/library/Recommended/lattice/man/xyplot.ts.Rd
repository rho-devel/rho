\name{B_01_xyplot.ts}
\title{Time series plotting methods}
\alias{xyplot.ts}

\usage{
\method{xyplot}{ts}(x, data = NULL,
       screens = if (superpose) 1 else colnames(x),
       \dots,
       superpose = FALSE,
       cut = FALSE,
       type = "l",
       col = NULL,
       lty = NULL,
       lwd = NULL,
       pch = NULL,
       cex = NULL,
       fill = NULL,
       auto.key = superpose,
       panel = if (superpose) "panel.superpose"
               else "panel.superpose.plain",
       par.settings = list(),
       layout = NULL, as.table = TRUE,
       xlab = "Time", ylab = NULL,
       default.scales = list(y = list(relation =
           if (missing(cut)) "free" else "same")))
}

\description{
  This function handles time series plotting, including cut-and-stack plots.
  Examples are given of superposing, juxtaposing and styling
  different time series.
}

\arguments{
  \item{x}{
    an object of class \code{\link{ts}}, which may be multi-variate,
    i.e. have a matrix structure with multiple columns.
  }
  \item{data}{
    not used, and must be left as \code{NULL}.
  }
  \item{\dots}{
    additional arguments passed to \code{\link{xyplot}},
    which may pass them on to \code{\link{panel.xyplot}}.
  }
  \item{screens}{
    factor (or coerced to factor) whose levels specify which
    panel each series is to be plotted in.  \code{screens = c(1, 2, 1)}
    would plot series 1, 2 and 3 in panels 1, 2 and 1. May also be a
    named list, see Details below.
  }
  \item{superpose}{
    overlays all series in one panel (via \code{screens = 1}) and uses
    grouped style settings (from
    \code{trellis.par.get("superpose.line")}, etc). Note that this is
    just a convenience argument: its only action is to change the
    default values of other arguments.
  }
  \item{cut}{
    defines a cut-and-stack plot. \code{cut} can be a \code{list} of
    arguments to the function \code{\link{equal.count}},
    i.e. \code{number} (number of intervals to divide into)
    and \code{overlap} (the fraction of overlap between cuts, default
    0.5). If \code{cut} is numeric this is passed as the
    \code{number} argument.

    \code{cut = TRUE} tries to choose an appropriate number of cuts (up
    to a maximum of 6), using \code{\link{banking}}, and assuming a square
    plot region. This should have the effect of minimising wasted space
    when \code{aspect = "xy"}.
  }
  \item{type, col, lty, lwd, pch, cex, fill}{
    graphical arguments, which are processed and eventually passed to
    \code{\link{panel.xyplot}}.
    These arguments can also be vectors or (named) lists, see Details
    for more information.
  }
  \item{auto.key}{
    a logical, or a list describing how to draw a key. See the
    \code{auto.key} entry in \code{\link{xyplot}}. The default here is
    to draw lines, not points, and any specified style arguments should
    show up automatically.
  }
  \item{panel}{
    the panel function. It is recommended to leave this alone, but one
    can pass a \code{panel.groups} argument which is handled by
    \code{\link{panel.superpose}} for each series. 
  }
  \item{par.settings}{
    style settings beyond the standard \code{col}, \code{lty},
    \code{lwd}, etc; see \code{\link{trellis.par.set}} and
    \code{\link{simpleTheme}}.
  }
  \item{layout}{
    numeric vector of length 2 specifying number of columns and rows in
    the plot. The default is to fill columns with up to 6 rows.
  }
  \item{as.table}{
    to draw panels from top to bottom. The order is determined by the
    order of columns in \code{x}.
  }
  \item{xlab, ylab}{
    X axis and Y axis labels; see \code{\link{xyplot}}. Note in
    particular that \code{ylab} may be a character vector, in which case the
    labels are spaced out equally, to correspond to the panels;
    but \emph{NOTE} in this case the vector should be reversed OR the
    argument \code{as.table} set to \code{FALSE}. 
  }
  \item{default.scales}{
    \code{scales} specification. The default is set to have
    \code{"free"} Y axis scales unless \code{cut} is given.
    Note, users should pass the \code{scales} argument rather than
    \code{default.scales}.
  }
}

\details{
  The handling of several graphical parameters is more
  flexible for multivariate series. These parameters can be
  vectors of the same length as the number of series plotted or
  are recycled if shorter. They can also be (partially) named list, e.g.,
  \code{list(A = c(1,2), c(3,4))} in which \code{c(3, 4)} is the
  default value and \code{c(1, 2)} the value only for series \code{A}.
  The \code{screens} argument can be specified in a similar way.

  %% TODO: explain details of style settings, groups, etc?
  
  Some examples are given below.
}

\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}

\author{
  Gabor Grothendieck, Achim Zeileis, Deepayan Sarkar and Felix Andrews
  \email{felix@nfrac.org}.

  The first two authors developed \code{xyplot.ts} in their \pkg{zoo}
  package, including the \code{screens} approach. The third author
  developed a different \code{xyplot.ts} for cut-and-stack plots in the
  \pkg{latticeExtra} package. The final author fused these together.
}

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.
  \url{http://lmdvr.r-forge.r-project.org/} (cut-and-stack plots)

}

\seealso{
  \code{\link{xyplot}},
  \code{\link{panel.xyplot}},
  \code{\link{plot.ts}},
  \code{\link{ts}},
  \code{\link[zoo]{xyplot.zoo}} in the \pkg{zoo} package.
}

\examples{
xyplot(ts(c(1:10,10:1)))

### Figure 14.1 from Sarkar (2008)
xyplot(sunspot.year, aspect = "xy",
       strip = FALSE, strip.left = TRUE,
       cut = list(number = 4, overlap = 0.05))

### A multivariate example; first juxtaposed, then superposed
xyplot(EuStockMarkets, scales = list(y = "same"))
xyplot(EuStockMarkets, superpose = TRUE, aspect = "xy", lwd = 2,
    type = c("l","g"), ylim = c(0, max(EuStockMarkets)))

### Examples using screens (these two are identical)
xyplot(EuStockMarkets, screens = c(rep("Continental", 3), "UK"))
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"))

### Automatic group styles
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
    superpose = TRUE)

xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
    superpose = TRUE, xlim = extendrange(1996:1998),
    par.settings = standard.theme(color = FALSE))

### Specifying styles for series by name
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
    col = list(DAX = "red", FTSE = "blue", "black"), auto.key = TRUE)

xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
    col = list(DAX = "red"), lty = list(SMI = 2), lwd = 1:2,
    auto.key = TRUE)

### Example with simpler data, few data points
set.seed(1)
z <- ts(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))
xyplot(z, screens = 1)
xyplot(z, screens = list(a = "primary (a)", "other (b & c)"),
  type = list(a = c("p", "h"), b = c("p", "s"), "o"),
  pch = list(a = 2, c = 3), auto.key = list(type = "o"))
}
\keyword{hplot}
\keyword{ ts }
