\name{B_08_splom}
\title{Scatter Plot Matrices}
\alias{splom}
\alias{parallel}
\alias{parallelplot}
\alias{splom.matrix}
\alias{splom.data.frame}
\alias{parallelplot.matrix}
\alias{parallelplot.data.frame}
\alias{splom.formula}
\alias{parallelplot.formula}
\usage{
splom(x, data, \dots)
parallelplot(x, data, \dots)

\method{splom}{formula}(x,
      data,
      auto.key = FALSE,
      aspect = 1,
      between = list(x = 0.5, y = 0.5),
      panel = lattice.getOption("panel.splom"),
      prepanel,
      scales,
      strip,
      groups,
      xlab,
      xlim,
      ylab = NULL,
      ylim,
      superpanel = lattice.getOption("panel.pairs"),
      pscales = 5,
      varnames = NULL,
      drop.unused.levels,
      \dots,
      lattice.options = NULL,
      default.scales,
      default.prepanel = lattice.getOption("prepanel.default.splom"),
      subset = TRUE)
\method{parallelplot}{formula}(x,
         data,
         auto.key = FALSE,
         aspect = "fill",
         between = list(x = 0.5, y = 0.5),
         panel = lattice.getOption("panel.parallel"),
         prepanel,
         scales,
         strip,
         groups,
         xlab = NULL,
         xlim,
         ylab = NULL,
         ylim,
         varnames = NULL,
         horizontal.axis = TRUE,
         drop.unused.levels,
         \dots,
         lattice.options = NULL,
         default.scales,
         default.prepanel = lattice.getOption("prepanel.default.parallel"),
         subset = TRUE)

\method{splom}{data.frame}(x, data = NULL, \dots, groups = NULL, subset = TRUE)
\method{splom}{matrix}(x, data = NULL, \dots, groups = NULL, subset = TRUE)

\method{parallelplot}{matrix}(x, data = NULL, \dots, groups = NULL, subset = TRUE)
\method{parallelplot}{data.frame}(x, data = NULL, \dots, groups = NULL, subset = TRUE)
}
\description{
  Draw Conditional Scatter Plot Matrices and Parallel Coordinate Plots
}
\arguments{
  \item{x}{
    The object on which method dispatch is carried out.

    For the \code{"formula"} method, a formula describing the structure
    of the plot, which should be of the form \code{~ x | g1 * g2 *
      \dots}, where \code{x} is a data frame or matrix.  Each of
    \code{g1,g2,\dots} must be either factors or shingles. The
    conditioning variables \code{g1, g2, \dots} may be omitted.

    For the \code{data.frame} methods, a data frame.
  }
  \item{data}{
    For the \code{formula} methods, an optional data frame in which
    variables in the formula (as well as \code{groups} and
    \code{subset}, if any) are to be evaluated.
  }
  \item{aspect}{ 
    aspect ratio of each panel (and subpanel), square by default for
    \code{splom}.
  }
  \item{between}{ 
    to avoid confusion between panels and subpanels, the default is to
    show the panels of a splom plot with space between them.
  }
  \item{panel}{
    For \code{parallelplot}, this has the usual interpretation, i.e., a
    function that creates the display within each panel.

    For \code{splom}, the terminology is slightly complicated. The role
    played by the panel function in most other high-level functions is
    played here by the \code{superpanel} function, which is responsible
    for the display for each conditional data subset. \code{panel} is
    simply an argument to the default \code{superpanel} function
    \code{panel.pairs}, and is passed on to it unchanged. It is used
    there to create each pairwise display. See \code{\link{panel.pairs}}
    for more useful options.

  }
  \item{superpanel}{ 
    function that sets up the splom display, by default as a scatterplot
    matrix.
  }
  \item{pscales}{ 
    a numeric value or a list, meant to be a less functional substitute
    for the \code{scales} argument in \code{xyplot} etc. This argument
    is passed to the \code{superpanel} function, and is handled by the
    default superpanel function \code{panel.pairs}. The help page for
    the latter documents this argument in more detail.
  }
  \item{varnames}{ 
    A character or expression vector or giving names to be used for the
    variables in \code{x}. By default, the column names of \code{x}.
  }
  \item{horizontal.axis}{logical indicating whether the parallel axes should
    be laid out horizontally (\code{TRUE}) or vertically (\code{FALSE}).
  }
  \item{auto.key, prepanel, scales,
    strip, groups, xlab, xlim, ylab, ylim, drop.unused.levels,
    lattice.options, default.scales, subset}{ See \code{\link{xyplot}} }
  \item{default.prepanel}{
    Fallback prepanel function.  See \code{\link{xyplot}}.
  }
  \item{\dots}{ Further arguments.  See corresponding entry in
    \code{\link{xyplot}} for non-trivial details.  }
}
\details{
  \code{splom} produces Scatter Plot Matrices.  The role usually played
  by \code{panel} is taken over by \code{superpanel}, which takes a data
  frame subset and is responsible for plotting it.  It is called with
  the coordinate system set up to have both x- and y-limits from
  \code{0.5} to \code{ncol(z) + 0.5}.  The only built-in option
  currently available is \code{\link{panel.pairs}}, which calls a
  further panel function for each pair \code{(i, j)} of variables in
  \code{z} inside a rectangle of unit width and height centered at
  \code{c(i, j)} (see \code{\link{panel.pairs}} for details).

  Many of the finer customizations usually done via arguments to high
  level function like \code{xyplot} are instead done by
  \code{panel.pairs} for \code{splom}. These include control of axis
  limits, tick locations and prepanel calcultions. If you are trying to
  fine-tune your \code{splom} plot, definitely look at the
  \code{\link{panel.pairs}} help page. The \code{scales} argument is
  usually not very useful in \code{splom}, and trying to change it may
  have undesired effects.

  \code{\link{parallelplot}} draws Parallel Coordinate Plots. (Difficult to
  describe, see example.)
  
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.
}
\value{
  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.
}

\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}}, \code{\link{panel.pairs}},
    \code{\link{panel.parallel}}.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                 col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~iris[1:3]|Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
          ltext(x = seq(.6, .8, length.out = 4), 
                y = seq(.9, .6, length.out = 4), 
                labels = c("Three", "Varieties", "of", "Iris"),
                cex = 2)
      })
parallelplot(~iris[1:4] | Species, iris) 
parallelplot(~iris[1:4], iris, groups = Species,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90)))
}
\keyword{hplot}

