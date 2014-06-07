\name{B_06_levelplot}
\title{Level plots and contour plots}
\alias{levelplot}
\alias{contourplot}
\alias{levelplot.formula}
\alias{contourplot.formula}
\alias{levelplot.table}
\alias{contourplot.table}
\alias{levelplot.array}
\alias{contourplot.array}
\alias{levelplot.matrix}
\alias{contourplot.matrix}
\usage{
levelplot(x, data, \dots)
contourplot(x, data, \dots)

\method{levelplot}{formula}(x,
          data,
          allow.multiple = is.null(groups) || outer,
          outer = TRUE,
          aspect = "fill",
          panel = if (useRaster) lattice.getOption("panel.levelplot.raster")
                  else lattice.getOption("panel.levelplot"),
          prepanel = NULL,
          scales = list(),
          strip = TRUE,
          groups = NULL,
          xlab,
          xlim,
          ylab,
          ylim,
          at,
          cuts = 15,
          pretty = FALSE,
          region = TRUE,
          drop.unused.levels =
              lattice.getOption("drop.unused.levels"),
          \dots,
          useRaster = FALSE,
          lattice.options = NULL,
          default.scales = list(),
          default.prepanel =
              lattice.getOption("prepanel.default.levelplot"),
          colorkey = region,
          col.regions,
          alpha.regions,
          subset = TRUE)

\method{contourplot}{formula}(x,
            data,
            panel = lattice.getOption("panel.contourplot"),
            default.prepanel =
                lattice.getOption("prepanel.default.contourplot"),
            cuts = 7,
            labels = TRUE,
            contour = TRUE,
            pretty = TRUE,
            region = FALSE,
            \dots)

\method{levelplot}{table}(x, data = NULL, aspect = "iso", \dots, xlim, ylim)

\method{contourplot}{table}(x, data = NULL, aspect = "iso", \dots, xlim, ylim)

\method{levelplot}{matrix}(x, data = NULL, aspect = "iso",
          \dots, xlim, ylim,
          row.values = seq_len(nrow(x)),
          column.values = seq_len(ncol(x)))

\method{contourplot}{matrix}(x, data = NULL, aspect = "iso",
            \dots, xlim, ylim,
            row.values = seq_len(nrow(x)),
            column.values = seq_len(ncol(x)))


\method{levelplot}{array}(x, data = NULL, \dots)

\method{contourplot}{array}(x, data = NULL, \dots)


}
\description{
  Draws false color level plots and contour plots.
}
\arguments{
  \item{x}{
    for the \code{formula} method, a formula of the form \code{z ~ x * y
      | g1 * g2 * \dots}, where \code{z} is a numeric response, and
    \code{x}, \code{y} are numeric values evaluated on a rectangular
    grid.  \code{g1, g2, \dots} are optional conditional variables, and
    must be either factors or shingles if present.

    Calculations are based on the assumption that all x and y values are
    evaluated on a grid (defined by their unique values). The function
    will not return an error if this is not true, but the display might
    not be meaningful.  However, the x and y values need not be equally
    spaced.

    Both \code{levelplot} and \code{wireframe} have methods for
    \code{matrix}, \code{array}, and \code{table} objects, in which case
    \code{x} provides the \code{z} vector described above, while its
    rows and columns are interpreted as the \code{x} and \code{y}
    vectors respectively.  This is similar to the form used in
    \code{filled.contour} and \code{image}.  For higher-dimensional
    arrays and tables, further dimensions are used as conditioning
    variables.  Note that the dimnames may be duplicated; this is
    handled by calling \code{\link{make.unique}} to make the names
    unique (although the original labels are used for the x- and
    y-axes).
  }
  \item{data}{
    For the \code{formula} methods, an optional data frame in which
    variables in the formula (as well as \code{groups} and
    \code{subset}, if any) are to be evaluated.  Usually ignored with a
    warning in other cases.
  }
  \item{row.values, column.values}{ Optional vectors of values that
    define the grid when \code{x} is a matrix.  \code{row.values} and
    \code{column.values} must have the same lengths as \code{nrow(x)}
    and \code{ncol(x)} respectively.  By default, row and column
    numbers. }
  \item{panel}{
    panel function used to create the display, as described in
    \code{\link{xyplot}}
  }
  \item{aspect}{
    For the \code{matrix} methods, the default aspect ratio is chosen to
    make each cell square.  The usual default is \code{aspect="fill"},
    as described in \code{\link{xyplot}}.
  }
  \item{at}{
    A numeric vector giving breakpoints along the range of
    \code{z}. Contours (if any) will be drawn at these heights, and the
    regions in between would be colored using \code{col.regions}.  In
    the latter case, values outside the range of \code{at} will not be
    drawn at all.  This serves as a way to limit the range of the data
    shown, similar to what a \code{zlim} argument might have been used
    for.  However, this also means that when supplying \code{at}
    explicitly, one has to be careful to include values outside the
    range of \code{z} to ensure that all the data are shown.

    \code{at} can have length one only if \code{region=FALSE}.  
  }
  \item{col.regions}{ color vector to be used if regions is TRUE. The
    general idea is that this should be a color vector of moderately
    large length (longer than the number of regions. By default this is
    100). It is expected that this vector would be gradually varying in
    color (so that nearby colors would be similar). When the colors are
    actually chosen, they are chosen to be equally spaced along this
    vector.  When there are more regions than colors in
    \code{col.regions}, the colors are recycled.  The actual color
    assignment is performed by \code{\link{level.colors}}, which is
    documented separately.
  }
  \item{alpha.regions}{
    numeric, specifying alpha transparency (works only on some devices)
  }
  \item{colorkey}{logical specifying whether a color key is to be drawn
    alongside the plot, or a list describing the color key. The list may
    contain the following components:

    \describe{
    
      \item{\code{space}:}{
        location of the colorkey, can be one of \code{"left"},
	\code{"right"}, \code{"top"} and \code{"bottom"}.  Defaults to
	\code{"right"}.
      }
      
      \item{\code{x}, \code{y}:}{ location, currently unused }

      \item{\code{col}:}{
	A color ramp specification, as in the \code{col.regions}
	argument in \code{\link{level.colors}}
      }
      
      \item{\code{at}:}{
        numeric vector specifying where the colors change. must be of
        length 1 more than the col vector.
      }

      \item{\code{labels}:}{
        a character vector for labelling the \code{at} values, or more
        commonly, a list describing characteristics of the labels.  This
	list may include components \code{labels}, \code{at},
	\code{cex}, \code{col}, \code{rot}, \code{font}, \code{fontface}
	and \code{fontfamily}.
      }

      \item{\code{tick.number}:}{ The approximate number of ticks desired. }

      \item{\code{tck}:}{ A (scalar) multipler for tick lengths. }

      \item{\code{corner}:}{ Interacts with x, y; currently unimplemented }

      \item{\code{width}:}{ The width of the key }

      \item{\code{height}:}{ The length of key as a fraction of the
	appropriate side of plot.
      } 

      \item{\code{raster}:}{ A logical flag indicating whether the
	colorkey should be rendered as a raster image using
	\code{\link{grid.raster}}.  See also
	\code{\link{panel.levelplot.raster}}.
      }

      \item{\code{interpolate}:}{ Logical flag, passed to
	\code{\link{rasterGrob}} when \code{raster=TRUE}. }

      \item{\code{axis.line}:}{ A list giving graphical parameters for
	the color key boundary and tick marks.  Defaults to
	\code{trellis.par.get("axis.line")}.  }

      \item{\code{axis.text}:}{ A list giving graphical parameters for
	the tick mark labels on the color key.  Defaults to
	\code{trellis.par.get("axis.text")}.  }

    }
  }
  \item{contour}{
    A logical flag, indicating whether to draw contour lines.
  }
  \item{cuts}{
    The number of levels the range of \code{z} would be divided into.
  }
  \item{labels}{
    Typically a logical indicating whether contour lines should be
    labelled, but other possibilities for more sophisticated control
    exists.  Details are documented in the help page for
    \code{\link{panel.levelplot}}, to which this argument is passed on
    unchanged.  That help page also documents the \code{label.style}
    argument, which affects how the labels are rendered.
  }
  \item{pretty}{
    A logical flag, indicating whether to use pretty cut locations and
    labels. 
  }
  \item{region}{
    A logical flag, indicating whether regions between contour lines
    should be filled as in a level plot.
  }
  \item{allow.multiple, outer, prepanel, scales, strip, groups, xlab,
    xlim, ylab, ylim, drop.unused.levels, lattice.options,
    default.scales, subset}{
    These arguments are described in the help page for
    \code{\link{xyplot}}.
  }
  \item{default.prepanel}{
    Fallback prepanel function.  See \code{\link{xyplot}}.
  }
  \item{\dots}{
    Further arguments may be supplied.  Some are processed by
    \code{levelplot} or \code{contourplot}, and those that are
    unrecognized are passed on to the panel function.
  }
  \item{useRaster}{
    A logical flag indicating whether raster representations should be
    used, both for the false color image and the color key (if present).
    Effectively, setting this to \code{TRUE} changes the default panel
    function from \code{\link{panel.levelplot}} to
    \code{\link{panel.levelplot.raster}}, and sets the default value of
    \code{colorkey$raster} to \code{TRUE}.
    
    Note that \code{\link{panel.levelplot.raster}} provides only a
    subset of the features of \code{\link{panel.levelplot}}, but setting
    \code{useRaster=TRUE} will not check whether any of the additional
    features have been requested.

    Not all devices support raster images.  For devices that appear to
    lack support, \code{useRaster=TRUE} will be ignored with a warning.
  }
}
\details{
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.

  Other useful arguments are mentioned in the help page for the default
  panel function \code{\link{panel.levelplot}} (these are formally
  arguments to the panel function, but can be specified in the high
  level calls directly).
}

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.
  \url{http://lmdvr.r-forge.r-project.org/}

}

\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}},
  \code{\link{panel.levelplot}}
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)


#S-PLUS example
require(stats)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
       parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length.out = 50)
t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
        radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
contourplot(fit ~ wind * temperature | radiation, data = grid,
            cuts = 10, region = TRUE,
            xlab = "Wind Speed (mph)",
            ylab = "Temperature (F)",
            main = "Cube Root Ozone (cube root ppb)")
detach()
}
\keyword{hplot}



