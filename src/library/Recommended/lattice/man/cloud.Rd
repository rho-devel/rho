\name{B_07_cloud}
\title{3d Scatter Plot and Wireframe Surface Plot}
\alias{cloud}
\alias{wireframe}
\alias{cloud.formula}
\alias{cloud.matrix}
\alias{cloud.table}
\alias{wireframe.formula}
\alias{wireframe.matrix}
\usage{
cloud(x, data, \dots)
wireframe(x, data, \dots)

\method{cloud}{formula}(x,
      data,
      allow.multiple = is.null(groups) || outer,
      outer = FALSE,
      auto.key = FALSE,
      aspect = c(1,1),
      panel.aspect = 1,
      panel = lattice.getOption("panel.cloud"),
      prepanel = NULL,
      scales = list(),
      strip = TRUE,
      groups = NULL,
      xlab,
      ylab,
      zlab,
      xlim = if (is.factor(x)) levels(x) else range(x, finite = TRUE),
      ylim = if (is.factor(y)) levels(y) else range(y, finite = TRUE),
      zlim = if (is.factor(z)) levels(z) else range(z, finite = TRUE),
      at,
      drape = FALSE,
      pretty = FALSE,
      drop.unused.levels,
      \dots,
      lattice.options = NULL,
      default.scales =
      list(distance = c(1, 1, 1),
           arrows = TRUE,
           axs = axs.default),
      default.prepanel = lattice.getOption("prepanel.default.cloud"),
      colorkey,
      col.regions,
      alpha.regions,
      cuts = 70,
      subset = TRUE,
      axs.default = "r")

\method{wireframe}{formula}(x,
          data,
          panel = lattice.getOption("panel.wireframe"),
          default.prepanel = lattice.getOption("prepanel.default.wireframe"),
          \dots)

\method{cloud}{matrix}(x, data = NULL, type = "h", 
      zlab = deparse(substitute(x)), aspect, \dots,
      xlim, ylim, row.values, column.values)

\method{cloud}{table}(x, data = NULL, groups = FALSE,
      zlab = deparse(substitute(x)),
      type = "h", \dots)

\method{wireframe}{matrix}(x, data = NULL,
          zlab = deparse(substitute(x)), aspect, \dots,
          xlim, ylim, row.values, column.values)
}
\description{
  Generic functions to draw 3d scatter plots and surfaces.  The
  \code{"formula"} methods do most of the actual work.
}
\arguments{
  \item{x}{
    The object on which method dispatch is carried out.

    For the \code{"formula"} methods, a formula of the form \code{z ~ x
      * y | g1 * g2 * \dots}, where \code{z} is a numeric response, and
    \code{x}, \code{y} are numeric values. \code{g1, g2, \dots}, if
    present, are conditioning variables used for conditioning, and must
    be either factors or shingles. In the case of \code{wireframe},
    calculations are based on the assumption that the \code{x} and
    \code{y} values are evaluated on a rectangular grid defined by their
    unique values.  The grid points need not be equally spaced.

    For \code{wireframe}, \code{x}, \code{y} and \code{z} may also be
    matrices (of the same dimension), in which case they are taken to
    represent a 3-D surface parametrized on a 2-D grid (e.g., a sphere).
    Conditioning is not possible with this feature. See details below.

    Missing values are allowed, either as \code{NA} values in the
    \code{z} vector, or missing rows in the data frame (note however
    that in that case the X and Y grids will be determined only by the
    available values). For a grouped display (producing multiple
    surfaces), missing rows are not allowed, but \code{NA}-s in \code{z}
    are.

    Both \code{wireframe} and \code{cloud} have methods for
    \code{matrix} objects, in which case \code{x} provides the
    \code{z} vector described above, while its rows and columns are
    interpreted as the \code{x} and \code{y} vectors respectively.  This
    is similar to the form used in \code{persp}.
  }
  \item{data}{
    for the \code{"formula"} methods, an optional data frame in which
    variables in the formula (as well as \code{groups} and
    \code{subset}, if any) are to be evaluated.  \code{data} should not
    be specified except when using the \code{"formula"} method.
  }
  \item{row.values, column.values}{ Optional vectors of values that
    define the grid when \code{x} is a matrix.  \code{row.values} and
    \code{column.values} must have the same lengths as \code{nrow(x)}
    and \code{ncol(x)} respectively.  By default, row and column
    numbers. }
  \item{allow.multiple, outer, auto.key, prepanel, strip, groups, xlab,
    xlim, ylab, ylim, drop.unused.levels, lattice.options,
    default.scales, subset}{
    These arguments are documented in the help page for
    \code{\link{xyplot}}.  For the \code{cloud.table} method,
    \code{groups} must be a logical indicating whether the last
    dimension should be used as a grouping variable as opposed to a
    conditioning variable.  This is only relevant if the table has more
    than 2 dimensions.
  }
  \item{type}{
    type of display in \code{cloud} (see \code{\link{panel.3dscatter}}
    for details).  Defaults to \code{"h"} for the \code{matrix} method.
  }
  \item{aspect, panel.aspect}{
    Unlike other high level functions, \code{aspect} is taken to be a
    numeric vector of length 2, giving the relative aspects of the
    y-size/x-size and z-size/x-size of the enclosing cube.  The usual
    role of the \code{aspect} argument in determining the aspect ratio
    of the panel (see \code{\link{xyplot}} for details) is played by
    \code{panel.aspect}, except that it can only be a numeric value.

    For the \code{matrix} methods, the default y/x aspect is
    \code{ncol(x) / nrow(x)} and the z/x aspect is the smaller of the
    y/x aspect and 1.
  }
  \item{panel}{
    panel function used to create the display.  See
    \code{\link{panel.cloud}} for (non-trivial) details.
  }
  \item{default.prepanel}{
    Fallback prepanel function.  See \code{\link{xyplot}}.
  }
  \item{scales}{
    a list describing the scales.  As with other high level functions
    (see \code{\link{xyplot}} for details), this list can contain
    parameters in name=value form.  It can also contain components with
    the special names \code{x}, \code{y} and \code{z}, which can be
    similar lists with axis-specific values overriding the ones
    specified in \code{scales}. 

    The most common use for this argument is to set \code{arrows=FALSE},
    which causes tick marks and labels to be used instead of arrows
    being drawn (the default).  Both can be suppressed by
    \code{draw=FALSE}.  Another special component is \code{distance},
    which specifies the relative distance of the axis label from the
    bounding box.  If specified as a component of \code{scales} (as
    opposed to one of \code{scales$z} etc), this can be (and is recycled
    if not) a vector of length 3, specifying distances for the x, y and
    z labels respectively.

    Other components that work in the \code{scales} argument of
    \code{xyplot} etc. should also work here (as long as they make
    sense), including explicit specification of tick mark locations and
    labels.  (Not everything is implemented yet, but if you find
    something that should work but does not, feel free to bug the
    maintainer.)

    Note, however, that for these functions \code{scales} cannot contain
    information that is specific to particular panels.  If you really
    need that, consider using the \code{scales.3d} argument of
    \code{panel.cloud}.
  }
  \item{axs.default}{
    Unlike 2-D display functions, \code{cloud} does not expand the
    bounding box to slightly beyound the range of the data, even though
    it should.  This is primarily because this is the natural behaviour
    in \code{wireframe}, which uses the same code.  \code{axs.default}
    is intended to provide a different default for \code{cloud}.
    However, this feature has not yet been implemented.
  }
  \item{zlab}{
    Specifies a label describing the z variable in ways similar to
    \code{xlab} and \code{ylab} (i.e. \dQuote{grob}, character string,
    expression or list) in other high level functions.  Additionally, if
    \code{zlab} (and \code{xlab} and \code{ylab}) is a list, it can
    contain a component called \code{rot}, controlling the rotation for
    the label
  }
  \item{zlim}{
    limits for the z-axis. Similar to \code{xlim} and \code{ylim} in
    other high level functions
  }
  \item{drape}{
    logical, whether the wireframe is to be draped in color. If
    \code{TRUE}, the height of a facet is used to determine its color in
    a manner similar to the coloring scheme used in
    \code{\link{levelplot}}.  Otherwise, the background color is used to
    color the facets.  This argument is ignored if \code{shade = TRUE}
    (see \code{\link{panel.3dwire}}).
  }
  \item{at, col.regions, alpha.regions}{
    these arguments are analogous to those in
    \code{\link{levelplot}}. if \code{drape=TRUE}, \code{at} gives the
    vector of cutpoints where the colors change, and \code{col.regions}
    the vector of colors to be used in that case.  \code{alpha.regions}
    determines the alpha-transparency on supporting devices.  These are
    passed down to the panel function, and also used in the colorkey if
    appropriate.  The default for \code{col.regions} and
    \code{alpha.regions} is derived from the Trellis setting
    \code{"regions"}
  }
  \item{cuts}{
    if \code{at} is unspecified, the approximate number of cutpoints if
    \code{drape=TRUE}
  }
  \item{pretty}{
    whether automatic choice of cutpoints should be prettfied
  }
  \item{colorkey}{
    logical indicating whether a color key should be drawn
    alongside, or a list describing such a key.  See
    \code{\link{levelplot}} for details.
  }
  \item{\dots}{
    Any number of other arguments can be specified, and are passed to
    the panel function.  In particular, the arguments \code{distance},
    \code{perspective}, \code{screen} and \code{R.mat} are very
    important in determining the 3-D display.  The argument \code{shade}
    can be useful for \code{wireframe} calls, and controls shading of
    the rendered surface.  These arguments are described in detail in
    the help page for \code{\link{panel.cloud}}.

    Additionally, an argument called \code{zoom} may be specified, which
    should be a numeric scalar to be interpreted as a scale factor by
    which the projection is magnified.  This can be useful to get the
    variable names into the plot.  This argument is actually only used
    by the default prepanel function.
  }
}
\details{
  These functions produce three dimensional plots in each panel (as long
  as the default panel functions are used).  The orientation is obtained
  as follows: the data are scaled to fall within a bounding box that is
  contained in the [-0.5, 0.5] cube (even smaller for non-default values
  of \code{aspect}).  The viewing direction is given by a sequence of
  rotations specified by the \code{screen} argument, starting from the
  positive Z-axis. The viewing point (camera) is located at a distance
  of \code{1/distance} from the origin. If \code{perspective=FALSE},
  \code{distance} is set to 0 (i.e., the viewing point is at an infinite
  distance).

  \code{cloud} draws a 3-D Scatter Plot, while \code{wireframe} draws a
  3-D surface (usually evaluated on a grid). Multiple surfaces can be
  drawn by \code{wireframe} using the \code{groups} argument (although
  this is of limited use because the display is incorrect when the
  surfaces intersect). Specifying \code{groups} with \code{cloud}
  results in a \code{panel.superpose}-like effect (via
  \code{\link{panel.3dscatter}}).

  \code{wireframe} can optionally render the surface as being
  illuminated by a light source (no shadows though). Details can be
  found in the help page for \code{\link{panel.3dwire}}. Note that
  although arguments controlling these are actually arguments for the
  panel function, they can be supplied to \code{cloud} and
  \code{wireframe} directly.

  For single panel plots, \code{wireframe} can also plot parametrized
  3-D surfaces (i.e., functions of the form f(u,v) = (x(u,v), y(u,v),
  z(u,v)), where values of (u,v) lie on a rectangle. The simplest
  example of this sort of surface is a sphere parametrized by latitude
  and longitude. This can be achieved by calling \code{wireframe} with a
  formula \code{x} of the form \code{z~x*y}, where \code{x}, \code{y}
  and \code{z} are all matrices of the same dimension, representing the
  values of x(u,v), y(u,v) and z(u,v) evaluated on a discrete
  rectangular grid (the actual values of (u,v) are irrelevant).

  When this feature is used, the heights used to calculate \code{drape}
  colors or shading colors are no longer the \code{z} values, but the
  distances of \code{(x,y,z)} from the origin.

  Note that this feature does not work with \code{groups},
  \code{subscripts}, \code{subset}, etc. Conditioning variables are also
  not supported in this case.

  The algorithm for identifying which edges of the bounding box are
  \sQuote{behind} the points doesn't work in some extreme
  situations. Also, \code{\link{panel.cloud}} tries to figure out the
  optimal location of the arrows and axis labels automatically, but can
  fail on occasion (especially when the view is from \sQuote{below} the
  data). This can be manually controlled by the \code{scpos} argument in
  \code{\link{panel.cloud}}.

  These and all other high level Trellis functions have several other
  arguments in common. These are extensively documented only in the
  help page for \code{\link{xyplot}}, which should be consulted to learn
  more detailed usage.

}

\note{ There is a known problem with grouped \code{wireframe} displays
  when the (x, y) coordinates represented in the data do not represent
  the full evaluation grid.  The problem occurs whether the grouping is
  specified through the \code{groups} argument or through the formula
  interface, and currently causes memory access violations.  Depending
  on the circumstances, this is manifested either as a meaningless plot
  or a crash.  To work around the problem, it should be enough to have
  a row in the data frame for each grid point, with an \code{NA}
  response (\code{z}) in rows that were previously missing.  }

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
  \code{\link{Lattice}} for an overview of the package, as well as 
  \code{\link{xyplot}}, \code{\link{levelplot}},
  \code{\link{panel.cloud}}.

  For interaction, see \code{\link{panel.identify.cloud}}.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
## volcano  ## 87 x 61 matrix
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$gr + g$y^2) * g$gr)
wireframe(z ~ x * y, data = g, groups = gr,
          scales = list(arrows = FALSE),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -60))

cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## cloud.table

cloud(prop.table(Titanic, margin = 1:3),
      type = c("p", "h"), strip = strip.custom(strip.names = TRUE),
      scales = list(arrows = FALSE, distance = 2), panel.aspect = 0.7,
      zlab = "Proportion")[, 1]

## transparent axes

par.set <-
    list(axis.line = list(col = "transparent"),
         clip = list(panel = "off"))
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
            data = iris, cex = .8, 
            groups = Species, 
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 3),
            par.settings = par.set,
            scales = list(col = "black")),
      split = c(1,1,2,1), more = TRUE)
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, cex = .8, 
            groups = Species,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 0),
            par.settings = par.set,
            scales = list(col = "black")),
      split = c(2,1,2,1))

}
\keyword{hplot}
