\name{F_1_panel.cloud}
\alias{panel.cloud}
\alias{panel.wireframe}
\alias{panel.3dscatter}
\alias{panel.3dwire}
\title{ Default Panel Function for cloud }
\description{
  These are default panel functions controlling \code{cloud} and
  \code{wireframe} displays.
}

\usage{
panel.cloud(x, y, subscripts, z,
            groups = NULL,
            perspective = TRUE,
            distance = if (perspective) 0.2 else 0, 
            xlim, ylim, zlim,
            panel.3d.cloud = "panel.3dscatter",
            panel.3d.wireframe = "panel.3dwire",
            screen = list(z = 40, x = -60),
            R.mat = diag(4), aspect = c(1, 1),
            par.box = NULL,
            xlab, ylab, zlab,
            xlab.default, ylab.default, zlab.default,
            scales.3d,
            proportion = 0.6,
            wireframe = FALSE,
            scpos,
            \dots,
            at,
            identifier = "cloud")
panel.wireframe(\dots)
panel.3dscatter(x, y, z, rot.mat, distance,
                groups, type = "p",
                xlim, ylim, zlim,
                xlim.scaled, ylim.scaled, zlim.scaled,
                zero.scaled,
                col, col.point, col.line,
                lty, lwd, cex, pch,
                cross, \dots, .scale = FALSE, subscripts,
                identifier = "3dscatter")
panel.3dwire(x, y, z, rot.mat = diag(4), distance,
             shade = FALSE,
             shade.colors.palette = trellis.par.get("shade.colors")$palette,
             light.source = c(0, 0, 1000),
             xlim, ylim, zlim, 
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             col = if (shade) "transparent" else "black",
             lty = 1, lwd = 1,
             alpha,
             col.groups = superpose.polygon$col,
             polynum = 100,
             \dots,
             .scale = FALSE,
             drape = FALSE,
             at,
             col.regions = regions$col,
             alpha.regions = regions$alpha,
             identifier = "3dwire")
}
\arguments{
  \item{x, y, z}{
    numeric (or possibly factors) vectors representing the data to be
    displayed. The interpretation depends on the context.  For
    \code{panel.cloud} these are essentially the same as the data passed
    to the high level plot (except if \code{formula} was a matrix, the
    appropriate \code{x} and \code{y} vectors are generated).  By the
    time they are passed to \code{panel.3dscatter} and
    \code{panel.3dwire}, they have been appropriately subsetted (using
    \code{subscripts}) and scaled (to lie inside a bounding box, usually
    the [-0.5, 0.5] cube).

    Further, for \code{panel.3dwire}, \code{x} and \code{y} are shorter
    than \code{z} and represent the sorted locations defining a
    rectangular grid. Also in this case, \code{z} may be a matrix if the
    display is grouped, with each column representing one surface.

    In \code{panel.cloud} (called from \code{wireframe}) and
    \code{panel.3dwire}, \code{x}, \code{y} and \code{z} could also be
    matrices (of the same dimension) when they represent a 3-D surface
    parametrized on a 2-D grid.
  }
  \item{subscripts}{
    index specifying which points to draw. The same \code{x}, \code{y}
    and \code{z} values (representing the whole data) are passed to
    \code{panel.cloud} for each panel.  \code{subscripts} specifies the
    subset of rows to be used for the particular panel.
  }
  \item{groups}{
    specification of a grouping variable, passed down from the high
    level functions.
  }

  \item{perspective}{
    logical, whether to plot a perspective view. Setting this to
    \code{FALSE} is equivalent to setting \code{distance} to 0
  }
  \item{distance}{
    numeric, between 0 and 1, controls amount of perspective. The
    distance of the viewing point from the origin (in the transformed
    coordinate system) is \code{1 / distance}. This is described in a
    little more detail in the documentation for \code{\link{cloud}}
  }

  \item{screen}{
    A list determining the sequence of rotations to be applied to the
    data before being plotted. The initial position starts with the
    viewing point along the positive z-axis, and the x and y axes in the
    usual position. Each component of the list should be named one of
    \code{"x"}, \code{"y"} or \code{"z"} (repetitions are allowed), with
    their values indicating the amount of rotation about that axis in
    degrees.
  }
  \item{R.mat}{
    initial rotation matrix in homogeneous coordinates, to be applied to
    the data before \code{screen} rotates the view further.
  }
  \item{par.box}{
    graphical parameters for box, namely, col, lty and lwd. By default
    obtained from the parameter \code{box.3d}.
  }

  \item{xlim, ylim, zlim}{
    limits for the respective axes. As with other lattice functions,
    these could each be a numeric 2-vector or a character vector
    indicating levels of a factor.
  }
  \item{panel.3d.cloud, panel.3d.wireframe}{
    functions that draw the data-driven part of the plot (as opposed to
    the bounding box and scales) in \code{cloud} and
    \code{wireframe}. This function is called after the \sQuote{back} of
    the bounding box is drawn, but before the \sQuote{front} is drawn.

    Any user-defined custom display would probably want to change these
    functions. The intention is to pass as much information to this
    function as might be useful (not all of which are used by the
    defaults). In particular, these functions can expect arguments
    called \code{xlim}, \code{ylim}, \code{zlim} which give the bounding
    box ranges in the original data scale and \code{xlim.scaled},
    \code{ylim.scaled}, \code{zlim.scaled} which give the bounding box
    ranges in the transformed scale. More arguments can be considered on
    request.
  }
  \item{aspect}{ aspect as in \code{cloud} }
  \item{xlab, ylab, zlab}{
    Labels, have to be lists. Typically the user will not manipulate
    these, but instead control this via arguments to \code{cloud}
    directly.
  }
  \item{xlab.default}{ for internal use  }
  \item{ylab.default}{ for internal use  }
  \item{zlab.default}{ for internal use  }
  \item{scales.3d}{ list defining the scales }
  \item{proportion}{
    numeric scalar, gives the length of arrows as a proportion of the
    sides
  }
  \item{scpos}{
    A list with three components x, y and z (each a scalar integer),
    describing which of the 12 sides of the cube the scales should be
    drawn. The defaults should be OK. Valid values are x: 1, 3, 9, 11;
    y: 8, 5, 7, 6 and z: 4, 2, 10, 12. (See comments in the source code
    of \code{panel.cloud} to see the details of this enumeration.)
  }
  \item{wireframe}{logical, indicating whether this is a wireframe plot}
  \item{drape}{
    logical, whether the facets will be colored by height, in a manner
    similar to \code{levelplot}.  This is ignored if \code{shade=TRUE}.
  }
  \item{at, col.regions, alpha.regions}{
    deals with specification of colors when \code{drape = TRUE} in
    \code{\link{wireframe}}.  \code{at} can be a numeric vector,
    \code{col.regions} a vector of colors, and \code{alpha.regions} a
    numeric scalar controlling transparency.  The resulting behaviour is
    similar to \code{\link{levelplot}}, \code{at} giving the breakpoints
    along the z-axis where colors change, and the other two determining
    the colors of the facets that fall in between.
  }
  \item{rot.mat}{
    4x4 transformation matrix in homogeneous coordinates. This gives the
    rotation matrix combining the \code{screen} and \code{R.mat}
    arguments to \code{\link{panel.cloud}}
  }
  \item{type}{
    Character vector, specifying type of cloud plot. Can include one or
    more of \code{"p"}, \code{"l"}, \code{"h"} or \code{"b"}. \code{"p"}
    and \code{"l"} mean \sQuote{points} and \sQuote{lines} respectively,
    and \code{"b"} means \sQuote{both}.  \code{"h"} stands for
    \sQuote{histogram}, and causes a line to be drawn from each point to
    the X-Y plane (i.e., the plane representing \code{z = 0}), or the
    lower (or upper) bounding box face, whichever is closer.
  }
  \item{xlim.scaled, ylim.scaled, zlim.scaled}{
     axis limits (after being scaled to the bounding box) 
  }
  \item{zero.scaled}{
    z-axis location (after being scaled to the bounding box) of the X-Y
    plane in the original data scale, to which lines will be dropped (if
    within range) from each point when \code{type = "h"}
  }
  \item{cross}{
    logical, defaults to \code{TRUE} if \code{pch = "+"}.
    \code{panel.3dscatter} can represent each point by a 3d
    \sQuote{cross} of sorts (it's much easier to understand looking at
    an example than from a description). This is different from the
    usual \code{pch} argument, and reflects the depth of the points and
    the orientation of the axes. This argument indicates whether this
    feature will be used.

    This is useful for two reasons. It can be set to \code{FALSE} to use
    \code{"+"} as the plotting character in the regular sense. It can
    also be used to force this feature in grouped displays.
  }
  \item{shade}{
    logical, indicating whether the surface is to be colored using an
    illumination model with a single light source
  }
  \item{shade.colors.palette}{
    a function (or the name of one) that is supposed to calculate the
    color of a facet when shading is being used. Three pieces of
    information are available to the function: first, the cosine of the
    angle between the incident light ray and the normal to the surface
    (representing foreshortening); second, the cosine of half the angle
    between the reflected ray and the viewing direction (useful for
    non-Lambertian surfaces); and third, the scaled (average) height of
    that particular facet with respect to the total plot z-axis limits.

    All three numbers should be between 0 and 1. The
    \code{shade.colors.palette} function should return a valid
    color. The default function is obtained from the trellis settings.
  }
  \item{light.source}{
    a 3-vector representing (in cartesian coordinates) the light
    source. This is relative to the viewing point being (0, 0,
    1/distance) (along the positive z-axis), keeping in mind that all
    observations are bounded within the [-0.5, 0.5] cube
  }
  \item{polynum}{
    quadrilateral faces are drawn in batches of \code{polynum} at a
    time. Drawing too few at a time increases the total number of calls
    to the underlying \code{grid.polygon} function, which affects
    speed. Trying to draw too many at once may be unnecessarily memory
    intensive. This argument controls the trade-off.
  }
  \item{col.groups}{colors for different groups}
  \item{col, col.point, col.line, lty, lwd, cex, pch, alpha}{
    Graphical parameters.  Some other arguments (such as \code{lex} for
    line width) may also be passed through the \code{\dots} argument.
  }
  \item{\dots}{ other parameters, passed down when appropriate}
  \item{.scale}{
    Logical flag, indicating whether \code{x}, \code{y}, and \code{z}
    should be assumed to be in the original data scale and hence scaled
    before being plotted.  \code{x}, \code{y}, and \code{z} are usually
    already scaled.  However, setting \code{.scale=TRUE} may be helpful
    for calls to \code{panel.3dscatter} and \code{panel.3dwire} in
    user-supplied panel functions.
  }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\details{
  These functions together are responsible for the content drawn inside
  each panel in \code{cloud} and \code{wireframe}.
  \code{panel.wireframe} is a wrapper to \code{panel.cloud}, which does
  the actual work.

  \code{panel.cloud} is responsible for drawing the content that does
  not depend on the data, namely, the bounding box, the arrows/scales,
  etc. At some point, depending on whether \code{wireframe} is TRUE, it
  calls either \code{panel.3d.wireframe} or \code{panel.3d.cloud}, which
  draws the data-driven part of the plot.

  The arguments accepted by these two functions are different, since
  they have essentially different purposes. For cloud, the data is
  unstructured, and \code{x}, \code{y} and \code{z} are all passed to
  the \code{panel.3d.cloud} function. For wireframe, on the other hand,
  \code{x} and \code{y} are increasing vectors with unique values,
  defining a rectangular grid. \code{z} must be a matrix with
  \code{length(x) * length(y)} rows, and as many columns as the number
  of groups.

  \code{panel.3dscatter} is the default \code{panel.3d.cloud} function.
  It has a \code{type} argument similar to \code{\link{panel.xyplot}},
  and supports grouped displays. It tries to honour depth ordering,
  i.e., points and lines closer to the camera are drawn later,
  overplotting more distant ones. (Of course there is no absolute
  ordering for line segments, so an ad hoc ordering is used. There is no
  hidden point removal.)

  \code{panel.3dwire} is the default \code{panel.3d.wireframe}
  function. It calculates polygons corresponding to the facets one by
  one, but waits till it has collected information about \code{polynum}
  facets, and draws them all at once. This avoids the overhead of
  drawing \code{grid.polygon} repeatedly, speeding up the rendering
  considerably. If \code{shade = TRUE}, these attempt to color the
  surface as being illuminated from a light source at
  \code{light.source}. \code{palette.shade} is a simple function that
  provides the deafult shading colors

  Multiple surfaces are drawn if \code{groups} is non-null in the call
  to \code{wireframe}, however, the algorithm is not sophisticated
  enough to render intersecting surfaces correctly.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{cloud}}, \code{\link{utilities.3d}}}
\keyword{dplot}
