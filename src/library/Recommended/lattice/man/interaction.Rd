\name{E_interaction}
\alias{panel.identify}
\alias{panel.identify.qqmath}
\alias{panel.identify.cloud}
\alias{panel.link.splom}
\alias{panel.brush.splom}
\alias{trellis.focus}
\alias{trellis.unfocus}
\alias{trellis.switchFocus}
\alias{trellis.panelArgs}
\alias{trellis.vpname}
\alias{trellis.grobname}
\concept{interaction}
\concept{augment}
\title{Functions to Interact with Lattice Plots}
\description{
  The classic Trellis paradigm is to plot the whole object at once,
  without the possibility of interacting with it afterwards.  However,
  by keeping track of the grid viewports where the panels and strips are
  drawn, it is possible to go back to them afterwards and enhance them
  one panel at a time.  These functions provide convenient interfaces to
  help in this.  Note that these are still experimental and the exact
  details may change in future.
}
\usage{
panel.identify(x, y = NULL,
               subscripts = seq_along(x),
               labels = subscripts, 
               n = length(x), offset = 0.5,
               threshold = 18, ## in points, roughly 0.25 inches
               panel.args = trellis.panelArgs(),
               \dots)
panel.identify.qqmath(x, distribution, groups, subscripts, labels,
                      panel.args = trellis.panelArgs(),
                      \dots)
panel.identify.cloud(x, y, z, subscripts,
                     perspective, distance, 
                     xlim, ylim, zlim,
                     screen, R.mat, aspect, scales.3d,
                     \dots,
                     panel.3d.identify,
                     n = length(subscripts),
                     offset = 0.5,
                     threshold = 18,
                     labels = subscripts,
                     panel.args = trellis.panelArgs())
panel.link.splom(threshold = 18, verbose = getOption("verbose"), \dots)
panel.brush.splom(threshold = 18, verbose = getOption("verbose"), \dots)


trellis.vpname(name = c("position", "split", "split.location", "toplevel",
                        "figure", "panel", "strip", "strip.left",
                        "legend", "legend.region", "main", "sub",
                        "xlab", "ylab", "xlab.top", "ylab.right", "page"),
               column, row,
               side = c("left", "top", "right", "bottom", "inside"),
               clip.off = FALSE, prefix)
trellis.grobname(name,
                 type = c("", "panel", "strip", "strip.left",
                          "key", "colorkey"),
                 group = 0,
                 which.given = lattice.getStatus("current.which.given",
                                                 prefix = prefix),
                 which.panel = lattice.getStatus("current.which.panel",
                                                 prefix = prefix),
                 column = lattice.getStatus("current.focus.column",
                                            prefix = prefix),
                 row = lattice.getStatus("current.focus.row",
                                         prefix = prefix),
                 prefix = lattice.getStatus("current.prefix"))
trellis.focus(name, column, row, side, clip.off,
              highlight = interactive(), \dots, prefix,
              guess = TRUE, verbose = getOption("verbose"))
trellis.switchFocus(name, side, clip.off, highlight, \dots, prefix)
trellis.unfocus()
trellis.panelArgs(x, packet.number)
}

\arguments{
  \item{x, y, z}{ variables defining the contents of the panel.  In the
    case of \code{trellis.panelArgs}, a \code{"trellis"} object. }
  \item{n}{
    the number of points to identify by default (overridden by a right
    click)
  }
  \item{subscripts}{
    an optional vector of integer indices associated with each point.
    See details below.
  }
  \item{labels}{
    an optional vector of labels associated with each point.  Defaults
    to \code{subscripts}
  }
  \item{distribution, groups}{ typical panel arguments of
    \code{\link{panel.qqmath}}.  These will usually be obtained from
    \code{panel.args}
  }
  \item{offset}{
    the labels are printed either below, above, to the left or to the
    right of the identified point, depending on the relative location of
    the mouse click. The \code{offset} specifies (in "char" units) how
    far from the identified point the labels should be printed.
  }
  \item{threshold}{
    threshold in grid's \code{"points"} units. Points further than these
    from the mouse click position are not considered
  }
  \item{panel.args}{
    list that contains components names \code{x} (and usually \code{y}),
    to be used if \code{x} is missing.  Typically, when called after
    \code{trellis.focus}, this would appropriately be the arguments
    passed to that panel.
  }
  \item{perspective, distance, xlim, ylim, zlim, screen, R.mat, aspect,
    scales.3d}{
    arguments as passed to \code{\link{panel.cloud}}.  These are
    required to recompute the relevant three-dimensional projections in
    \code{panel.identify.cloud}.
  }
  \item{panel.3d.identify}{
    the function that is responsible for the actual interaction once the
    data rescaling and rotation computations have been done.  By
    default, an internal function similar to \code{panel.identify} is
    used.
  }
  \item{name}{

    A character string indicating which viewport or grob we are looking
    for.  Although these do not necessarily provide access to all
    viewports and grobs created by a lattice plot, they cover most of
    the ones that end-users may find interesting.

    \code{trellis.vpname} and \code{trellis.focus} deal with viewport
    names only, and only accept the values explicitly listed above.
    \code{trellis.grobname} is meant to create names for grobs, and can
    currently accept any value.

    If \code{name}, as well as \code{column} and \code{row} is missing
    in a call to \code{trellis.focus}, the user can click inside a panel
    (or an associated strip) to focus on that panel.  Note however that
    this assumes equal width and height for each panel, and may not work
    when this is not true.

    When \code{name} is \code{"panel"}, \code{"strip"}, or
    \code{"strip.left"}, \code{column} and \code{row} must also be
    specified.  When \code{name} is \code{"legend"}, \code{side} must
    also be specified.

  }
  \item{column, row}{
    integers, indicating position of the panel or strip that should be
    assigned focus in the Trellis layout. Rows are usually calculated
    from the bottom up, unless the plot was created with
    \code{as.table=TRUE}
  }
  \item{guess}{
    logical.  If \code{TRUE}, and the display has only one panel, that
    panel will be automatically selected by a call to
    \code{trellis.focus}.
  }
  \item{side}{
    character string, relevant only for legends (i.e., when
    \code{name="legend"}), indicating their position.  Partial specification
    is allowed, as long as it is unambiguous.
  }
  \item{clip.off}{
    logical, whether clipping should be off, relevant when \code{name}
    is \code{"panel"} or \code{"strip"}.  This is necessary if axes are
    to be drawn outside the panel or strip.  Note that setting
    \code{clip.off=FALSE} does not necessarily mean that clipping is on;
    that is determined by conditions in effect during printing.

  }
  \item{type}{
    A character string specifying whether the grob is specific to
    a particular panel or strip.

    When \code{type} is \code{"panel"}, \code{"strip"}, or
    \code{"strip.left"}, information about the panel is added to the
    grob name.
  }
  \item{group}{
    An integer specifying whether the grob is specific to a particular
    group within the plot.

    When \code{group} is greater than zero, information about the
    group is added to the grob name.
  }
  \item{which.given, which.panel}{
    integers, indicating which conditional variable is being
    represented (within a strip) and the current levels of
    the conditional variables.

    When \code{which.panel} has length greater than 1,
    and the \code{type} is \code{"strip"} or \code{"strip.left"},
    information about the conditional variable is added to
    the grob name.
  }
  \item{prefix}{
    A character string acting as a prefix identifying the plot of a
    \code{"trellis"} object, primarily used to distinguish otherwise
    equivalent viewports in different plots. This only becomes relevant
    when a particular page is occupied by more than one plot.  Defaults
    to the value appropriate for the last \code{"trellis"} object
    printed, as determined by the \code{prefix} argument in
    \code{\link{print.trellis}}.

    Users should not usually need to supply a value for this argument
    except to interact with an existing plot other than the one plotted
    last.

    For \code{switchFocus}, ignored except when it does not match the
    prefix of the currently active plot, in which case an error occurs.
  }
  \item{highlight}{
    logical, whether the viewport being assigned focus should be
    highlighted.  For \code{trellis.focus}, the default is \code{TRUE}
    in interactive mode, and \code{trellis.switchFocus} by default
    preserves the setting currently active.
  }
  \item{packet.number}{
    integer, which panel to get data from.  See
    \code{\link{packet.number}} for details on how this is calculated
  }
  \item{verbose}{ whether details  will be printed }
  \item{\dots}{
    For \code{panel.identify.qqmath}, extra parameters are passed on to
    \code{panel.identify}.  For \code{panel.identify}, extra arguments
    are treated as graphical parameters and are used for labelling.  For
    \code{trellis.focus} and \code{trellis.switchFocus}, these are used
    (in combination with \code{\link{lattice.options}}) for highlighting
    the chosen viewport if so requested.  Graphical parameters can be
    supplied for \code{panel.link.splom}.
  }
}

\details{
  \code{panel.identify} is similar to \code{\link{identify}}.  When
  called, it waits for the user to identify points (in the panel being
  drawn) via mouse clicks.  Clicks other than left-clicks terminate the
  procedure.  Although it is possible to call it as part of the panel
  function, it is more typical to use it to identify points after
  plotting the whole object, in which case a call to
  \code{trellis.focus} first is necessary.

  \code{panel.link.splom} is meant for use with \code{\link{splom}},
  and requires a panel to be chosen using \code{trellis.focus} before it
  is called.  Clicking on a point causes that and the corresponding
  proections in other pairwise scatter plots to be highlighted.
  \code{panel.brush.splom} is a (misnamed) alias for
  \code{panel.link.splom}, retained for back-compatibility.

  \code{panel.identify.qqmath} is a specialized wrapper meant for use
  with the display produced by \code{\link{qqmath}}.
  \code{panel.identify.qqmath} is a specialized wrapper meant for use
  with the display produced by \code{\link{cloud}}.  It would be unusual
  to call them except in a context where default panel function
  arguments are available through \code{trellis.panelArgs} (see below).
  
  One way in which \code{panel.identify} etc. are different from
  \code{\link{identify}} is in how it uses the \code{subscripts}
  argument.  In general, when one identifies points in a panel, one
  wants to identify the origin in the data frame used to produce the
  plot, and not within that particular panel.  This information is
  available to the panel function, but only in certain situations.  One
  way to ensure that \code{subscripts} is available is to specify
  \code{subscripts = TRUE} in the high level call such as \code{xyplot}.
  If \code{subscripts} is not explicitly specified in the call to
  \code{panel.identify}, but is available in \code{panel.args}, then
  those values will be used.  Otherwise, they default to
  \code{seq_along(x)}.  In either case, the final return value will be
  the subscripts that were marked.

  The process of printing (plotting) a Trellis object builds up a grid
  layout with named viewports which can then be accessed to modify the
  plot further.  While full flexibility can only be obtained by using
  grid functions directly, a few lattice functions are available for the
  more common tasks.

  \code{trellis.focus} can be used to move to a particular panel or
  strip, identified by its position in the array of panels.  It can also
  be used to focus on the viewport corresponding to one of the labels or
  a legend, though such usage would be less useful.  The exact
  viewport is determined by the \code{name} along with the other
  arguments, not all of which are relevant for all names.  Note that
  when more than one object is plotted on a page, \code{trellis.focus}
  will always go to the plot that was created last.  For more
  flexibility, use grid functions directly (see note below).

  After a successful call to \code{trellis.focus}, the desired viewport
  (typically panel or strip area) will be made the \sQuote{current}
  viewport (plotting area), which can then be enhanced by calls to
  standard lattice panel functions as well as grid functions.

  It is quite common to have the layout of panels chosen when a
  \code{"trellis"} object is drawn, and not before then.  Information on
  the layout (specifically, how many rows and columns, and which packet
  belongs in which position in this layout) is retained for the last
  \code{"trellis"} object plotted, and is available through
  \code{trellis.currentLayout}.

  \code{trellis.unfocus} unsets the focus, and makes the top level
  viewport the current viewport.

  \code{trellis.switchFocus} is a convenience function to switch from
  one viewport to another, while preserving the current \code{row} and
  \code{column}.  Although the rows and columns only make sense for
  panels and strips, they would be preserved even when the user switches
  to some other viewport (where row/column is irrelevant) and then
  switches back.
  
  Once a panel or strip is in focus, \code{trellis.panelArgs} can be
  used to retrieve the arguments that were available to the panel
  function at that position.  In this case, it can be called without
  arguments as \preformatted{trellis.panelArgs()}

  This usage is also allowed when a \code{"trellis"} object is being
  printed, e.g. inside the panel functions or the axis function (but not
  inside the prepanel function).  \code{trellis.panelArgs} can also
  retrieve the panel arguments from any \code{"trellis"} object.  Note
  that for this usage, one needs to specify the \code{packet.number} (as
  described under the \code{panel} entry in \code{\link{xyplot}}) and
  not the position in the layout, because a layout determines the panel
  only \bold{after} the object has been printed.

  It is usually not necessary to call \code{trellis.vpname} and
  \code{trellis.grobname} directly.  However, they can be useful in
  generating appropriate names in a portable way when using grid
  functions to interact with the plots directly, as described in the
  note below.

}


\value{
  \code{panel.identify} returns an integer vector containing the
  subscripts of the identified points (see details above).  The
  equivalent of \code{identify} with \code{pos=TRUE} is not yet
  implemented, but can be considered for addition if requested.

  \code{trellis.panelArgs} returns a named list of arguments that were
  available to the panel function for the chosen panel.

  \code{trellis.vpname} and \code{trellis.grobname} return character
  strings.

  \code{trellis.focus} has a meaningful return value only if it has been
  used to focus on a panel interactively, in which case the return value
  is a list with components \code{col} and \code{row} giving the column
  and row positions respectively of the chosen panel, unless the choice
  was cancelled (by a right click), in which case the return value is
  \code{NULL}.  If click was outside a panel, both \code{col} and
  \code{row} are set to 0.
}
  
\note{

  The viewports created by lattice are accessible to the user through
  \code{trellis.focus} as described above.  Functions from the grid
  package can also be used directly.  For example,
  \code{\link[grid:current.viewport]{current.vpTree}} can be used to
  inspect the current viewport tree and
  \code{\link[grid:viewports]{seekViewport}} or
  \code{\link[grid:viewports]{downViewport}} can be used to navigate to
  these viewports.  For such usage, \code{trellis.vpname} and
  \code{trellis.grobname} provides a portable way to access the
  appropriate viewports and grobs by name.

}

\examples{

\dontrun{
xyplot(1:10 ~ 1:10)
trellis.focus("panel", 1, 1)
panel.identify()
}

xyplot(Petal.Length ~ Sepal.Length | Species, iris, layout = c(2, 2))
Sys.sleep(1)

trellis.focus("panel", 1, 1)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()

trellis.focus("panel", 2, 1)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()

trellis.focus("panel", 1, 2)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()


## choosing loess smoothing parameter

p <- xyplot(dist ~ speed, cars)

panel.loessresid <-
    function(x = panel.args$x,
             y = panel.args$y,
             span,
             panel.args = trellis.panelArgs())
{
    fm <- loess(y ~ x, span = span)
    xgrid <- do.breaks(current.panel.limits()$xlim, 50)
    ygrid <- predict(fm, newdata = data.frame(x = xgrid))
    panel.lines(xgrid, ygrid)
    pred <- predict(fm)
    ## center residuals so that they fall inside panel
    resids <- y - pred + mean(y)
    fm.resid <- loess.smooth(x, resids, span = span)
    ##panel.points(x, resids, col = 1, pch = 4)
    panel.lines(fm.resid, col = 1)
}


spans <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
update(p, index.cond = list(rep(1, length(spans))))
panel.locs <- trellis.currentLayout()

i <- 1

for (row in 1:nrow(panel.locs))
    for (column in 1:ncol(panel.locs))
    if (panel.locs[row, column] > 0)
{
    trellis.focus("panel", row = row, column = column,
                  highlight = FALSE)
    panel.loessresid(span = spans[i])
    grid::grid.text(paste("span = ", spans[i]),
                    x = 0.25,
                    y = 0.75,
                    default.units = "npc")
    trellis.unfocus()
    i <- i + 1
}


}

\seealso{
  \code{\link{identify}}, \code{\link{Lattice}},
  \code{\link{print.trellis}}, \code{\link{trellis.currentLayout}},
  \code{\link[grid:current.viewport]{current.vpTree}},
  \code{\link[grid:viewports]{viewports}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}.  Felix
  Andrews provided initial implementations of
  \code{panel.identify.qqmath} and support for focusing on panels
  interctively.
}

\keyword{dplot}


