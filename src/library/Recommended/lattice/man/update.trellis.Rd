\name{C_06_update.trellis}
\alias{update.trellis}
\alias{[.trellis}
\alias{t.trellis}
\alias{trellis.last.object}
\title{Retrieve and Update Trellis Object}
\description{
  Update method for objects of class \code{"trellis"}, and a way to retrieve
  the last printed trellis object (that was saved).
}

\usage{
\method{update}{trellis}(object,
       panel,
       aspect,
       as.table,
       between,
       key,
       auto.key,
       legend,
       layout,
       main,
       page,
       par.strip.text,
       prepanel,
       scales,
       skip,
       strip,
       strip.left,
       sub,
       xlab,
       ylab,
       xlab.top,
       ylab.right,
       xlim,
       ylim,
       xscale.components,
       yscale.components,
       axis,
       par.settings,
       plot.args,
       lattice.options,
       index.cond,
       perm.cond,
       \dots)

\method{t}{trellis}(x)

\method{[}{trellis}(x, i, j, \dots, drop = FALSE)

trellis.last.object(\dots, prefix)

}
\arguments{

  \item{object, x}{
    The object to be updated, of class \code{"trellis"}.
  }
  \item{i, j}{ indices to be used.  Names are not currently allowed. }
  \item{drop}{
    logical, whether dimensions with only one level are to be dropped.
    Currently ignored, behaves as if it were \code{FALSE}.
  }
  % \item{warn}{
  %   logical, whether to warn when no plot is saved.
  % }

  \item{panel, aspect, as.table, between, key, auto.key, legend, layout,
    main, page, par.strip.text, prepanel, scales, skip, strip,
    strip.left, sub, xlab, ylab, xlab.top, ylab.right, xlim, ylim,
    xscale.components, yscale.components, axis, par.settings, plot.args,
    lattice.options, index.cond, perm.cond, \dots}{
    arguments that will be used to update \code{object}. See details
    below. 
  }

  \item{prefix}{
    A character string acting as a prefix identifying the plot of a
    \code{"trellis"} object.  Only relevant when a particular page is
    occupied by more than one plot.  Defaults to the value appropriate
    for the last \code{"trellis"} object printed.  See
    \code{\link{trellis.focus}}.
  }
}


\details{

  All high level lattice functions such as \code{xyplot} produce an
  object of (S3) class \code{"trellis"}, which is usually displayed by its
  \code{print} method. However, the object itself can be manipulated and
  modified to a large extent using the \code{update} method, and then
  re-displayed as needed.

  Most arguments to high level functions can also be supplied to the
  \code{update} method as well, with some exceptions. Generally
  speaking, anything that would needs to change the data within each
  panel is a no-no (this includes the \code{formula, data, groups,
    subscripts} and \code{subset}). Everything else is technically game,
  though might not be implemented yet. If you find something missing
  that you wish to have, feel free to make a request.

  Not all arguments accepted by a Lattice function are processed by
  \code{update}, but the ones listed above should work.  The purpose of
  these arguments are described in the help page for
  \code{\link{xyplot}}.  Any other argument is added to the list of
  arguments to be passed to the \code{panel} function. Because of their
  somewhat special nature, updates to objects produced by \code{cloud}
  and \code{wireframe} do not work very well yet.

  The \code{"["} method is a convenient shortcut for updating
  \code{index.cond}.  The \code{t} method is a convenient shortcut for
  updating \code{perm.cond} in the special (but frequent) case where
  there are exactly two conditioning variables, when it has the effect
  of switching (\sQuote{transposing}) their order.

  The print method for \code{"trellis"} objects optionally saves the
  object after printing it.  If this feature is enabled,
  \code{trellis.last.object} can retrieve it.  By default, the last
  object plotted is retrieved, but if multiple objects are plotted on
  the current page, then others can be retrieved using the appropriate
  \code{prefix} argument.  If \code{\link{trellis.last.object}} is
  called with arguments, these are used to update the retrieved object
  before returning it.

}

% WAS: \dontrun{
% trellis.last.object(xlab = "Year")
% }

\examples{
spots <- by(sunspots, gl(235, 12, labels = 1749:1983), mean)
old.options <- lattice.options(save.object = TRUE)
xyplot(spots ~ 1749:1983, xlab = "", type = "l",
       scales = list(x = list(alternating = 2)),
       main = "Average Yearly Sunspots")
update(trellis.last.object(), aspect = "xy")
trellis.last.object(xlab = "Year")
lattice.options(old.options)
}

\value{
  An object of class \code{trellis}, by default plotted by
  \code{print.trellis}.  \code{trellis.last.object} returns
  \code{NULL} is no saved object is available.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{trellis.object}}, \code{\link{Lattice}},
  \code{\link{xyplot}}
}
\keyword{dplot}
