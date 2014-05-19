\name{C_02_trellis.par.get}
\alias{trellis.par.get}
\alias{trellis.par.set}
\alias{show.settings}
\title{ Graphical Parameters for Trellis Displays }
\description{
  Functions used to query, display and modify graphical parameters for
  fine control of Trellis displays.  Modifications are made to the
  settings for the currently active device only.
}
\usage{
trellis.par.set(name, value, \dots, theme, warn = TRUE, strict = FALSE)
trellis.par.get(name = NULL)
show.settings(x = NULL)
}
% trellis.par.set(name, value, warn)
% trellis.par.set(theme)
% trellis.par.set(\dots)
\arguments{
  \item{name}{
    A character string giving the name of a component.  If unspecified
    in \code{trellis.par.get()}, the return value is a named list
    containing all the current settings (this can be used to get the
    valid values for \code{name}).
  }
  \item{value}{
    a list giving the desired value of the component. Components that
    are already defined as part of the current settings but are not
    mentioned in \code{value} will remain unchanged.
  }
  \item{theme}{
    a list decribing how to change the settings, similar to what is
    returned by \code{trellis.par.get()}.  This is purely for
    convenience, allowing multiple calls to \code{trellis.par.set} to be
    condensed into one.  The name of each component must be a valid
    \code{name} as described above, with the corresponding value a valid
    \code{value} as described above.

    As in \code{\link{trellis.device}}, \code{theme} can also be a
    function that produces such a list when called.  The function name
    can be supplied as a quoted string.
  }
  \item{\dots}{
    Multiple settings can be specified in \code{name = value} form.
    Equivalent to calling with \code{theme = list(\dots)}
  }
  \item{warn}{
    A logical flag, indicating whether a warning should be issued when
    \code{trellis.par.get} is called when no graphics device is open.
  }
  \item{strict}{
    Usually a logical flag, indicating whether the \code{value} should
    be interpreted strictly.  Usually, assignment of value to the
    corresponding named component is fuzzy in the sense that
    sub-components that are absent from \code{value} but not currently
    \code{NULL} are retained.  By specifying \code{strict = TRUE}, such
    values will be removed.

    An even stricter interpretation is allowed by specifying
    \code{strict} as a numeric value larger than \code{1}.  In that
    case, top-level components not specified in the call will also be
    removed.  This is primarily for internal use.
  }
  \item{x}{
    optional list of components that change the settings (any valid
    value of \code{theme}).  These are used to modify the current
    settings (obtained by \code{trellis.par.get}) before they are
    displayed.
  }
}
\details{
  The various graphical parameters (color, line type, background etc)
  that control the look and feel of Trellis displays are highly
  customizable. Also, R can produce graphics on a number of devices, and
  it is expected that a different set of parameters would be more suited
  to different devices. These parameters are stored internally in a
  variable named \code{lattice.theme}, which is a list whose components
  define settings for particular devices. The components are idenified
  by the name of the device they represent (as obtained by
  \code{.Device}), and are created as and when new devices are opened
  for the first time using \code{trellis.device} (or Lattice plots are
  drawn on a device for the first time in that session).

  The initial settings for each device defaults to values appropriate
  for that device. In practice, this boils down to three distinct
  settings, one for screen devices like \code{x11} and \code{windows},
  one for black and white plots (mostly useful for \code{postscript})
  and one for color printers (color \code{postcript}, \code{pdf}).

  Once a device is open, its settings can be modified. When another
  instance of the same device is opened later using
  \code{trellis.device}, the settings for that device are reset to its
  defaults, unless otherwise specified in the call to
  \code{trellis.device}. But settings for different devices are treated
  separately, i.e., opening a postscript device will not alter the x11
  settings, which will remain in effect whenever an x11 device is
  active.

  The functions \code{trellis.par.*} are meant to be interfaces to the
  global settings. They always apply on the settings for the currently
  ACTIVE device.

  \code{trellis.par.get}, called without any arguments, returns the full
  list of settings for the active device. With the \code{name} argument
  present, it returns that component only. \code{trellis.par.get} sets
  the value of the \code{name} component of the current active device
  settings to \code{value}.

  \code{trellis.par.get} is usually used inside trellis functions to get
  graphical parameters before plotting. Modifications by users via
  \code{trellis.par.set} is traditionally done as follows:

  \code{add.line <- trellis.par.get("add.line")}

  \code{add.line$col <- "red"}

  \code{trellis.par.set("add.line", add.line)}

  More convenient (but not S compatible) ways to do this are

  \code{trellis.par.set(list(add.line = list(col = "red")))}

  and

  \code{trellis.par.set(add.line = list(col = "red"))}
  

  The actual list of the components in \code{trellis.settings} has not
  been finalized, so I'm not attempting to list them here. The current
  value can be obtained by \code{print(trellis.par.get())}. Most names
  should be self-explanatory.

  \code{show.settings} provides a graphical display summarizing some of
  the values in the current settings.
}


\note{
  In some ways, \code{trellis.par.get} and \code{trellis.par.set}
  together are a replacement for the \code{\link{par}} function used in
  traditional R graphics.  In particular, changing \code{par} settings
  has little (if any) effect on lattice output.  Since lattice plots are
  implemented using Grid graphics, its parameter system \emph{does} have
  an effect unless overridden by a suitable lattice parameter setting.
  Such parameters can be specified as part of a lattice theme by
  including them in the \code{grid.pars} component (see
  \code{\link[grid:gpar]{gpar}} for a list of valid parameter names).
}

\value{
  \code{trellis.par.get} returns a list giving parameters for that
  component. If \code{name} is missing, it returns the full list.

  Most of the settings are graphical parameters that control various
  elements of a lattice plot.  For details, see the examples below.  The
  more unusual settings are described here.
  
  \describe{
    \item{grid.pars}{ Grid graphical parameters that are in effect
      globally unless overridden by specific settings.}
    \item{fontsize}{ A list of two components (each a numeric scalar),
      \code{text} and \code{points}, for text and symbols
      respectively.
    } 
    \item{clip}{ A list of two components (each a character string,
      either \code{"on"} or \code{"off"}), \code{panel} and
      \code{strip}.
    } 
    \item{axis.components}{}
    \item{layout.heights}{}
    \item{layout.widths}{}
  }

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{trellis.device}}, \code{\link{Lattice}},
  \code{\link[grid:gpar]{gpar}}
}

\examples{
show.settings()

tp <- trellis.par.get()

unusual <- c("grid.pars", "fontsize", "clip", "axis.components",
             "layout.heights", "layout.widths")

for (u in unusual) tp[[u]] <- NULL
names.tp <- lapply(tp, names)
unames <- sort(unique(unlist(names.tp)))
ans <- matrix(0, nrow = length(names.tp), ncol = length(unames))
rownames(ans) <- names(names.tp)
colnames(ans) <- unames
for (i in seq(along = names.tp))
    ans[i, ] <- as.numeric(unames \%in\% names.tp[[i]])
ans <- ans[, order(-colSums(ans))]
ans <- ans[order(rowSums(ans)), ]
ans[ans == 0] <- NA

levelplot(t(ans), colorkey = FALSE, 
          scales = list(x = list(rot = 90)),
          panel = function(x, y, z, ...) {
              panel.abline(v = unique(as.numeric(x)), 
                           h = unique(as.numeric(y)), 
                           col = "darkgrey")
              panel.xyplot(x, y, pch = 16 * z, ...)
          },
          xlab = "Graphical parameters", 
          ylab = "Setting names")

}
\keyword{dplot}


