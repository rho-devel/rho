\name{C_01_trellis.device}
\alias{trellis.device}
\alias{standard.theme}
\alias{canonical.theme}
\alias{col.whitebg}
\title{ Initializing Trellis Displays }
\description{
  Initialization of a display device with appropriate graphical
  parameters.
}

\usage{
trellis.device(device = getOption("device"),
               color = !(dev.name == "postscript"),
               theme = lattice.getOption("default.theme"),
               new = TRUE,
               retain = FALSE,
               \dots)

standard.theme(name, color)
canonical.theme(name, color)
col.whitebg()

}
\arguments{
  \item{device}{ function (or the name of one as a character string)
    that starts a device.  Admissible values depend on the platform and
    how \R was compiled (see \code{\link{Devices}}), but usually
    \code{"pdf"}, \code{"postscript"}, \code{"png"}, \code{"jpeg"} and
    at least one of \code{"X11"}, \code{"windows"} and \code{"quartz"}
    will be available.
  }
  \item{color}{ logical, whether the initial settings should be color or
    black and white.  Defaults to \code{FALSE} for postscript devices,
    \code{TRUE} otherwise.  Note that this only applies to the initial
    choice of colors, which can be overridden using \code{theme} or
    subsequent calls to \code{\link{trellis.par.set}} (and by arguments
    supplied directly in high level calls for some settings).
  }
  \item{theme}{ list of components that changes the settings of the
    device opened, or, a function that when called produces such a
    list.  The function name can be supplied as a quoted string.  These
    settings are only used to modify the default settings (determined by
    other arguments), and need not contain all possible parameters.

    A possible use of this argument is to change the default settings by 
    specifying \code{lattice.options(default.theme = "col.whitebg")}. 
    For back-compatibility, this is initially (when lattice is loaded)
    set to \code{getOption(lattice.theme)}.

    If \code{theme} is a function, it will not be supplied any
    arguments, however, it is guaranteed that a device will already be
    open when it is called, so one may use \code{.Device} inside the
    function to ascertain what device has been opened.
  }
  \item{new}{ logical flag indicating whether a new device should be
    started.  If \code{FALSE}, the options for the current device are
    changed to the defaults determined by the other arguments.
  }
  \item{retain}{
    logical. If \code{TRUE} and a setting for this device already
    exists, then that is used instead of the defaults for this
    device. By default, pre-existing settings are overwritten (and
    lost).
  }
  \item{name}{ name of the device for which the setting is required, as
    returned by \code{.Device}
  }
  \item{\dots}{ additional parameters to be passed to the \code{device}
    function, most commonly \code{file} for non-screen devices, as well
    as \code{height}, \code{width}, etc.  See the help file for
    individual devices for admissible arguments.
  }
}
\details{

  Trellis Graphics functions obtain the default values of various
  graphical parameters (colors, line types, fonts, etc.) from a
  customizable \dQuote{settings} list.  This functionality is analogous
  to \code{\link{par}} for standard \R graphics and, together with
  \code{\link{lattice.options}}, mostly supplants it (\code{\link{par}}
  settings are mostly ignored by Lattice).  Unlike \code{\link{par}},
  Trellis settings can be controlled separately for each different
  device type (but not concurrently for different instances of the same
  device).  \code{standard.theme} and \code{col.whitebg} produce
  predefined settings (a.k.a. themes), while \code{trellis.device}
  provides a high level interface to control which \dQuote{theme} will
  be in effect when a new device is opened.  \code{trellis.device} is
  called automatically when a \code{"trellis"} object is plotted, and
  the defaults can be used to provide sufficient control, so in a
  properly configured system it is rarely necessary for the user to call
  \code{trellis.device} explicitly.

  The \code{standard.theme} function is intended to provide device
  specific settings (e.g. light colors on a grey background for screen
  devices, dark colors or black and white for print devices) which were
  used as defaults prior to \R 2.3.0.  However, these defaults are not
  always appropriate, due to the variety of platforms and hardware
  settings on which \R is used, as well as the fact that a plot created
  on a particular device may be subsequently used in many different
  ways.  For this reason, a \dQuote{safe} default is used for all
  devices from \R 2.3.0 onwards.  The old behaviour can be reinstated by
  setting \code{standard.theme} as the default \code{theme} argument,
  e.g. by putting \code{lattice.options(default.theme =
  "standard.theme")} in a startup script (see the entry for \code{theme}
  above for details).
}

\value{
  \code{standard.theme} returns a list of components defining graphical
  parameter settings for Lattice displays. It is used internally in
  \code{trellis.device}, and can also be used as the \code{theme}
  argument to \code{trellis.par.set}, or even as \code{theme} in
  \code{trellis.device} to use the defaults for another device.
  \code{canonical.theme} is an alias for \code{standard.theme}.

  \code{col.whitebg} returns a similar (but smaller) list that is
  suitable as the \code{theme} argument to \code{trellis.device} and
  \code{\link{trellis.par.set}}.  It contains settings values which
  provide colors suitable for plotting on a white background.  Note that
  the name \code{col.whitebg} is somewhat of a misnomer, since it
  actually sets the background to transparent rather than white.
}

\note{
  Earlier versions of \code{trellis.device} had a \code{bg} argument to
  set the background color, but this is no longer supported.  If
  supplied, the \code{bg} argument will be passed on to the device
  function; however, this will have no effect on the Trellis settings.
  It is rarely meaningful to change the background alone; if you feel
  the need to change the background, consider using the \code{theme}
  argument instead.
}

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.
  \url{http://lmdvr.r-forge.r-project.org/}

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{

  \code{\link{Lattice}} for an overview of the \code{lattice} package.
  
  \code{\link{Devices}} for valid choices of \code{device} on your
  platform.

  \code{\link{trellis.par.get}} and \code{\link{trellis.par.set}} can be
  used to query and modify the settings \emph{after} a device has been
  initialized.  The \code{par.settings} argument to high level
  functions, described in \code{\link{xyplot}}, can be used to attach
  transient settings to a \code{"trellis"} object.
}
\keyword{dplot}

