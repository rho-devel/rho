\name{B_09_tmd}
\alias{tmd}
\alias{tmd.formula}
\alias{tmd.trellis}
\alias{prepanel.tmd.default}
\alias{prepanel.tmd.qqmath}
\alias{panel.tmd.default}
\alias{panel.tmd.qqmath}
\title{Tukey Mean-Difference Plot}
\description{
  \code{tmd} Creates Tukey Mean-Difference Plots from a trellis object
  returned by \code{xyplot}, \code{qq} or \code{qqmath}.  The prepanel
  and panel functions are used as appropriate.  The \code{formula}
  method for \code{tmd} is provided for convenience, and simply calls
  \code{tmd} on the object created by calling \code{xyplot} on that
  formula.
}
\usage{
tmd(object, \dots)

\method{tmd}{trellis}(object,
    xlab = "mean",
    ylab = "difference",
    panel, 
    prepanel, 
    \dots)

prepanel.tmd.qqmath(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL,
             subscripts, \dots)
panel.tmd.qqmath(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL, 
             subscripts, \dots,
             identifier = "tmd")
panel.tmd.default(x, y, groups = NULL, \dots,
                  identifier = "tmd")
prepanel.tmd.default(x, y, \dots)
}
\arguments{
  \item{object}{ An object of class \code{"trellis"} returned by
    \code{xyplot}, \code{qq} or \code{qqmath}.  }
  \item{xlab}{ x label}
  \item{ylab}{ y label}
  \item{panel}{ panel function to be used.  See details below.  }
  \item{prepanel}{ prepanel function.  See details below.  }
  \item{f.value, distribution, qtype}{see \code{\link{panel.qqmath}}.  }
  \item{groups, subscripts}{see \code{\link{xyplot}}.  }
  \item{x, y}{ data as passed to panel functions in original call.  }
  \item{\dots}{ other arguments}
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\details{
  The Tukey Mean-difference plot is produced by modifying the (x,y)
  values of each panel as follows: the new coordinates are given by
  \code{x=(x+y)/2} and \code{y=y-x}, which are then plotted.  The
  default panel function(s) add a reference line at \code{y=0} as well.

  \code{tmd} acts on the a \code{"trellis"} object, not on the actual plot
  this object would have produced.  As such, it only uses the arguments
  supplied to the panel function in the original call, and completely
  ignores what the original panel function might have done with this
  data.  \code{tmd} uses these panel arguments to set up its own scales
  (using its \code{prepanel} argument) and display (using
  \code{panel}).  It is thus important to provide suitable prepanel and
  panel functions to \code{tmd} depending on the original call.

  Such functions currently exist for \code{xyplot}, \code{qq} (the ones
  with \code{default} in their name) and \code{qqmath}, as listed in the
  usage section above.  These assume the default displays for the
  corresponding high-level call.  If unspecified, the \code{prepanel} and
  \code{panel} arguments default to suitable choices.

  \code{tmd} uses the \code{update} method for \code{"trellis"} objects,
  which processes all extra arguments supplied to \code{tmd}.
}

\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{ \code{\link{qq}}, \code{\link{qqmath}}, \code{\link{xyplot}},
  \code{\link{Lattice}}
}

\examples{
tmd(qqmath(~height | voice.part, data = singer))
}
\keyword{dplot}
