\name{D_trellis.object}
\alias{trellis.object}
\docType{class}
%%\non_function{}
\title{A Trellis Plot Object}

\description{

  This class of objects is returned by high level lattice functions, and
  is usually plotted by default by its
  \code{\link[lattice:print.trellis]{print}} method.

}

\details{
  A trellis object, as returned by high level lattice functions like
  \code{\link{xyplot}}, is a list with the \code{"class"} attribute set
  to \code{"trellis"}.  Many of the components of this list are simply
  the arguments to the high level function that produced the object.
  Among them are: \code{as.table}, \code{layout}, \code{page},
  \code{panel}, \code{prepanel}, \code{main}, \code{sub},
  \code{par.strip.text}, \code{strip}, \code{skip}, \code{xlab}
  \code{ylab}, \code{par.settings}, \code{lattice.options} and
  \code{plot.args}.  Some other typical components are:

  \describe{
    \item{\code{formula}}{
      the Trellis formula used in the call
    }
    \item{\code{index.cond}}{
      list with index for each of the conditioning variables
    }
    \item{\code{perm.cond}}{
      permutation of the order of the conditioning variables
    }
    \item{\code{aspect.fill}}{
      logical, whether \code{aspect} is \code{"fill"}
    }
    \item{\code{aspect.ratio}}{
      numeric, aspect ratio to be used if \code{aspect.fill} is
      \code{FALSE}
    }
    \item{\code{call}}{ call that generated the object. }
    \item{\code{condlevels}}{
      list with levels of the conditioning variables
    }
    \item{\code{legend}}{
      list describing the legend(s) to be drawn
    }
    \item{\code{panel.args}}{
      a list as long as the number of panels, each element being a list
      itself, containing the arguments in named form to be passed to the
      panel function in that panel.
    }
    \item{\code{panel.args.common}}{
      a list containing the arguments common to all the panel functions
      in \code{name=value} form
    }
    \item{\code{x.scales}}{
      list describing x-scale, can consist of several other lists,
      paralleling panel.args, if x-relation is not \code{"same"}
    }
    \item{\code{y.scales}}{
      list describing y-scale, similar to \code{x.scales}
    }
    \item{\code{x.between}}{ numeric vector of interpanel x-space }
    \item{\code{y.between}}{ numeric vector of interpanel y-space }
    \item{\code{x.limits}}{
      numeric vector of length 2 or list, giving x-axis limits
    }
    \item{\code{y.limits}}{ similar to \code{x.limits} }
    \item{\code{packet.sizes}}{
      array recording the number of observations in each packet
    }
  }
}

\seealso{
  \code{\link{Lattice}}, \code{\link{xyplot}},
  \code{\link{print.trellis}}
}

\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}


\keyword{classes}

