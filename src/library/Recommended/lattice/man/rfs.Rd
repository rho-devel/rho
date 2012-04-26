\name{B_10_rfs}
\alias{rfs}
\title{ Residual and Fit Spread Plots}
\description{
  Plots fitted values and residuals (via qqmath) on a common scale for
  any object that has methods for fitted values and residuals. 
}
\usage{
rfs(model, layout=c(2, 1), xlab="f-value", ylab=NULL,
    distribution = qunif,
    panel, prepanel, strip, \dots)
}
\arguments{
  \item{model}{a fitted model object with methods \code{fitted.values}
    and \code{residuals}. Can be the value returned by \code{oneway} }
  \item{layout}{default layout is c(2,1)  }
  \item{xlab}{defaults to \code{"f.value"} }
  \item{distribution}{the distribution function to be used for \code{qqmath}}
  \item{ylab, panel, prepanel, strip}{ See \code{\link{xyplot}} }
  \item{\dots}{ other arguments, passed on to \code{\link{qqmath}}.  }
}
\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{oneway}}, \code{\link{qqmath}},
  \code{\link{xyplot}}, \code{\link{Lattice}}
}
\examples{
rfs(oneway(height ~ voice.part, data = singer, spread = 1), aspect = 1)
}
\keyword{dplot}
