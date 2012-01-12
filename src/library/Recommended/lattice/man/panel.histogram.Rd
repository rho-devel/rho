\name{F_1_panel.histogram}
\alias{panel.histogram}
\title{ Default Panel Function for histogram }
\description{
  This is the default panel function for \code{histogram}.
}
\usage{
panel.histogram(x,
                breaks,
                equal.widths = TRUE,
                type = "density",
                nint = round(log2(length(x)) + 1), 
                alpha, col, border, lty, lwd,
                \dots,
                identifier = "histogram")
}
\arguments{
  \item{x}{ The data points for which the histogram is to be drawn} 
  \item{breaks}{ The breakpoints for the histogram}
  \item{equal.widths}{ logical used when \code{breaks==NULL} }
  \item{type}{ Type of histogram, possible values being \code{"percent"},
    \code{"density"} and \code{"count"}}
  \item{nint}{ Number of bins for the histogram }
  \item{alpha, col, border, lty, lwd}{
    graphical parameters for bars; defaults are obtained from the
    \code{plot.polygon} settings.
  }
  \item{\dots}{ other arguments, passed to \code{\link{hist}} when
    deemed appropriate }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{histogram}}}
\keyword{dplot}

