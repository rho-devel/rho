\name{F_2_panel.qqmathline}
\alias{panel.qqmathline}
\title{Useful panel function with qqmath}
\description{
  Useful panel function with qqmath. Draws a line passing through the
  points (usually) determined by the .25 and .75 quantiles of the sample
  and the theoretical distribution.
}
\usage{
panel.qqmathline(x, y = x,
                 distribution = qnorm,
                 probs = c(0.25, 0.75),
                 qtype = 7,
                 groups = NULL, 
                 \dots,
                 identifier = "qqmathline")
}
\arguments{
  \item{x}{ The original sample, possibly reduced to a fewer number of
    quantiles, as determined by the \code{f.value} argument to
    \code{qqmath}
  }
  \item{y}{
    an alias for \code{x} for backwards compatibility
  }
  \item{distribution}{
    quantile function for reference theoretical distribution.
  }
  \item{probs}{
    numeric vector of length two, representing probabilities.
    Corresponding quantile pairs define the line drawn.
  }
  \item{qtype}{
    the \code{type} of quantile computation used in
    \code{\link{quantile}}
  }
  \item{groups}{
    optional grouping variable.  If non-null, a line will be drawn for
    each group.
  }
  \item{\dots}{other arguments.}
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{prepanel.qqmathline}},
  \code{\link{qqmath}},
  \code{\link{quantile}}
}
\keyword{dplot}
