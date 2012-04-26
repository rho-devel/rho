\name{B_11_oneway}
\alias{oneway}
\title{ Fit One-way Model}
\description{
  Fits a One-way model to univariate data grouped by a factor, the
  result often being displayed using \code{rfs} 
}
\usage{
oneway(formula, data, location=mean, spread=function(x) sqrt(var(x)))
}
\arguments{
  \item{formula}{ formula of the form \code{y ~ x} where \code{y} is the
    numeric response and \code{x} is the grouping factor
  }
  \item{data}{ data frame in which the model is to be evaluated }
  \item{location}{ function or numeric giving the location statistic to
    be used for centering the observations, e.g. \code{median}, 0 (to
    avoid centering).
  }
  \item{spread}{ function or numeric giving the spread statistic to
    be used for scaling the observations, e.g. \code{sd}, 1 (to
    avoid scaling).
  }
}
\value{
  A list with components
  \item{location}{ vector of locations for each group.}
  \item{spread}{ vector of spreads for each group.}
  \item{fitted.values}{ vector of locations for each observation.}
  \item{residuals}{ residuals (\code{y - fitted.values}).}
  \item{scaled.residuals}{residuals scaled by \code{spread} for their
    group} 
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{rfs}}, \code{\link{Lattice}} }
\keyword{dplot}
