\name{mona.object}
\alias{mona.object}
\title{Monothetic Analysis (MONA) Object}
\description{
  The objects of class \code{"mona"} represent the divisive
  hierarchical clustering of a dataset with only binary variables
  (measurements).   This class of objects is returned from
  \code{\link{mona}}.
}
\section{METHODS}{
  The \code{"mona"} class has methods for the following generic functions:
  \code{print}, \code{summary}, \code{plot}.
}
\value{
  A legitimate \code{mona} object is a list with the following components:

  \item{data}{
    matrix with the same dimensions as the original data matrix,
    but with factors coded as 0 and 1, and all missing values replaced.
  }
  \item{order}{
    a vector giving a permutation of the original observations to allow
    for plotting, in the sense that the branches of a clustering tree
    will not cross.
  }
  \item{order.lab}{
    a vector similar to \code{order}, but containing observation labels
    instead of observation numbers. This component is only available if
    the original observations were labelled.
  }
  \item{variable}{
    vector of length n-1 where n is the number of observations,
    specifying the variables used to separate the observations of \code{order}.
  }
  \item{step}{
    vector of length n-1 where n is the number of observations,
    specifying the separation steps at which the observations of
    \code{order} are separated.
  }
}
\seealso{\code{\link{mona}} for examples etc, \code{\link{plot.mona}}.
}
\keyword{cluster}

