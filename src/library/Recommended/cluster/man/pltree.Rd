\name{pltree}
\alias{pltree}
\title{Clustering Trees - Generic Function}
\description{
  Generic function drawing a clustering tree (``dendrogram'') on the
  current graphics device.  There is a \code{twins} method, see
  \code{\link{pltree.twins}} for usage and examples.
}
\usage{
pltree(x, \dots)
}
\arguments{
  \item{x}{an \R object (for which a \code{pltree} method is defined).}
  \item{...}{methods may have additional arguments.  Graphical
    parameters (see \code{\link{par}}) may also be supplied as arguments
    to this function.}
}
\seealso{\code{\link{pltree.twins}}, \code{\link{twins.object}}.
}
\keyword{cluster}
\keyword{hplot}

