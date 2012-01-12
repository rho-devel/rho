\name{clusplot}
\alias{clusplot}
\alias{clusplot.partition}
\title{Bivariate Cluster Plot (of a Partitioning Object)}
\description{
  Draws a 2-dimensional \dQuote{clusplot} (clustering plot) on the
  current graphics device.
  The generic function has a default and a \code{partition} method.
}
\usage{
clusplot(x, \dots)

\method{clusplot}{partition}(x, main = NULL, dist = NULL, \dots)
}
\arguments{
  \item{x}{an \R object, here, specifically an object of class
    \code{"partition"}, e.g. created by one of the functions
    \code{\link{pam}}, \code{\link{clara}}, or \code{\link{fanny}}.}
  \item{main}{title for the plot; when \code{NULL} (by default), a title
    is constructed, using \code{x$call}.}
  \item{dist}{when \code{x} does not have a \code{diss} nor a
    \code{data} component, e.g., for \code{\link{pam}(dist(*),
      keep.diss=FALSE)}, \code{dist} must specify the dissimilarity for the
    clusplot.}
  \item{\dots}{optional arguments passed to methods, notably the
    \code{\link{clusplot.default}} method (except for the \code{diss}
    one) may also be supplied to this function.  Many graphical parameters
    (see \code{\link{par}}) may also be supplied as arguments here.}
}
\section{Side Effects}{
  a 2-dimensional clusplot is created on the current graphics device.
}
\value{
  For the \code{partition} (and \code{default}) method: An invisible
  list with components \code{Distances} and \code{Shading}, as for
  \code{\link{clusplot.default}}, see there.
}
\details{
  The \code{clusplot.partition()} method relies on \code{\link{clusplot.default}}.

  If the clustering algorithms \code{pam}, \code{fanny} and \code{clara}
  are applied to a data matrix of observations-by-variables then a
  clusplot of the resulting clustering can always be drawn.  When the
  data matrix contains missing values and the clustering is performed
  with \code{\link{pam}} or \code{\link{fanny}}, the dissimilarity
  matrix will be given as input to \code{clusplot}.  When the clustering
  algorithm \code{\link{clara}} was applied to a data matrix with NAs
  then clusplot will replace the missing values as described in
  \code{\link{clusplot.default}}, because a dissimilarity matrix is not
  available.
}
\seealso{\code{\link{clusplot.default}} for references;
  \code{\link{partition.object}}, \code{\link{pam}},
  \code{\link{pam.object}}, \code{\link{clara}},
  \code{\link{clara.object}}, \code{\link{fanny}},
  \code{\link{fanny.object}}, \code{\link{par}}.
}
\examples{ ## For more, see ?clusplot.default

## generate 25 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
clusplot(pam(x, 2))
## add noise, and try again :
x4 <- cbind(x, rnorm(25), rnorm(25))
clusplot(pam(x4, 2))
}
\keyword{cluster}
\keyword{hplot}
