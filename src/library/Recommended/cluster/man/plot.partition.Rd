\name{plot.partition}
\alias{plot.partition}
\title{Plot of a Partition of the Data Set}
\description{Creates plots for visualizing a \code{partition} object.}
\usage{
\method{plot}{partition}(x, ask = FALSE, which.plots = NULL,
     nmax.lab = 40, max.strlen = 5, data = x$data, dist = NULL,
     stand = FALSE, lines = 2,
     shade = FALSE, color = FALSE, labels = 0, plotchar = TRUE,
     span = TRUE, xlim = NULL, ylim = NULL, main = NULL, \dots)
}
\arguments{
  \item{x}{an object of class \code{"partition"}, typically created by the
    functions \code{\link{pam}}, \code{\link{clara}}, or \code{\link{fanny}}.}
  \item{ask}{logical; if true and \code{which.plots} is \code{NULL},
    \code{plot.partition} operates in interactive mode, via \code{\link{menu}}.}
  \item{which.plots}{integer vector or NULL (default), the latter
    producing both plots.  Otherwise, \code{which.plots} must contain
    integers of \code{1} for a \emph{clusplot} or \code{2} for
    \emph{silhouette}.}
  \item{nmax.lab}{integer indicating the number of labels which is
    considered too large for single-name labeling the silhouette plot.}
  \item{max.strlen}{positive integer giving the length to which
    strings are truncated in silhouette plot labeling.}
  \item{data}{numeric matrix with the scaled data; per default taken
    from the partition object \code{x}, but can be specified explicitly.}
  \item{dist}{when \code{x} does not have a \code{diss} component as for
    \code{\link{pam}(*, keep.diss=FALSE)}, \code{dist} must be the
    dissimilarity if a clusplot is desired.}
  \item{stand,lines,shade,color,labels,plotchar,span,xlim,ylim,main, \dots}{
    All optional arguments available for the \code{\link{clusplot.default}}
    function (except for the \code{diss} one) and graphical parameters
    (see \code{\link{par}}) may also be supplied as arguments to this function.}
}
\section{Side Effects}{
  An appropriate plot is produced on the current graphics device.  This
  can be one or both of the following choices:
 \cr Clusplot
 \cr Silhouette plot
}
\details{
  When \code{ask= TRUE}, rather than producing each plot sequentially,
  \code{plot.partition} displays a menu listing all the plots that can
  be produced.
  If the menu is not desired but a pause between plots is still wanted,
  call \code{par(ask= TRUE)} before invoking the plot command.

  The \emph{clusplot} of a cluster partition consists of a two-dimensional
  representation of the observations, in which the clusters are
  indicated by ellipses (see \code{\link{clusplot.partition}} for more
  details).

  The \emph{silhouette plot} of a nonhierarchical clustering is fully
  described in Rousseeuw (1987) and in chapter 2 of Kaufman and
  Rousseeuw (1990).
  For each observation i, a bar is drawn, representing its silhouette
  width s(i), see \code{\link{silhouette}} for details.
  Observations are grouped per cluster, starting with cluster 1 at the
  top.  Observations with a large s(i) (almost 1) are very well
  clustered, a small s(i) (around 0) means that the observation lies
  between two clusters, and observations with a negative s(i) are
  probably placed in the wrong cluster.

  A clustering can be performed for several values of \code{k} (the number of
  clusters).  Finally, choose the value of \code{k} with the largest overall
  average silhouette width.
}
\note{
  In the silhouette plot, observation labels are only printed when the
  number of observations is less than \code{nmax.lab} (40, by default),
  for readability.  Moreover, observation labels are truncated to
  maximally \code{max.strlen} (5) characters.  \cr
  For more flexibility, use \code{plot(silhouette(x), ...)}, see
  \code{\link{plot.silhouette}}.
}
\references{
  Rousseeuw, P.J. (1987)
  Silhouettes: A graphical aid to the interpretation and validation of
  cluster analysis. \emph{J. Comput. Appl. Math.}, \bold{20}, 53--65.

  Further, the references in \code{\link{plot.agnes}}.
}
\seealso{
  \code{\link{partition.object}}, \code{\link{clusplot.partition}},
  \code{\link{clusplot.default}}, \code{\link{pam}},
  \code{\link{pam.object}}, \code{\link{clara}},
  \code{\link{clara.object}}, \code{\link{fanny}},
  \code{\link{fanny.object}}, \code{\link{par}}.
}
\examples{
## generate 25 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
plot(pam(x, 2))

## Save space not keeping data in clus.object, and still clusplot() it:
data(xclara)
cx <- clara(xclara, 3, keep.data = FALSE)
cx$data # is NULL
plot(cx, data = xclara)
}
\keyword{cluster}
\keyword{hplot}
% Converted by Sd2Rd version 0.3-2.
