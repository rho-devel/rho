\name{pltree.twins}
\alias{pltree.twins}
\title{Clustering Tree of a Hierarchical Clustering}
\description{
  Draws a clustering tree (dendrogram) of a \code{twins} object, i.e.,
  hierarchical clustering, typically resulting from
  \code{\link{agnes}()} or \code{\link{diana}()}.
}
\usage{
\method{pltree}{twins}(x, main = paste("Dendrogram of ", deparse(x$call)),
             labels = NULL, ylab = "Height", \dots)
}
\arguments{
  \item{x}{an object of class \code{"twins"}, typically created by either
    \code{\link{agnes}()} or \code{\link{diana}()}.}
  \item{main}{main title with a sensible default.}
  \item{labels}{labels to use; the default is constructed from \code{x}.}
  \item{ylab}{label for y-axis.}
  \item{\dots}{graphical parameters (see \code{\link{par}}) may also
    be supplied as arguments to this function.}
}
\value{
  a NULL value is returned.
}
\details{
  Creates a plot of a clustering tree given a \code{twins} object.  The
  leaves of the tree are the original observations.  In case of an
  agglomerative clustering, two branches come together at the distance
  between the two clusters being merged.  For a divisive clustering, a
  branch splits up at the diameter of the cluster being splitted.

  Note that currently the method function simply calls
  \code{plot(\link[stats]{as.hclust}(x), ...)}, which dispatches to
  \code{\link{plot.hclust}(..)}.  If more flexible plots are needed,
  consider \code{xx <- \link{as.dendrogram}(\link{as.hclust}(x))} and plotting
  \code{xx}, see \code{\link{plot.dendrogram}}.
}
\seealso{
  \code{\link{agnes}}, \code{\link{agnes.object}}, \code{\link{diana}},
  \code{\link{diana.object}}, \code{\link{hclust}}, \code{\link{par}},
  \code{\link{plot.agnes}}, \code{\link{plot.diana}}.
}
\examples{
data(votes.repub)
agn <- agnes(votes.repub)
pltree(agn)

dagn  <- as.dendrogram(as.hclust(agn))
dagn2 <- as.dendrogram(as.hclust(agn), hang = 0.2)
op <- par(mar = par("mar") + c(0,0,0, 2)) # more space to the right
plot(dagn2, horiz = TRUE)
plot(dagn, horiz = TRUE, center = TRUE,
     nodePar = list(lab.cex = 0.6, lab.col = "forest green", pch = NA),
     main = deparse(agn$call))
par(op)
}
\keyword{cluster}
\keyword{hplot}



