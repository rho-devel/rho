\name{plot.agnes}
%% almost identical to ./plot.diana.Rd and quite similar to ./plot.mona.Rd
\alias{plot.agnes}
\title{Plots of an Agglomerative Hierarchical Clustering}
\description{
  Creates plots for visualizing an \code{agnes} object.
}
\usage{
\method{plot}{agnes}(x, ask = FALSE, which.plots = NULL, main = NULL,
           sub = paste("Agglomerative Coefficient = ",round(x$ac, digits = 2)),
           adj = 0, nmax.lab = 35, max.strlen = 5, xax.pretty = TRUE, \dots)
}
\arguments{
  \item{x}{an object of class \code{"agnes"}, typically created by
    \code{\link{agnes}(.)}.}
  \item{ask}{logical; if true and \code{which.plots} is \code{NULL},
    \code{plot.agnes} operates in interactive mode, via \code{\link{menu}}.}
  \item{which.plots}{integer vector or NULL (default), the latter
    producing both plots.  Otherwise, \code{which.plots}
    must contain integers of \code{1} for a \emph{banner} plot or \code{2} for a
    dendrogram or ``clustering tree''.}
  \item{main, sub}{main and sub title for the plot, with convenient
    defaults.  See documentation for these arguments in \code{\link{plot.default}}.}
  \item{adj}{for label adjustment in \code{\link{bannerplot}()}.}
  \item{nmax.lab}{integer indicating the number of labels which is
    considered too large for single-name labelling the banner plot.}
  \item{max.strlen}{positive integer giving the length to which
    strings are truncated in banner plot labeling.}
  \item{xax.pretty}{logical or integer indicating if
    \code{\link{pretty}(*, n = xax.pretty)} should be used for the x axis.
    \code{xax.pretty = FALSE} is for back compatibility.}
  \item{\dots}{graphical parameters (see \code{\link{par}}) may also
    be supplied and are passed to \code{\link{bannerplot}()} or
    \code{pltree()} (see \code{\link{pltree.twins}}), respectively.}
}
\section{Side Effects}{
  Appropriate plots are produced on the current graphics device. This can
  be one or both of the following choices:
  \cr Banner
  \cr Clustering tree
}
\details{
  When \code{ask = TRUE}, rather than producing each plot sequentially,
  \code{plot.agnes} displays a menu listing all the plots that can be produced.
  If the menu is not desired but a pause between plots is still wanted
  one must set \code{par(ask= TRUE)} before invoking the plot command.

  The banner displays the hierarchy of clusters, and is equivalent to a tree.
  See Rousseeuw (1986) or chapter 5 of Kaufman and Rousseeuw (1990).
  The banner plots distances at which observations and clusters are merged.
  The observations are listed in the order found by the \code{agnes} algorithm,
  and the numbers in the \code{height} vector are represented as bars
  between the observations.

  The leaves of the clustering tree are the original observations.  Two
  branches come together at the distance between the two clusters being merged.

  For more customization of the plots, rather call
  \code{\link{bannerplot}} and \code{pltree()}, i.e., its method
  \code{\link{pltree.twins}}, respectively.

  directly with
  corresponding arguments, e.g., \code{xlab} or \code{ylab}.
}
\note{
  In the banner plot, observation labels are only printed when the
  number of observations is limited less than \code{nmax.lab} (35, by
  default), for readability.  Moreover, observation labels are truncated
  to maximally \code{max.strlen} (5) characters.

  For the dendrogram, more flexibility than via \code{pltree()} is
  provided by \code{dg <- \link{as.dendrogram}(x)} and
  plotting \code{dg} via \code{\link[stats]{plot.dendrogram}}.
}
\references{
  Kaufman, L. and Rousseeuw, P.J. (1990)
  \emph{Finding Groups in Data: An Introduction to Cluster Analysis}.
  Wiley, New York.

  Rousseeuw, P.J. (1986). A visual display for hierarchical classification,
  in \emph{Data Analysis and Informatics 4}; edited by E. Diday,
  Y. Escoufier, L. Lebart, J. Pages, Y. Schektman, and R. Tomassone.
  North-Holland, Amsterdam, 743--748.

  Struyf, A., Hubert, M. and Rousseeuw, P.J. (1997)
  Integrating Robust Clustering Techniques in S-PLUS,
  \emph{Computational Statistics and Data Analysis}, \bold{26}, 17--37.
}
\seealso{
  \code{\link{agnes}} and \code{\link{agnes.object}};
  \code{\link{bannerplot}}, \code{\link{pltree.twins}},
  and \code{\link{par}}.
}
\examples{
## Can also pass `labels' to pltree() and bannerplot():
data(iris)
cS <- as.character(Sp <- iris$Species)
cS[Sp == "setosa"] <- "S"
cS[Sp == "versicolor"] <- "V"
cS[Sp == "virginica"] <- "g"
ai <- agnes(iris[, 1:4])
plot(ai, labels = cS, nmax = 150)# bannerplot labels are mess
}
\keyword{cluster}
\keyword{hplot}
