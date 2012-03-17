\name{silhouette}
\alias{silhouette}
\alias{silhouette.clara}
\alias{silhouette.default}
\alias{silhouette.partition}
\alias{sortSilhouette}
\alias{summary.silhouette}
\alias{print.summary.silhouette}
\alias{plot.silhouette}
\title{Compute or Extract Silhouette Information from Clustering}
\description{
  Compute silhouette information according to a given clustering in
  \eqn{k} clusters.
}
\usage{
silhouette(x, \dots)
\method{silhouette}{default}  (x, dist, dmatrix, \dots)
\method{silhouette}{partition}(x, \dots)
\method{silhouette}{clara}(x, full = FALSE, \dots)

sortSilhouette(object, \dots)
\method{summary}{silhouette}(object, FUN = mean, \dots)
\method{plot}{silhouette}(x, nmax.lab = 40, max.strlen = 5,
     main = NULL, sub = NULL, xlab = expression("Silhouette width "* s[i]),
     col = "gray",  do.col.sort = length(col) > 1, border = 0,
     cex.names = par("cex.axis"), do.n.k = TRUE, do.clus.stat = TRUE, \dots)
}
\arguments{
  \item{x}{an object of appropriate class; for the \code{default}
    method an integer vector with \eqn{k} different integer cluster
    codes or a list with such an \code{x$clustering}
    component.  Note that silhouette statistics are only defined if
    \eqn{2 \le k \le n-1}{2 <= k <= n-1}.}
  \item{dist}{a dissimilarity object inheriting from class
    \code{\link{dist}} or coercible to one.  If not specified,
    \code{dmatrix} must be.}
  \item{dmatrix}{a symmetric dissimilarity matrix (\eqn{n \times n}{n * n}),
    specified instead of \code{dist}, which can be more efficient.}
  \item{full}{logical specifying if a \emph{full} silhouette should be
    computed for \code{\link{clara}} object.  Note that this requires
    \eqn{O(n^2)} memory, since the full dissimilarity (see
    \code{\link{daisy}}) is needed internally.}
  \item{object}{an object of class \code{silhouette}.}
  \item{\dots}{further arguments passed to and from methods.}
  \item{FUN}{function used to summarize silhouette widths.}
  \item{nmax.lab}{integer indicating the number of labels which is
    considered too large for single-name labeling the silhouette plot.}
  \item{max.strlen}{positive integer giving the length to which
    strings are truncated in silhouette plot labeling.}
  \item{main, sub, xlab}{arguments to \code{\link{title}}; have a
    sensible non-NULL default here.}
  \item{col, border, cex.names}{arguments passed
    \code{\link{barplot}()}; note that the default used to be \code{col
      = heat.colors(n), border = par("fg")} instead.\cr
    \code{col} can also be a color vector of length \eqn{k} for
    clusterwise coloring, see also \code{do.col.sort}:
  }
  \item{do.col.sort}{logical indicating if the colors \code{col} should
    be sorted \dQuote{along} the silhouette; this is useful for casewise or
    clusterwise coloring.}
  \item{do.n.k}{logical indicating if \eqn{n} and \eqn{k} \dQuote{title text}
    should be written.}
  \item{do.clus.stat}{logical indicating if cluster size and averages
    should be written right to the silhouettes.}
}
\details{
    For each observation i, the \emph{silhouette width} \eqn{s(i)} is
    defined as follows: \cr
    Put a(i) = average dissimilarity between i and all other points of the
    cluster to which i belongs (if i is the \emph{only} observation in
    its cluster, \eqn{s(i) := 0} without further calculations).
    For all \emph{other} clusters C, put \eqn{d(i,C)} = average
    dissimilarity of i to all observations of C.  The smallest of these
    \eqn{d(i,C)} is \eqn{b(i) := \min_C d(i,C)},
    and can be seen as the dissimilarity between i and its \dQuote{neighbor}
    cluster, i.e., the nearest one to which it does \emph{not} belong.
    Finally, \deqn{s(i) := \frac{b(i) - a(i) }{max(a(i), b(i))}.}{%
      s(i) := ( b(i) - a(i) ) / max( a(i), b(i) ).}

    \code{silhouette.default()} is now based on C code donated by Romain
    Francois (the R version being still available as
    \code{cluster:::silhouette.default.R}).

    Observations with a large \eqn{s(i)} (almost 1) are very well
    clustered, a small \eqn{s(i)} (around 0) means that the observation
    lies between two clusters, and observations with a negative
    \eqn{s(i)} are probably placed in the wrong cluster.
}
\note{
  While \code{silhouette()} is \emph{intrinsic} to the
  \code{\link{partition}} clusterings, and hence has a (trivial) method
  for these, it is straightforward to get silhouettes from hierarchical
  clusterings from \code{silhouette.default()} with
  \code{\link{cutree}()} and distance as input.

  By default, for \code{\link{clara}()} partitions, the silhouette is
  just for the best random \emph{subset} used.  Use \code{full = TRUE}
  to compute (and later possibly plot) the full silhouette.
}
\value{
  \code{silhouette()} returns an object, \code{sil}, of class
  \code{silhouette} which is an [n x 3] matrix with attributes.  For
  each observation i, \code{sil[i,]} contains the cluster to which i
  belongs as well as the neighbor cluster of i (the cluster, not
  containing i, for which the average dissimilarity between its
  observations and i is minimal), and the silhouette width \eqn{s(i)} of
  the observation.  The \code{\link{colnames}} correspondingly are
  \code{c("cluster", "neighbor", "sil_width")}.

  \code{summary(sil)} returns an object of class
  \code{summary.silhouette}, a list with components
%%Rd bug: fails inside \value{}!:\describe{
    \item{si.summary}{numerical \code{\link{summary}} of the individual
      silhouette widths \eqn{s(i)}.}
    \item{clus.avg.widths}{numeric (rank 1) array of clusterwise
      \emph{means} of silhouette widths where \code{mean = FUN} is used.}
    \item{avg.width}{the total mean \code{FUN(s)} where \code{s} are the
      individual silhouette widths.}
    \item{clus.sizes}{\code{\link{table}} of the \eqn{k} cluster sizes.}
    \item{call}{if available, the call creating \code{sil}.}
    \item{Ordered}{logical identical to \code{attr(sil, "Ordered")}, see
      below.}
%%Rd bug:   }

  \code{sortSilhouette(sil)} orders the rows of \code{sil} as in the
  silhouette plot, by cluster (increasingly) and decreasing silhouette
  width \eqn{s(i)}.
  \cr
  \code{attr(sil, "Ordered")} is a logical indicating if \code{sil} \emph{is}
  ordered as by \code{sortSilhouette()}. In that case,
  \code{rownames(sil)} will contain case labels or numbers, and \cr
  \code{attr(sil, "iOrd")} the ordering index vector.
}
\references{
  Rousseeuw, P.J. (1987)
  Silhouettes: A graphical aid to the interpretation and validation of
  cluster analysis. \emph{J. Comput. Appl. Math.}, \bold{20}, 53--65.

  chapter 2 of Kaufman, L. and Rousseeuw, P.J. (1990), see
  the references in \code{\link{plot.agnes}}.
}
\seealso{\code{\link{partition.object}}, \code{\link{plot.partition}}.
}
\examples{
data(ruspini)
pr4 <- pam(ruspini, 4)
str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

si2 <- silhouette(pr4$clustering, dist(ruspini, "canberra"))
summary(si2) # has small values: "canberra"'s fault
plot(si2, nmax= 80, cex.names=0.6)

op <- par(mfrow= c(3,2), oma= c(0,0, 3, 0),
          mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))
for(k in 2:6)
   plot(silhouette(pam(ruspini, k=k)), main = paste("k = ",k), do.n.k=FALSE)
mtext("PAM(Ruspini) as in Kaufman & Rousseeuw, p.101",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()

## the same with cluster-wise colours:
c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20")
for(k in 2:6)
   plot(silhouette(pam(ruspini, k=k)), main = paste("k = ",k), do.n.k=FALSE,
        col = c6[1:k])
par(op)

## clara(): standard silhouette is just for the best random subset
data(xclara)
set.seed(7)
str(xc1k <- xclara[sample(nrow(xclara), size = 1000) ,])
cl3 <- clara(xc1k, 3)
plot(silhouette(cl3))# only of the "best" subset of 46
## The full silhouette: internally needs large (36 MB) dist object:
sf <- silhouette(cl3, full = TRUE) ## this is the same as
s.full <- silhouette(cl3$clustering, daisy(xc1k))
if(paste(R.version$major, R.version$minor, sep=".") >= "2.3.0")
   stopifnot(all.equal(sf, s.full, check.attributes = FALSE, tol = 0))
## color dependent on original "3 groups of each 1000":
plot(sf, col = 2+ as.integer(names(cl3$clustering) ) \%/\% 1000,
     main ="plot(silhouette(clara(.), full = TRUE))")

## Silhouette for a hierarchical clustering:
ar <- agnes(ruspini)
si3 <- silhouette(cutree(ar, k = 5), # k = 4 gave the same as pam() above
    	           daisy(ruspini))
plot(si3, nmax = 80, cex.names = 0.5)
## 2 groups: Agnes() wasn't too good:
si4 <- silhouette(cutree(ar, k = 2), daisy(ruspini))
plot(si4, nmax = 80, cex.names = 0.5)
}
\keyword{cluster}
