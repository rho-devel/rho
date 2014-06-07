\name{pam.object}
\alias{pam.object}
\title{Partitioning Around Medoids (PAM) Object}
\description{
  The objects of class \code{"pam"} represent a partitioning of a
  dataset into clusters.
}
\section{GENERATION}{
  These objects are returned from \code{\link{pam}}.}
\section{METHODS}{
  The \code{"pam"} class has methods for the following generic functions:
  \code{print}, \code{summary}.
}
\section{INHERITANCE}{
  The class \code{"pam"} inherits from \code{"partition"}.
  Therefore, the generic functions \code{plot} and \code{clusplot} can
  be used on a \code{pam} object.
}
\value{
  A legitimate \code{pam} object is a list with the following components:
  \item{medoids}{
    the medoids or representative objects of the
    clusters. If a dissimilarity matrix was given as input to
    \code{pam}, then a vector of numbers or labels of observations is
    given, else \code{medoids} is a matrix with in each row the
    coordinates of one medoid.}
  \item{id.med}{integer vector of \emph{indices} giving the medoid
    observation numbers.}
  \item{clustering}{the clustering vector, see \code{\link{partition.object}}.}
  \item{objective}{the objective function after the first and second
    step of the \code{pam} algorithm.}
  \item{isolation}{
    vector with length equal to the number of clusters, specifying which
    clusters are isolated clusters (L- or L*-clusters) and which clusters are
    not isolated.\cr
    A cluster is an L*-cluster iff its diameter is smaller than its
    separation.  A cluster is an L-cluster iff for each observation i
    the maximal dissimilarity between i and any other observation of the
    cluster is smaller than the minimal dissimilarity between i and any
    observation of another cluster.  Clearly each L*-cluster is also an
    L-cluster.
  }
  \item{clusinfo}{
    matrix, each row gives numerical information for one cluster. These
    are the cardinality of the cluster (number of observations), the
    maximal and average dissimilarity between the observations in the
    cluster and the cluster's medoid, %% FIXME: Now differs from clara.object.Rd:
    the diameter of the cluster
    (maximal dissimilarity between two observations of the cluster), and
    the separation of the cluster (minimal dissimilarity between an
    observation of the cluster and an observation of another cluster).
  }
  \item{silinfo}{list with silhouette width information, see
    \code{\link{partition.object}}.}
  \item{diss}{dissimilarity (maybe NULL), see \code{\link{partition.object}}.}
  \item{call}{generating call, see \code{\link{partition.object}}.}
  \item{data}{(possibibly standardized) see \code{\link{partition.object}}.}
}
\seealso{
  \code{\link{pam}}, \code{\link{dissimilarity.object}},
  \code{\link{partition.object}}, \code{\link{plot.partition}}.
}
\examples{
## Use the silhouette widths for assessing the best number of clusters,
## following a one-dimensional example from Christian Hennig :
##
x <- c(rnorm(50), rnorm(50,mean=5), rnorm(30,mean=15))
asw <- numeric(20)
## Note that "k=1" won't work!
for (k in 2:20)
  asw[k] <- pam(x, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

plot(1:20, asw, type= "h", main = "pam() clustering assessment",
     xlab= "k  (# clusters)", ylab = "average silhouette width")
axis(1, k.best, paste("best",k.best,sep="\n"), col = "red", col.axis = "red")
}
\keyword{cluster}

