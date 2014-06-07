\name{clara.object}
\alias{clara.object}
\title{Clustering Large Applications (CLARA) Object}
\description{
  The objects of class \code{"clara"} represent a partitioning of a large
  dataset into clusters and are typically returned from \code{\link{clara}}.
}
\section{Methods, Inheritance}{
  The \code{"clara"} class has methods for the following generic functions:
  \code{print}, \code{summary}.

  The class \code{"clara"} inherits from \code{"partition"}.
  Therefore, the generic functions \code{plot} and \code{clusplot} can
  be used on a \code{clara} object.
}
\value{
  A legitimate \code{clara} object is a list with the following components:

  \item{sample}{
    labels or case numbers of the observations in the best sample, that is,
    the sample used by the \code{clara} algorithm for the final partition.}
  \item{medoids}{the medoids or representative objects of the clusters.
    It is a matrix with in each row the coordinates of one medoid.
    Possibly \code{NULL}, namely when the object resulted from
    \code{clara(*, medoids.x=FALSE)}. Use the following \code{i.med} in
    that case.}
  \item{i.med}{
    the \emph{indices} of the \code{medoids} above: \code{medoids <- x[i.med,]}
    where \code{x} is the original data matrix in \code{clara(x,*)}.}
  \item{clustering}{the clustering vector, see \code{\link{partition.object}}.}
  \item{objective}{the objective function for the final clustering of
    the entire dataset.}
  \item{clusinfo}{
    matrix, each row gives numerical information for one cluster. These
    are the cardinality of the cluster (number of observations), the
    maximal and average dissimilarity between the observations in the
    cluster and the cluster's medoid.  %% FIXME: Now differs from pam.object.Rd:
    The last column is the maximal
    dissimilarity between the observations in the cluster and the
    cluster's medoid, divided by the minimal dissimilarity between the
    cluster's medoid and the medoid of any other cluster. If this ratio
    is small, the cluster is well-separated from the other clusters.
  }
  \item{diss}{dissimilarity (maybe NULL), see \code{\link{partition.object}}.}
  \item{silinfo}{list with silhouette width information for the best sample, see
    \code{\link{partition.object}}.}
  \item{call}{generating call, see \code{\link{partition.object}}.}
  \item{data}{matrix, possibibly standardized, or NULL, see
    \code{\link{partition.object}}.}
}
\seealso{
  \code{\link{clara}}, \code{\link{dissimilarity.object}},
  \code{\link{partition.object}}, \code{\link{plot.partition}}.
}
\keyword{cluster}
