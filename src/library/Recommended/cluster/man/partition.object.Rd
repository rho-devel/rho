\name{partition.object}
\alias{partition}% == class
\alias{partition.object}
\title{Partitioning Object}
\description{
  The objects of class \code{"partition"} represent a partitioning of a
  dataset into clusters.
}
\section{GENERATION}{
  These objects are returned from \code{pam}, \code{clara} or \code{fanny}.
}
\section{METHODS}{
  The \code{"partition"} class has a method for the following generic functions:
  \code{plot}, \code{clusplot}.
}
\section{INHERITANCE}{
  The following classes inherit from class \code{"partition"} :
  \code{"pam"}, \code{"clara"} and \code{"fanny"}.

  See \code{\link{pam.object}}, \code{\link{clara.object}} and
  \code{\link{fanny.object}} for details.
}
\value{a \code{"partition"} object is a list with the following
  (and typically more) components:
  \item{clustering}{
    the clustering vector.  An integer vector of length \eqn{n}, the number of
    observations, giving for each observation the number (`id') of the
    cluster to which it belongs.}
  \item{call}{the matched \code{\link{call}} generating the object.}
  \item{silinfo}{
    a list with all \emph{silhouette} information, only available when
    the number of clusters is non-trivial, i.e., \eqn{1 < k < n} and
    then has the following components, see \code{\link{silhouette}}
    \describe{
      \item{widths}{an (n x 3) matrix, as returned by
	\code{\link{silhouette}()}, with for each observation i the
	cluster to which i belongs, as well as the neighbor cluster of i
	(the cluster, not containing i, for which the average
	dissimilarity between its observations and i is minimal), and
	the silhouette width \eqn{s(i)} of the observation.
      }
      \item{clus.avg.widths}{the average silhouette width per cluster.}
      \item{avg.width}{the average silhouette width for the dataset, i.e.,
	simply the average of \eqn{s(i)} over all observations \eqn{i}.}
    }% describe
    This information is also needed to construct a \emph{silhouette plot} of
    the clustering, see \code{\link{plot.partition}}.

    Note that \code{avg.width} can be maximized over different
    clusterings (e.g. with varying number of clusters) to choose an
    \emph{optimal} clustering.%% see an example or a demo << FIXME >>
  }
  \item{objective}{value of criterion maximized during the
    partitioning algorithm, may more than one entry for different stages.}
  \item{diss}{
    an object of class \code{"dissimilarity"}, representing the total
    dissimilarity matrix of the dataset (or relevant subset, e.g. for
    \code{clara}).
  }
  \item{data}{
    a matrix containing the original or standardized data.  This might
    be missing to save memory or when a dissimilarity matrix was given
    as input structure to the clustering method.
  }
}
\seealso{\code{\link{pam}}, \code{\link{clara}}, \code{\link{fanny}}.
}
\keyword{cluster}
