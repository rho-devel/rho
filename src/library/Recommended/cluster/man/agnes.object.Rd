\name{agnes.object}
\alias{agnes.object}
\title{Agglomerative Nesting (AGNES) Object}
\description{
  The objects of class \code{"agnes"}
  represent an agglomerative hierarchical clustering of a dataset.
}
\section{GENERATION}{
  This class of objects is returned from \code{\link{agnes}}.
}
\section{METHODS}{
  The \code{"agnes"} class has methods for the following generic functions:
  \code{print}, \code{summary}, \code{plot}, and \code{\link{as.dendrogram}}.
}
\section{INHERITANCE}{
  The class \code{"agnes"} inherits from \code{"twins"}.
  Therefore, the generic functions \code{\link{pltree}} and
  \code{\link{as.hclust}} are available for \code{agnes} objects.
  After applying \code{as.hclust()}, all \emph{its} methods are
  available, of course.
}
\value{
  A legitimate \code{agnes} object is a list with the following components:
  \item{order}{
    a vector giving a permutation of the original observations to allow
    for plotting, in the sense that the branches of a clustering tree
    will not cross.}
  \item{order.lab}{
    a vector similar to \code{order}, but containing observation labels
    instead of observation numbers. This component is only available if
    the original observations were labelled.
  }
  \item{height}{
    a vector with the distances between merging clusters at the successive
    stages.
  }
  \item{ac}{
    the agglomerative coefficient, measuring the clustering structure of the
    dataset.

    For each observation i, denote by m(i) its dissimilarity to the
    first cluster it is merged with, divided by the dissimilarity of the
    merger in the final step of the algorithm.  The \code{ac} is the
    average of all 1 - m(i). It can also be seen as the average width
    (or the percentage filled) of the banner plot.  Because \code{ac}
    grows with the number of observations, this measure should not
    be used to compare datasets of very different sizes.
  }
  \item{merge}{
    an (n-1) by 2 matrix, where n is the number of observations.  Row i
    of \code{merge} describes the merging of clusters at step i of the
    clustering.  If a number j in the row is negative, then the single
    observation |j| is merged at this stage.  If j is positive, then the
    merger is with the cluster formed at stage j of the algorithm.
  }
  \item{diss}{
    an object of class \code{"dissimilarity"} (see
    \code{\link{dissimilarity.object}}), representing the total
    dissimilarity matrix of the dataset.
  }
  \item{data}{
    a matrix containing the original or standardized measurements, depending
    on the \code{stand} option of the function \code{agnes}. If a
    dissimilarity matrix was given as input structure, then this
    component is not available.
  }
}
\seealso{
  \code{\link{agnes}}, \code{\link{diana}},
  \code{\link{as.hclust}}, \code{\link{hclust}},
  \code{\link{plot.agnes}}, \code{\link{twins.object}}.
}
\keyword{cluster}
