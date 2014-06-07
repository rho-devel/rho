\name{diana}
\title{DIvisive ANAlysis Clustering}
\alias{diana}
\alias{diana.object}
\description{
  Computes a divisive hierarchical clustering of the dataset
  returning an object of class \code{diana}.
}
\usage{
diana(x, diss = inherits(x, "dist"), metric = "euclidean", stand = FALSE,
      keep.diss = n < 100, keep.data = !diss, trace.lev = 0)
}
\arguments{
  \item{x}{
    data matrix or data frame, or dissimilarity matrix or object,
    depending on the value of the \code{diss} argument.

    In case of a matrix or data frame, each row corresponds to an observation,
    and each column corresponds to a variable.  All variables must be numeric.
    Missing values (\code{\link{NA}}s) \emph{are} allowed.

    In case of a dissimilarity matrix, \code{x} is typically the output
    of \code{\link{daisy}} or \code{\link{dist}}.  Also a vector of
    length n*(n-1)/2 is allowed (where n is the number of observations),
    and will be interpreted in the same way as the output of the
    above-mentioned functions. Missing values (NAs) are \emph{not} allowed.
  }
  \item{diss}{
    logical flag: if TRUE (default for \code{dist} or
    \code{dissimilarity} objects), then \code{x} will be considered as a
    dissimilarity matrix.  If FALSE, then \code{x} will be considered as
    a matrix of observations by variables.
  }
  \item{metric}{
    character string specifying the metric to be used for calculating
    dissimilarities between observations.\cr
    The currently available options are "euclidean" and
    "manhattan".  Euclidean distances are root sum-of-squares of
    differences, and manhattan distances are the sum of absolute
    differences.  If \code{x} is already a dissimilarity matrix, then
    this argument will be ignored.
  }
  \item{stand}{logical; if true, the measurements in \code{x} are
    standardized before calculating the dissimilarities.  Measurements
    are standardized for each variable (column), by subtracting the
    variable's mean value and dividing by the variable's mean absolute
    deviation.  If \code{x} is already a dissimilarity matrix, then this
    argument will be ignored.}
  \item{keep.diss, keep.data}{logicals indicating if the dissimilarities
    and/or input data \code{x} should be kept in the result.  Setting
    these to \code{FALSE} can give much smaller results and hence even save
    memory allocation \emph{time}.}
  \item{trace.lev}{integer specifying a trace level for printing
    diagnostics during the algorithm.  Default \code{0} does not print
    anything; higher values print increasingly more.}
}
\value{
  an object of class \code{"diana"} representing the clustering;
  this class has methods for the following generic functions:
  \code{print}, \code{summary}, \code{plot}.

  Further, the class \code{"diana"} inherits from
  \code{"twins"}.  Therefore, the generic function \code{\link{pltree}} can be
  used on a \code{diana} object, and \code{\link{as.hclust}} and
  \code{\link{as.dendrogram}} methods are available.

  A legitimate \code{diana} object is a list with the following components:
  \item{order}{
    a vector giving a permutation of the original observations to allow for
    plotting, in the sense that the branches of a clustering tree will
    not cross.
  }
  \item{order.lab}{
    a vector similar to \code{order}, but containing observation labels
    instead of observation numbers.  This component is only available if
    the original observations were labelled.
  }
  \item{height}{a vector with the diameters of the clusters prior to splitting.
  }
  \item{dc}{
    the divisive coefficient, measuring the clustering structure of the
    dataset.  For each observation i, denote by \eqn{d(i)} the diameter of the
    last cluster to which it belongs (before being split off as a single
    observation), divided by the diameter of the whole dataset.  The
    \code{dc} is the average of all \eqn{1 - d(i)}.  It can also be seen
    as the average width (or the percentage filled) of the banner plot.
    Because \code{dc} grows with the number of observations, this
    measure should not be used to compare datasets of very different
    sizes.
  }
  \item{merge}{
    an (n-1) by 2 matrix, where n is the number of
    observations. Row i of \code{merge} describes the split at step n-i of
    the clustering. If a number \eqn{j} in row r is negative, then the single
    observation \eqn{|j|} is split off at stage n-r. If j is positive, then the
    cluster that will be splitted at stage n-j (described by row j), is
    split off at stage n-r.
  }
  \item{diss}{
    an object of class \code{"dissimilarity"}, representing the total
    dissimilarity matrix of the dataset.
  }
  \item{data}{
    a matrix containing the original or standardized measurements, depending
    on the \code{stand} option of the function \code{agnes}.  If a
    dissimilarity matrix was given as input structure, then this component
    is not available.
  }

}
\details{
\code{diana} is fully described in chapter 6 of Kaufman and Rousseeuw (1990).
It is probably unique in computing a divisive hierarchy, whereas most
other software for hierarchical clustering is agglomerative.
Moreover, \code{diana} provides (a) the divisive coefficient
(see \code{diana.object}) which measures the amount of clustering structure
found; and (b) the banner, a novel graphical display
(see \code{plot.diana}).

The \code{diana}-algorithm constructs a hierarchy of clusterings,
starting with one large
cluster containing all n observations. Clusters are divided until each cluster
contains only a single observation.\cr
At each stage, the cluster with the largest diameter is selected.
(The diameter of a cluster is the largest dissimilarity between any
two of its observations.)\cr
To divide the selected cluster, the algorithm first looks for its most
disparate observation (i.e., which has the largest average dissimilarity to the
other observations of the selected cluster). This observation initiates the
"splinter group". In subsequent steps, the algorithm reassigns observations
that are closer to the "splinter group" than to the "old party". The result
is a division of the selected cluster into two new clusters.
}
\seealso{
  \code{\link{agnes}} also for background and references;
  \code{\link{cutree}} (and \code{\link{as.hclust}}) for grouping
  extraction; \code{\link{daisy}}, \code{\link{dist}},
  \code{\link{plot.diana}}, \code{\link{twins.object}}.
}
\examples{
data(votes.repub)
dv <- diana(votes.repub, metric = "manhattan", stand = TRUE)
print(dv)
plot(dv)

## Cut into 2 groups:
dv2 <- cutree(as.hclust(dv), k = 2)
table(dv2) # 8 and 42 group members
rownames(votes.repub)[dv2 == 1]

## For two groups, does the metric matter ?
dv0 <- diana(votes.repub, stand = TRUE) # default: Euclidean
dv.2 <- cutree(as.hclust(dv0), k = 2)
table(dv2 == dv.2)## identical group assignments

str(as.dendrogram(dv0)) # {via as.dendrogram.twins() method}

data(agriculture)
## Plot similar to Figure 8 in ref
\dontrun{plot(diana(agriculture), ask = TRUE)}
\dontshow{plot(diana(agriculture))}
}
\keyword{cluster}
