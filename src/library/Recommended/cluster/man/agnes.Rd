\name{agnes}
\alias{agnes}
\title{Agglomerative Nesting (Hierarchical Clustering)}
\concept{UPGMA clustering}
\description{
  Computes agglomerative hierarchical clustering of the dataset.
}
\usage{
agnes(x, diss = inherits(x, "dist"), metric = "euclidean",
      stand = FALSE, method = "average", par.method,
      keep.diss = n < 100, keep.data = !diss)
}
\arguments{
  \item{x}{
    data matrix or data frame, or dissimilarity matrix, depending on the
    value of the \code{diss} argument.

    In case of a matrix or data frame, each row corresponds to an observation,
    and each column corresponds to a variable. All variables must be numeric.
    Missing values (NAs) are allowed.

    In case of a dissimilarity matrix, \code{x} is typically the output of
    \code{\link{daisy}} or \code{\link{dist}}.
    Also a vector with length n*(n-1)/2 is allowed (where n is the number
    of observations), and will be interpreted in the same way as the
    output of the above-mentioned functions. Missing values (NAs) are not
    allowed.
  }
  \item{diss}{
    logical flag: if TRUE (default for \code{dist} or
    \code{dissimilarity} objects), then \code{x} is assumed to be a
    dissimilarity matrix.  If FALSE, then \code{x} is treated as
    a matrix of observations by variables.
  }
  \item{metric}{
    character string specifying the metric to be used for calculating
    dissimilarities between observations.
    The currently available options are "euclidean" and "manhattan".
    Euclidean distances are root sum-of-squares of differences, and
    manhattan distances are the sum of absolute differences.
    If \code{x} is already a dissimilarity matrix, then this argument will
    be ignored.
  }
  \item{stand}{
    logical flag: if TRUE, then the measurements in \code{x} are
    standardized before calculating the dissimilarities. Measurements
    are standardized for each variable (column), by subtracting the
    variable's mean value and dividing by the variable's mean absolute
    deviation.  If \code{x} is already a dissimilarity matrix, then this
    argument will be ignored.
  }
  \item{method}{
    character string defining the clustering method.  The six methods
    implemented are "average" ([unweighted pair-]group average method, UPGMA),
    "single" (single linkage), "complete" (complete linkage),
    "ward" (Ward's method), "weighted" (weighted average linkage) and
    its generalization \code{"flexible"} which uses (a constant version of)
    the Lance-Williams formula and the \code{par.method} argument.  Default
    is "average".
  }
  \item{par.method}{if \code{method == "flexible"}, numeric vector of
    length 1, 3, or 4, see in the details section.
  }
  \item{keep.diss, keep.data}{logicals indicating if the dissimilarities
    and/or input data \code{x} should be kept in the result.  Setting
    these to \code{FALSE} can give much smaller results and hence even save
    memory allocation \emph{time}.}
}
\value{
  an object of class \code{"agnes"} (which extends \code{"twins"})
  representing the clustering.  See \code{\link{agnes.object}} for
  details, and methods applicable.
}
\details{
  \code{agnes} is fully described in chapter 5 of Kaufman and Rousseeuw (1990).
  Compared to other agglomerative clustering methods such as \code{hclust},
  \code{agnes} has the following features: (a) it yields the
  agglomerative coefficient (see \code{\link{agnes.object}})
  which measures the amount of clustering structure found; and (b)
  apart from the usual tree it also provides the banner, a novel
  graphical display (see \code{\link{plot.agnes}}).

  The \code{agnes}-algorithm constructs a hierarchy of clusterings.\cr
  At first, each observation is a small cluster by itself.  Clusters are
  merged until only one large cluster remains which contains all the
  observations.  At each stage the two \emph{nearest} clusters are combined
  to form one larger cluster.

  For \code{method="average"}, the distance between two clusters is the
  average of the dissimilarities between the points in one cluster and the
  points in the other cluster.
  \cr
  In \code{method="single"}, we use the smallest dissimilarity between a
  point in the first cluster and a point in the second cluster (nearest
  neighbor method).
  \cr
  When \code{method="complete"}, we use the largest dissimilarity
  between a point in the first cluster and a point in the second cluster
  (furthest neighbor method).

  The \code{method = "flexible"} allows (and requires) more details:
  The Lance-Williams formula specifies how dissimilarities are
  computed when clusters are agglomerated (equation (32) in K.\&R.,
  p.237).  If clusters \eqn{C_1} and \eqn{C_2} are agglomerated into a
  new cluster, the dissimilarity between their union and another
  cluster \eqn{Q} is given by
  \deqn{
    D(C_1 \cup C_2, Q) = \alpha_1 * D(C_1, Q) + \alpha_2 * D(C_2, Q) +
                         \beta * D(C_1,C_2) + \gamma * |D(C_1, Q) - D(C_2, Q)|,
  }
  where the four coefficients \eqn{(\alpha_1, \alpha_2, \beta, \gamma)}
  are specified by the vector \code{par.method}:

  If \code{par.method} is of length 1,
  say \eqn{= \alpha}, \code{par.method} is extended to
  give the \dQuote{Flexible Strategy} (K. \& R., p.236 f) with
  Lance-Williams coefficients \eqn{(\alpha_1 = \alpha_2 = \alpha, \beta =
    1 - 2\alpha, \gamma=0)}.\cr
  If of length 3, \eqn{\gamma = 0} is used.

  \bold{Care} and expertise is probably needed when using \code{method
    = "flexible"} particularly for the case when \code{par.method} is
  specified of longer length than one.
  The \emph{weighted average} (\code{method="weighted"}) is the same as
  \code{method="flexible", par.method = 0.5}.
}
\section{BACKGROUND}{
  Cluster analysis divides a dataset into groups (clusters) of
  observations that are similar to each other.
  \describe{
    \item{Hierarchical methods}{like
      \code{agnes}, \code{\link{diana}}, and \code{\link{mona}}
      construct a hierarchy of clusterings, with the number of clusters
      ranging from one to the number of observations.}
    \item{Partitioning methods}{like
      \code{\link{pam}}, \code{\link{clara}}, and \code{\link{fanny}}
      require that the number of clusters be given by the user.}
    }
}
\references{
  Kaufman, L. and Rousseeuw, P.J. (1990).
  \emph{Finding Groups in Data: An Introduction to Cluster Analysis}.
  Wiley, New York.

  Anja Struyf, Mia Hubert & Peter J. Rousseeuw (1996)
  Clustering in an Object-Oriented Environment.
  \emph{Journal of Statistical Software} \bold{1}.
  \url{http://www.jstatsoft.org/v01/i04}

  Struyf, A., Hubert, M. and Rousseeuw, P.J. (1997). Integrating
  Robust Clustering Techniques in S-PLUS,
  \emph{Computational Statistics and Data Analysis}, \bold{26}, 17--37.

  Lance, G.N., and W.T. Williams (1966).
  A General Theory of Classifactory Sorting Strategies, I. Hierarchical
  Systems.
  \emph{Computer J.} \bold{9}, 373--380.

}
\seealso{
  \code{\link{agnes.object}}, \code{\link{daisy}}, \code{\link{diana}},
  \code{\link{dist}}, \code{\link{hclust}}, \code{\link{plot.agnes}},
  \code{\link{twins.object}}.
}
\examples{
data(votes.repub)
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)

op <- par(mfrow=c(2,2))
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)
agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.6)
plot(agnS)
par(op)

## Exploring the dendrogram structure
(d2 <- as.dendrogram(agn2)) # two main branches
d2[[1]] # the first branch
d2[[2]] # the 2nd one  { 8 + 42  = 50 }
d2[[1]][[1]]# first sub-branch of branch 1 .. and shorter form
identical(d2[[c(1,1)]],
          d2[[1]][[1]])
## a "textual picture" of the dendrogram :
str(d2)

data(agriculture)

## Plot similar to Figure 7 in ref
\dontrun{plot(agnes(agriculture), ask = TRUE)}
\dontshow{plot(agnes(agriculture))}
}
\keyword{cluster}
