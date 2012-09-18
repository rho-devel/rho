\name{pam}
\alias{pam}
\title{Partitioning Around Medoids}
\description{
  Partitioning (clustering) of the data into \code{k} clusters ``around
  medoids'', a more robust version of K-means.
}
\usage{
pam(x, k, diss = inherits(x, "dist"), metric = "euclidean",
    medoids = NULL, stand = FALSE, cluster.only = FALSE,
    do.swap = TRUE,
    keep.diss = !diss && !cluster.only && n < 100,
    keep.data = !diss && !cluster.only,
    pamonce = FALSE, trace.lev = 0)
}
\arguments{
  \item{x}{
    data matrix or data frame, or dissimilarity matrix or object,
    depending on the value of the \code{diss} argument.

    In case of a matrix or data frame, each row corresponds to an
    observation, and each column corresponds to a variable.  All
    variables must be numeric.  Missing values (\code{\link{NA}}s)
    \emph{are} allowed---as long as every pair of observations has at
    least one case not missing.

    In case of a dissimilarity matrix, \code{x} is typically the output
    of \code{\link{daisy}} or \code{\link{dist}}.  Also a vector of
    length n*(n-1)/2 is allowed (where n is the number of observations),
    and will be interpreted in the same way as the output of the
    above-mentioned functions. Missing values (NAs) are \emph{not}
    allowed.
  }
  \item{k}{positive integer specifying the number of clusters, less than
    the number of observations.}
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
  \item{medoids}{NULL (default) or length-\code{k} vector of integer
    indices (in \code{1:n}) specifying initial medoids instead of using
    the \sQuote{\emph{build}} algorithm.}
  \item{stand}{logical; if true, the measurements in \code{x} are
    standardized before calculating the dissimilarities.  Measurements
    are standardized for each variable (column), by subtracting the
    variable's mean value and dividing by the variable's mean absolute
    deviation.  If \code{x} is already a dissimilarity matrix, then this
    argument will be ignored.}
  \item{cluster.only}{logical; if true, only the clustering will be
    computed and returned, see details.}
  \item{do.swap}{logical indicating if the \bold{swap} phase should
    happen. The default, \code{TRUE}, correspond to the
    original algorithm.  On the other hand, the \bold{swap} phase is
    much more computer intensive than the \bold{build} one for large
    \eqn{n}, so can be skipped by \code{do.swap = FALSE}.}
  \item{keep.diss, keep.data}{logicals indicating if the dissimilarities
    and/or input data \code{x} should be kept in the result.  Setting
    these to \code{FALSE} can give much smaller results and hence even save
    memory allocation \emph{time}.}
  \item{pamonce}{logical or integer in \code{0:2} specifying algorithmic
    short cuts as proposed by Reynolds et al. (2006), see below.}
  \item{trace.lev}{integer specifying a trace level for printing
    diagnostics during the build and swap phase of the algorithm.
    Default \code{0} does not print anything; higher values print
    increasingly more.}
}
\value{
  an object of class \code{"pam"} representing the clustering.  See
  \code{?\link{pam.object}} for details.
}
\details{
  \code{pam} is fully described in chapter 2 of Kaufman and Rousseeuw
  (1990).  Compared to the k-means approach in \code{kmeans}, the
  function \code{pam} has the following features: (a) it also accepts a
  dissimilarity matrix; (b) it is more robust because it minimizes a sum
  of dissimilarities instead of a sum of squared euclidean distances;
  (c) it provides a novel graphical display, the silhouette plot (see
  \code{plot.partition}) (d) it allows to select the number of clusters
  using \code{mean(\link{silhouette}(pr))} on the result
  \code{pr <- pam(..)}, or directly its component
  \code{pr$silinfo$avg.width}, see also \code{\link{pam.object}}.

  When \code{cluster.only} is true, the result is simply a (possibly
  named) integer vector specifying the clustering, i.e.,\cr
  \code{pam(x,k, cluster.only=TRUE)} is the same as \cr
  \code{pam(x,k)$clustering} but computed more efficiently.

  The \code{pam}-algorithm is based on the search for \code{k}
  representative objects or medoids among the observations of the
  dataset.  These observations should represent the structure of the
  data.  After finding a set of \code{k} medoids, \code{k} clusters are
  constructed by assigning each observation to the nearest medoid.  The
  goal is to find \code{k} representative objects which minimize the sum
  of the dissimilarities of the observations to their closest
  representative object.
  \cr
  By default, when \code{medoids} are not specified, the algorithm first
  looks for a good initial set of medoids (this is called the
  \bold{build} phase).  Then it finds a local minimum for the
  objective function, that is, a solution such that there is no single
  switch of an observation with a medoid that will decrease the
  objective (this is called the \bold{swap} phase).

  When the \code{medoids} are specified, their order does \emph{not}
  matter; in general, the algorithms have been designed to not depend on
  the order of the observations.

  The \code{pamonce} option, new in cluster 1.14.2 (Jan. 2012), has been
  proposed by Matthias Studer, University of Geneva, based on the
  findings by Reynolds et al. (2006).

  The default \code{FALSE} (or integer \code{0}) corresponds to the
  original \dQuote{swap} algorithm, whereas \code{pamonce = 1} (or
  \code{TRUE}), corresponds to the first proposal .... %% FIXME
  and \code{pamonce = 2} additionally implements the second proposal as
  well. % FIXME more details
}
\note{
  For large datasets, \code{pam} may need too much memory or too much
  computation time since both are \eqn{O(n^2)}.  Then,
  \code{\link{clara}()} is preferable, see its documentation.
}
\author{Kaufman and Rousseeuw's orginal Fortran code was translated to C
  and augmented in several ways, e.g. to allow \code{cluster.only=TRUE}
  or \code{do.swap=FALSE}, by Martin Maechler.
  \cr
  Matthias Studer, Univ.Geneva provided the \code{pamonce}
  implementation.
}
\references{
%% the pamonce options :
  Reynolds, A., Richards, G., de la Iglesia, B. and Rayward-Smith, V. (1992)
  Clustering rules: A comparison of partitioning and hierarchical
  clustering algorithms;
  \emph{Journal of Mathematical Modelling and Algorithms} \bold{5},
  475--504 (\url{http://dx.doi.org/10.1007/s10852-005-9022-1}).

}
\seealso{
  \code{\link{agnes}} for background and references;
  \code{\link{pam.object}}, \code{\link{clara}}, \code{\link{daisy}},
  \code{\link{partition.object}}, \code{\link{plot.partition}},
  \code{\link{dist}}.
}
\examples{
## generate 25 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
pamx <- pam(x, 2)
pamx
summary(pamx)
plot(pamx)
## use obs. 1 & 16 as starting medoids -- same result (typically)
(p2m <- pam(x, 2, medoids = c(1,16)))

p3m <- pam(x, 3, trace = 2)
## rather stupid initial medoids:
(p3m. <- pam(x, 3, medoids = 3:1, trace = 1))

\dontshow{
 ii <- pmatch(c("obj","call"), names(pamx))
 stopifnot(all.equal(pamx [-ii],  p2m [-ii],  tol=1e-14),
           all.equal(pamx$objective[2], p2m$objective[2], tol=1e-14))
}
pam(daisy(x, metric = "manhattan"), 2, diss = TRUE)

data(ruspini)
## Plot similar to Figure 4 in Stryuf et al (1996)
\dontrun{plot(pam(ruspini, 4), ask = TRUE)}
\dontshow{plot(pam(ruspini, 4))}
}
\keyword{cluster}
