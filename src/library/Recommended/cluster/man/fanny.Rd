\name{fanny}
\alias{fanny}
\title{Fuzzy Analysis Clustering}
\description{
  Computes a fuzzy clustering of the data into \code{k} clusters.
}
\usage{
fanny(x, k, diss = inherits(x, "dist"), memb.exp = 2,
      metric = c("euclidean", "manhattan", "SqEuclidean"),
      stand = FALSE, iniMem.p = NULL, cluster.only = FALSE,
      keep.diss = !diss && !cluster.only && n < 100,
      keep.data = !diss && !cluster.only,
      maxit = 500, tol = 1e-15, trace.lev = 0)
}
\arguments{
  \item{x}{
    data matrix or data frame, or dissimilarity matrix, depending on the
    value of the \code{diss} argument.

    In case of a matrix or data frame, each row corresponds to an observation,
    and each column corresponds to a variable. All variables must be numeric.
    Missing values (NAs) are allowed.

    In case of a dissimilarity matrix, \code{x} is typically the output
    of \code{\link{daisy}} or \code{\link{dist}}.  Also a vector of
    length n*(n-1)/2 is allowed (where n is the number of observations),
    and will be interpreted in the same way as the output of the
    above-mentioned functions.  Missing values (NAs) are not allowed.
  }
  \item{k}{integer giving the desired number of clusters.  It is
    required that \eqn{0 < k < n/2} where \eqn{n} is the number of
    observations.}
  \item{diss}{
    logical flag: if TRUE (default for \code{dist} or
    \code{dissimilarity} objects), then \code{x} is assumed to be a
    dissimilarity matrix.  If FALSE, then \code{x} is treated as
    a matrix of observations by variables.
  }
  \item{memb.exp}{number \eqn{r} strictly larger than 1 specifying the
    \emph{membership exponent} used in the fit criterion; see the
    \sQuote{Details} below. Default: \code{2} which used to be hardwired
    inside FANNY.}
  \item{metric}{character string specifying the metric to be used for
    calculating dissimilarities between observations.  Options are
    \code{"euclidean"} (default), \code{"manhattan"}, and
    \code{"SqEuclidean"}.  Euclidean distances are root sum-of-squares
    of differences, and manhattan distances are the sum of absolute
    differences, and \code{"SqEuclidean"}, the \emph{squared} euclidean
    distances are sum-of-squares of differences.  Using this last option is
    equivalent (but somewhat slower) to computing so called \dQuote{fuzzy C-means}.
    \cr
    If \code{x} is already a dissimilarity matrix, then this argument will
    be ignored.
  }
  \item{stand}{logical; if true, the measurements in \code{x} are
    standardized before calculating the dissimilarities.  Measurements
    are standardized for each variable (column), by subtracting the
    variable's mean value and dividing by the variable's mean absolute
    deviation.  If \code{x} is already a dissimilarity matrix, then this
    argument will be ignored.}
  \item{iniMem.p}{numeric \eqn{n \times k}{n * k} matrix or \code{NULL}
    (by default); can be used to specify a starting \code{membership}
    matrix, i.e., a matrix of non-negative numbers, each row summing to
    one.
  } %% FIXME: add example
  \item{cluster.only}{logical; if true, no silhouette information will be
    computed and returned, see details.}%% FIXME: add example
  \item{keep.diss, keep.data}{logicals indicating if the dissimilarities
    and/or input data \code{x} should be kept in the result.  Setting
    these to \code{FALSE} can give smaller results and hence also save
    memory allocation \emph{time}.}
  \item{maxit, tol}{maximal number of iterations and default tolerance
    for convergence (relative convergence of the fit criterion) for the
    FANNY algorithm.  The defaults \code{maxit = 500} and \code{tol =
      1e-15} used to be hardwired inside the algorithm.}
  \item{trace.lev}{integer specifying a trace level for printing
    diagnostics during the C-internal algorithm.
    Default \code{0} does not print anything; higher values print
    increasingly more.}
}
\value{
  an object of class \code{"fanny"} representing the clustering.
  See \code{\link{fanny.object}} for details.
}
\details{
  In a fuzzy clustering, each observation is \dQuote{spread out} over
  the various clusters.  Denote by \eqn{u_{iv}}{u(i,v)} the membership
  of observation \eqn{i} to cluster \eqn{v}.

  The memberships are nonnegative, and for a fixed observation i they sum to 1.
  The particular method \code{fanny} stems from chapter 4 of
  Kaufman and Rousseeuw (1990) (see the references in
  \code{\link{daisy}}) and has been extended by Martin Maechler to allow
  user specified \code{memb.exp}, \code{iniMem.p}, \code{maxit},
  \code{tol}, etc.

  Fanny aims to minimize the objective function
  \deqn{\sum_{v=1}^k
    \frac{\sum_{i=1}^n\sum_{j=1}^n u_{iv}^r u_{jv}^r d(i,j)}{
      2 \sum_{j=1}^n u_{jv}^r}}{%
    SUM_[v=1..k] (SUM_(i,j) u(i,v)^r u(j,v)^r d(i,j)) / (2 SUM_j u(j,v)^r)}
  where \eqn{n} is the number of observations, \eqn{k} is the number of
  clusters, \eqn{r} is the membership exponent \code{memb.exp} and
  \eqn{d(i,j)} is the dissimilarity between observations \eqn{i} and \eqn{j}.
  \cr Note that \eqn{r \to 1}{r -> 1} gives increasingly crisper
  clusterings whereas \eqn{r \to \infty}{r -> Inf} leads to complete
  fuzzyness.  K\&R(1990), p.191 note that values too close to 1 can lead
  to slow convergence.  Further note that even the default, \eqn{r = 2}
  can lead to complete fuzzyness, i.e., memberships \eqn{u_{iv} \equiv
    1/k}{u(i,v) == 1/k}.  In that case a warning is signalled and the
  user is advised to chose a smaller \code{memb.exp} (\eqn{=r}).

  Compared to other fuzzy clustering methods, \code{fanny} has the following
  features: (a) it also accepts a dissimilarity matrix; (b) it is
  more robust to the \code{spherical cluster} assumption; (c) it provides
  a novel graphical display, the silhouette plot (see
  \code{\link{plot.partition}}).
}
\seealso{
  \code{\link{agnes}} for background and references;
  \code{\link{fanny.object}}, \code{\link{partition.object}},
  \code{\link{plot.partition}}, \code{\link{daisy}}, \code{\link{dist}}.
}
\examples{
## generate 10+15 objects in two clusters, plus 3 objects lying
## between those clusters.
x <- rbind(cbind(rnorm(10, 0, 0.5), rnorm(10, 0, 0.5)),
           cbind(rnorm(15, 5, 0.5), rnorm(15, 5, 0.5)),
           cbind(rnorm( 3,3.2,0.5), rnorm( 3,3.2,0.5)))
fannyx <- fanny(x, 2)
## Note that observations 26:28 are "fuzzy" (closer to # 2):
fannyx
summary(fannyx)
plot(fannyx)

(fan.x.15 <- fanny(x, 2, memb.exp = 1.5)) # 'crispier' for obs. 26:28
(fanny(x, 2, memb.exp = 3))               # more fuzzy in general

data(ruspini)
f4 <- fanny(ruspini, 4)
stopifnot(rle(f4$clustering)$lengths == c(20,23,17,15))
plot(f4, which = 1)
## Plot similar to Figure 6 in Stryuf et al (1996)
plot(fanny(ruspini, 5))
}
\keyword{cluster}
