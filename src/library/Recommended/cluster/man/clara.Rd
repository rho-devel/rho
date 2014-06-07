\name{clara}
\alias{clara}
\title{Clustering Large Applications}
\description{
  Computes a \code{"clara"} object, a list representing a clustering of
  the data into \code{k} clusters.
}
\usage{
clara(x, k, metric = "euclidean", stand = FALSE, samples = 5,
      sampsize = min(n, 40 + 2 * k), trace = 0, medoids.x = TRUE,
      keep.data = medoids.x, rngR = FALSE, pamLike = FALSE)
}
\arguments{
  \item{x}{
    data matrix or data frame, each row corresponds to an observation,
    and each column corresponds to a variable.  All variables must be numeric.
    Missing values (NAs) are allowed.}
  \item{k}{integer, the number of clusters.
    It is required that \eqn{0 < k < n} where \eqn{n} is the number of
    observations (i.e., n = \code{nrow(x)}).}
  \item{metric}{
    character string specifying the metric to be used for calculating
    dissimilarities between observations.
    The currently available options are "euclidean" and "manhattan".
    Euclidean distances are root sum-of-squares of differences, and
    manhattan distances are the sum of absolute differences.
  }
  \item{stand}{logical, indicating if the measurements in \code{x} are
    standardized before calculating the dissimilarities.  Measurements
    are standardized for each variable (column), by subtracting the
    variable's mean value and dividing by the variable's mean absolute
    deviation.
  }
  \item{samples}{integer, number of samples to be drawn from the
    dataset.  The default, \code{5}, is rather small for historical (and
    now back compatibility) reasons and we recommend to set
    \code{samples} an order of magnitude larger.
  }
  \item{sampsize}{integer, number of observations in each
    sample. \code{sampsize} should be higher than the number of clusters
    (\code{k}) and at most the number of observations (n = \code{nrow(x)}).}
  \item{trace}{integer indicating a \emph{trace level} for diagnostic
    output during the algorithm.}
  \item{medoids.x}{logical indicating if the medoids should be
    returned, identically to some rows of the input data \code{x}.  If
    \code{FALSE}, \code{keep.data} must be false as well, and the medoid
    indices, i.e., row numbers of the medoids will still be returned
    (\code{i.med} component), and the algorithm saves space by needing
    one copy less of \code{x}.}
  \item{keep.data}{logical indicating if the (\emph{scaled} if
    \code{stand} is true) data should be kept in the result.
%     (\code{keepdata} is equivalent to \code{keep.data} where the former
%     is deprecated.)
    Setting this to \code{FALSE} saves memory (and hence time), but
    disables \code{\link{clusplot}()}ing of the result.  Use
    \code{medoids.x = FALSE} to save even more memory.}
  \item{rngR}{logical indicating if \R's random number generator should
    be used instead of the primitive clara()-builtin one.  If true, this
    also means that each call to \code{clara()} returns a different result
    -- though only slightly different in good situations.}
  \item{pamLike}{logical indicating if the \dQuote{swap} phase (see
    \code{\link{pam}}, in C code) should use the same algorithm as
    \code{\link{pam}()}.  Note that from Kaufman and Rousseeuw's
    description this \emph{should} have been true always, but as the
    original Fortran code and the subsequent port to C has always
    contained a small one-letter change (a typo according to Martin Maechler)
    with respect to PAM, the default, \code{pamLike = FALSE} has been chosen to
    remain back compatible rather than \dQuote{PAM compatible}.}
}
\value{
  an object of class \code{"clara"} representing the clustering.  See
  \code{\link{clara.object}} for details.
}
\details{
  \code{clara} is fully described in chapter 3 of Kaufman and Rousseeuw (1990).
  Compared to other partitioning methods such as \code{pam}, it can deal with
  much larger datasets.  Internally, this is achieved by considering
  sub-datasets of fixed size (\code{sampsize}) such that the time and
  storage requirements become linear in \eqn{n} rather than quadratic.

  Each sub-dataset is partitioned into \code{k} clusters using the same
  algorithm as in \code{\link{pam}}.\cr
  Once \code{k} representative objects have been selected from the
  sub-dataset, each observation of the entire dataset is assigned
  to the nearest medoid.

  The mean (equivalent to the sum) of the dissimilarities of the
  observations to their closest medoid is used as a measure of the
  quality of the clustering.  The sub-dataset for which the mean (or
  sum) is minimal, is retained.  A further analysis is carried out on
  the final partition.

  Each sub-dataset is forced to contain the medoids obtained from the
  best sub-dataset until then.  Randomly drawn observations are added to
  this set until \code{sampsize} has been reached.
}
\note{
%% mostly by Martin Maechler :
  By default, the random sampling is implemented with a \emph{very}
  simple scheme (with period \eqn{2^{16} = 65536}) inside the Fortran
  code, independently of \R's random number generation, and as a matter
  of fact, deterministically.  Alternatively, we recommend setting
  \code{rngR = TRUE} which uses \R's random number generators.  Then,
  \code{clara()} results are made reproducible typically by using
  \code{\link{set.seed}()} before calling \code{clara}.

  The storage requirement of \code{clara} computation (for small
  \code{k}) is about
  \eqn{O(n \times p) + O(j^2)}{O(n * p) + O(j^2)} where
  \eqn{j = \code{sampsize}}, and \eqn{(n,p) = \code{dim(x)}}.
  The CPU computing time (again assuming small \code{k}) is about
  \eqn{O(n \times p \times j^2 \times N)}{O(n * p * j^2 * N)}, where
  \eqn{N = \code{samples}}.

  For \dQuote{small} datasets, the function \code{\link{pam}} can be used
  directly.  What can be considered \emph{small}, is really a function
  of available computing power, both memory (RAM) and speed.
  Originally (1990), \dQuote{small} meant less than 100 observations;
  in 1997, the authors said \emph{\dQuote{small (say with fewer than 200
  observations)}}; as of 2006, you can use \code{\link{pam}} with
  several thousand observations.
}
\author{
  Kaufman and Rousseeuw (see \code{\link{agnes}}), originally.
  All arguments from \code{trace} on, and most \R documentation and all
  tests by Martin Maechler.
}
\seealso{
  \code{\link{agnes}} for background and references;
  \code{\link{clara.object}}, \code{\link{pam}},
  \code{\link{partition.object}}, \code{\link{plot.partition}}.
}
\examples{
## generate 500 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))
clarax <- clara(x, 2, samples=50)
clarax
clarax$clusinfo
## using pamLike=TRUE  gives the same (apart from the 'call'):
all.equal(clarax[-8],
          clara(x, 2, samples=50, pamLike = TRUE)[-8])
plot(clarax)

## `xclara' is an artificial data set with 3 clusters of 1000 bivariate
## objects each.
data(xclara)
(clx3 <- clara(xclara, 3))
## "better" number of samples
cl.3 <- clara(xclara, 3, samples=100)
## but that did not change the result here:
stopifnot(cl.3$clustering == clx3$clustering)
## Plot similar to Figure 5 in Struyf et al (1996)
\dontrun{plot(clx3, ask = TRUE)}
\dontshow{plot(clx3)}

## Try 100 times *different* random samples -- for reliability:
nSim <- 100
nCl <- 3 # = no.classes
set.seed(421)# (reproducibility)
cl <- matrix(NA,nrow(xclara), nSim)
for(i in 1:nSim)
   cl[,i] <- clara(xclara, nCl, medoids.x = FALSE, rngR = TRUE)$cluster
tcl <- apply(cl,1, tabulate, nbins = nCl)
## those that are not always in same cluster (5 out of 3000 for this seed):
(iDoubt <- which(apply(tcl,2, function(n) all(n < nSim))))
if(length(iDoubt)) { # (not for all seeds)
  tabD <- tcl[,iDoubt, drop=FALSE]
  dimnames(tabD) <- list(cluster = paste(1:nCl), obs = format(iDoubt))
  t(tabD) # how many times in which clusters
}

}
\keyword{cluster}
