\name{clusGap}
\title{Gap Statistic for Estimating the Number of Clusters}
\alias{clusGap}
\alias{maxSE}
\alias{print.clusGap}
\alias{plot.clusGap}
\description{
  \code{clusGap()} calculates a goodness of clustering measure, the
  \dQuote{gap} statistic.  For each number of clusters \eqn{k}, it
  compares \eqn{\log(W(k))} with \eqn{E^*[\log(W(k))]} where the latter
  is defined via bootstrapping, i.e. simulating from a reference
  distribution.

  \code{maxSE(f, SE.f)} determines the location of the \bold{maximum}
  of \code{f}, taking a \dQuote{1-SE rule} into account for the
  \code{*SE*} methods.  The default method \code{"firstSEmax"} looks for
  the smallest \eqn{k} such that its value \eqn{f(k)} is not more than 1
  standard error away from the first local maximum.
  This is similar but not the same as \code{"Tibs2001SEmax"}, Tibshirani
  et al's recommendation of determining the number of clusters from the
  gap statistics and their standard deviations.
}
\usage{
clusGap(x, FUNcluster, K.max, B = 100, verbose = interactive(), \dots)

maxSE(f, SE.f,
      method = c("firstSEmax", "Tibs2001SEmax", "globalSEmax",
                 "firstmax", "globalmax"),
      SE.factor = 1)
\S3method{print}{clusGap}(x, method = "firstSEmax", SE.factor = 1, \dots)
}
\arguments{
  \item{x}{numeric matrix or \code{\link{data.frame}}.}
  \item{FUNcluster}{a \code{\link{function}} which accepts as first
    argument a (data) matrix like \code{x}, second argument, say
    \eqn{k, k\geq 2}{k, k >= 2}, the number of clusters desired,
    and returns a \code{\link{list}} with a component named (or shortened to)
    \code{cluster} which is a vector of length \code{n = nrow(x)} of
    integers in \code{1:k} determining the clustering or grouping of the
    \code{n} observations.}
  \item{K.max}{the maximum number of clusters to consider, must be at
    least two.}
  \item{B}{integer, number of Monte Carlo (\dQuote{bootstrap}) samples.}
  \item{verbose}{integer or logical, determining if \dQuote{progress}
    output should be printed.  The default prints one bit per bootstrap
    sample.}
  \item{\dots}{optionally further arguments for \code{FUNcluster()}, see
    \code{kmeans} example below.}
  \item{f}{numeric vector of \sQuote{function values}, of length
      \eqn{K}, whose (\dQuote{1 SE respected}) maximum we want.}
  \item{SE.f}{numeric vector of length \eqn{K} of standard errors of \code{f}.}
  \item{method}{character string indicating how the \dQuote{optimal}
    number of clusters, \eqn{\hat k}{k^}, is computed from the gap
    statistics (and their standard deviations), or more generally how
    the location \eqn{\hat k}{k^} of the maximum of \eqn{f_k}{f[k]}
    should be determined.

    %% -> ../R/clusGap.R
    \describe{
      \item{\code{"globalmax"}:}{simply corresponds to the global maximum,
	i.e., is \code{which.max(f)}}
      \item{\code{"firstmax"}:}{gives the location of the first \emph{local}
	maximum.}
      \item{\code{"Tibs2001SEmax"}:}{uses the criterion, Tibshirani et
	al(2001) proposed: \dQuote{the smallest \eqn{k} such that \eqn{f(k)
	    \ge f(k+1) - s_{k+1}}}.  Note that this chooses \eqn{k = 1}
	when all standard deviations are larger than the differences
	\eqn{f(k+1) - f(k)}.}
      \item{\code{"firstSEmax"}:}{location of the first \eqn{f()} value
	which is not larger than the first \emph{local} maximum minus
	\code{SE.factor * SE.f[]}, i.e, within an \dQuote{f S.E.} range
	of that maximum (see also \code{SE.factor}).}
      \item{\code{"globalSEmax"}:}{(used in Dudoit and Fridlyand (2002),
	supposedly following Tibshirani's proposition):
	location of the first \eqn{f()} value which is not larger than
	the \emph{global} maximum minus \code{SE.factor * SE.f[]}, i.e,
	within an \dQuote{f S.E.} range of that maximum (see also
	\code{SE.factor}).}
    }
    See the examples for a comparison in a simple case.
  }
  \item{SE.factor}{[When \code{method} contains \code{"SE"}] Determining
    the optimal number of clusters, Tibshirani et al. proposed the
    \dQuote{1 S.E.}-rule.  Using an \code{SE.factor} \eqn{f}, the
    \dQuote{f S.E.}-rule is used, more generally.}
}
\details{
  The main result \code{<res>$Tab[,"gap"]} of course is from
  bootstrapping aka Monte Carlo simulation and hence random, or
  equivalently, depending on the initial random seed (see
  \code{\link{set.seed}()}).
  On the other hand, in our experience, using \code{B = 500} gives
  quite precise results such that the gap plot is basically unchanged
  after an another run.
}
\value{
  an object of S3 class \code{"clusGap"}, basically a list with
  components
  \item{Tab}{a matrix with \code{K.max} rows and 4 columns, named
    "logW", "E.logW", "gap", and "SE.sim",
    where \code{gap = E.logW - logW}, and \code{SE.sim} corresponds to
    the standard error of \code{gap}, \code{SE.sim[k]=}\eqn{s_k}{s[k]},
    where \eqn{s_k := \sqrt{1 + 1/B} sd^*(gap_j)}{s[k] := sqrt(1 + 1/B)
    sd^*(gap[])}, and \eqn{sd^*()} is the standard deviation of the
    simulated (\dQuote{bootstrapped}) gap values.
  }
  \item{n}{number of observations, i.e., \code{nrow(x)}.}
  \item{B}{input \code{B}}
  \item{FUNcluster}{input function \code{FUNcluster}}
}
\references{
  Tibshirani, R., Walther, G. and Hastie, T. (2001).
  Estimating the number of data clusters via the Gap statistic.
  \emph{Journal of the Royal Statistical Society B}, \bold{63}, 411--423.

  Tibshirani, R., Walther, G. and Hastie, T. (2000).
  Estimating the number of clusters in a dataset via the Gap statistic.
  Technical Report. Stanford.

  Per Broberg (2006). SAGx: Statistical Analysis of the GeneChip.
  R package version 1.9.7.% moved to Bioconductor sometime after 2006
  \url{http://home.swipnet.se/pibroberg/expression_hemsida1.html}
}
\author{
  This function is originally based on the functions \code{gap} of
  (Bioconductor) package \pkg{SAGx} by Per Broberg,
  \code{gapStat()} from former package \pkg{SLmisc} by Matthias Kohl
  and ideas from \code{gap()} and its methods of package \pkg{lga} by
  Justin Harrington.

  The current implementation is by Martin Maechler.
}
\seealso{
  \code{\link{silhouette}} for a much simpler less sophisticated
  goodness of clustering measure.

  \code{\link[fpc]{cluster.stats}()} in package \pkg{fpc} for
  alternative measures.

  %\code{\link[SGAx]{gap}} in Bioconductor package \pkg{SGAx}.
}
\examples{
### --- maxSE() methods -------------------------------------------
(mets <- eval(formals(maxSE)$method))
fk <- c(2,3,5,4,7,8,5,4)
sk <- c(1,1,2,1,1,3,1,1)/2
## use plot.clusGap():
plot(structure(class="clusGap", list(Tab = cbind(gap=fk, SE.sim=sk))))
## Note that 'firstmax' and 'globalmax' are always at 3 and 6 :
sapply(c(1/4, 1,2,4), function(SEf)
        sapply(mets, function(M) maxSE(fk, sk, method = M, SE.factor = SEf)))

### --- clusGap() -------------------------------------------------
## ridiculously nicely separated clusters in 3 D :
x <- rbind(matrix(rnorm(150,           sd = 0.1), ncol = 3),
           matrix(rnorm(150, mean = 1, sd = 0.1), ncol = 3),
           matrix(rnorm(150, mean = 2, sd = 0.1), ncol = 3),
           matrix(rnorm(150, mean = 3, sd = 0.1), ncol = 3))

## Slightly faster way to use pam (see below)
pam1 <- function(x,k) list(cluster = pam(x,k, cluster.only=TRUE))

doExtras <- cluster:::doExtras()
## or set it explicitly to TRUE for the following
if(doExtras) {
## Note we use  B = 60 in the following examples to keep them "speedy".
## ---- rather keep the default B = 500 for your analysis!

## note we can  pass 'nstart = 20' to kmeans() :
gskmn <- clusGap(x, FUN = kmeans, nstart = 20, K.max = 8, B = 60)
gskmn #-> its print() method
plot(gskmn, main = "clusGap(., FUN = kmeans, n.start=20, B= 60)")
set.seed(12); system.time(
  gsPam0 <- clusGap(x, FUN = pam, K.max = 8, B = 60)
)
set.seed(12); system.time(
  gsPam1 <- clusGap(x, FUN = pam1, K.max = 8, B = 60)
)
## and show that it gives the same:
stopifnot(identical(gsPam1[-4], gsPam0[-4]))
gsPam1
print(gsPam1, method="globalSEmax")
print(gsPam1, method="globalmax")
}% end {doExtras}

gs.pam.RU <- clusGap(ruspini, FUN = pam1, K.max = 8, B = 60)
gs.pam.RU
plot(gs.pam.RU, main = "Gap statistic for the 'ruspini' data")
mtext("k = 4 is best .. and  k = 5  pretty close")

\donttest{## This takes a minute..
## No clustering ==> k = 1 ("one cluster") should be optimal:
Z <- matrix(rnorm(256*3), 256,3)
gsP.Z <- clusGap(Z, FUN = pam1, K.max = 8, B = 200)
plot(gsP.Z)
gsP.Z
}%end{dont..}
}
\keyword{cluster}
