\name{dissimilarity.object}
\alias{dissimilarity.object}
\title{Dissimilarity Matrix Object}
\description{
  Objects of class \code{"dissimilarity"} representing the dissimilarity
  matrix of a dataset.
}
\section{GENERATION}{
  \code{\link{daisy}} returns this class of objects.
  Also the functions \code{pam}, \code{clara}, \code{fanny},
  \code{agnes}, and \code{diana} return a \code{dissimilarity} object,
  as one component of their return objects.
}
\section{METHODS}{
  The \code{"dissimilarity"} class has methods for the following generic
  functions: \code{print}, \code{summary}.
}
\value{
  The dissimilarity matrix is symmetric, and hence its lower triangle
  (column wise) is represented as a vector to save storage space.
  If the object, is called \code{do}, and \code{n} the number of
  observations, i.e., \code{n <- attr(do, "Size")}, then
  for \eqn{i < j <= n}, the dissimilarity between (row) i and j is
  \code{do[n*(i-1) - i*(i-1)/2 + j-i]}.
  The length of the vector is \eqn{n*(n-1)/2}, i.e., of order \eqn{n^2}.

  \code{"dissimilarity"} objects also inherit from class
  \code{\link{dist}} and can use \code{dist} methods, in
  particular, \code{\link{as.matrix}}, such that \eqn{d_{ij}}{d(i,j)}
  from above is just \code{as.matrix(do)[i,j]}.

  The object has the following attributes:
  \item{Size}{the number of observations in the dataset.}
  \item{Metric}{the metric used for calculating the
    dissimilarities.  Possible values are "euclidean", "manhattan",
    "mixed" (if variables of different types were present in the
    dataset), and "unspecified".}
  \item{Labels}{optionally, contains the labels, if any, of the
    observations of the dataset.}
  \item{NA.message}{optionally, if a dissimilarity could not be
    computed, because of too many missing values for some observations
    of the dataset.}
  \item{Types}{when a mixed metric was used, the types for each
    variable as one-letter codes (as in the book, e.g. p.54):
    \describe{
      \item{A}{Asymmetric binary}
      \item{S}{Symmetric  binary}
      \item{N}{Nominal (factor)}
      \item{O}{Ordinal (ordered factor)}
      \item{I}{Interval scaled (numeric)}
      \item{T}{raTio to be log transformed (positive numeric)}
      }.}
}
\seealso{
  \code{\link{daisy}}, \code{\link{dist}},
  \code{\link{pam}}, \code{\link{clara}}, \code{\link{fanny}},
  \code{\link{agnes}}, \code{\link{diana}}.
}
%\examples{} --> ./daisy.Rd
\keyword{cluster}

