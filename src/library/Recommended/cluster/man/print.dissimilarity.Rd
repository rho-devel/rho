\title{Print and Summary Methods for Dissimilarity Objects}
\name{print.dissimilarity}
\alias{print.dissimilarity}
\alias{summary.dissimilarity}
\alias{print.summary.dissimilarity}
\description{
  Print or summarize the distances and the attributes of a
  \code{dissimilarity} object.

  These are methods for the functions \code{print()} and \code{summary()} for
  \code{dissimilarity} objects.  See \code{print}, \code{print.default},
  or \code{summary} for the general behavior of these.
}
\usage{
\method{print}{dissimilarity}(x, diag = NULL, upper = NULL,
      digits = getOption("digits"), justify = "none", right = TRUE, \dots)
\method{summary}{dissimilarity}(object,
        digits = max(3, getOption("digits") - 2), \dots)
\method{print}{summary.dissimilarity}(x, \dots)
}
\arguments{
  \item{x, object}{a \code{dissimilarity} object or a
    \code{summary.dissimilarity} one for \code{print.summary.dissimilarity()}.}
  \item{digits}{the number of digits to use, see \code{\link{print.default}}.}
  \item{diag, upper, justify, right}{optional arguments specifying how
    the triangular dissimilarity matrix is printed; see
    \code{\link[stats]{print.dist}}.}
  \item{\dots}{potential further arguments (require by generic).}
}
\seealso{
  \code{\link{daisy}}, \code{\link{dissimilarity.object}},
  \code{\link{print}}, \code{\link{print.default}}, \code{\link{print.dist}}.
}
\examples{
 ## See  example(daisy)

 sd <- summary(daisy(matrix(rnorm(100), 20,5)))
 sd # -> print.summary.dissimilarity(.)
 str(sd)
}
\keyword{cluster}
\keyword{print}


