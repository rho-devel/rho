\name{summary.agnes}
\alias{summary.agnes}
\alias{print.summary.agnes}
\title{Summary Method for `agnes' Objects}
\description{
  Returns (and prints) a summary list for an \code{agnes} object.
  Printing gives more output than the corresponding
  \code{\link{print.agnes}} method.
}
\usage{
\method{summary}{agnes}(object, \dots)
\method{print}{summary.agnes}(x, \dots)
}
\arguments{
  \item{x, object}{a \code{\link{agnes}} object.}
  \item{\dots}{potential further arguments (require by generic).}
}
\seealso{\code{\link{agnes}}, \code{\link{agnes.object}}.}
\examples{
data(agriculture)
summary(agnes(agriculture))
}
\keyword{cluster}
\keyword{print}

