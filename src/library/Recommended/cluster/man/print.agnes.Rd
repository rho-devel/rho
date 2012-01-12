\name{print.agnes}
\alias{print.agnes}
\title{Print Method for AGNES Objects}
\description{
  Prints the call, agglomerative coefficient, ordering of objects and
  distances between merging clusters (`Height') of an \code{agnes} object.

  This is a method for the generic \code{\link{print}()} function for objects
  inheriting from class \code{agnes}, see \code{\link{agnes.object}}.
}
\usage{
\method{print}{agnes}(x, \dots)
}
\arguments{
  \item{x}{an agnes object.}
  \item{\dots}{potential further arguments (required by generic).}
}
\seealso{
  \code{\link{summary.agnes}} producing more output;
  \code{\link{agnes}}, \code{\link{agnes.object}}, \code{\link{print}},
  \code{\link{print.default}}.
}
\keyword{cluster}
\keyword{print}
