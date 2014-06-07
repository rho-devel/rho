\name{volume.ellipsoid}
\alias{volume}
\alias{volume.ellipsoid}
\title{Compute the Volume of Planar Object}
\description{
  Compute the volume of a planar object.
  This is a generic function and a method for \code{ellipsoid} objects.
}
\usage{
\method{volume}{ellipsoid}(object)
}
\arguments{
  \item{object}{an \R object the volume of which is wanted; for the
    \code{ellipsoid} method, an object of that class (see
    \code{\link{ellipsoidhull}} or the example below).}
}
\value{
  a number, the volume of the given \code{object}.
}
\seealso{\code{\link{ellipsoidhull}} for spanning ellipsoid computation.}
\examples{
## example(ellipsoidhull) # which defines `ellipsoid' object <namefoo>

myEl <- structure(list(cov = rbind(c(3,1),1:2), loc = c(0,0), d2 = 10),
                   class = "ellipsoid")
volume(myEl)# i.e. "area" here (d = 2)
myEl # also mentions the "volume"
}
\keyword{utilities}
