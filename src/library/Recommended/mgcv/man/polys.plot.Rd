\name{polys.plot}
\alias{polys.plot}
\title{Plot geographic regions defined as polygons}
\usage{
polys.plot(pc,z=NULL,scheme="heat",lab="",...)
}

\arguments{
  \item{pc}{A named list of matrices. Each matrix has two columns. The matrix rows each define the 
   vertex of a boundary polygon. If a boundary is defined by several polygons, then each of these must be 
   separated by an \code{NA} row in the matrix. See \code{\link{mrf}} for an example.}

  \item{z}{A vector of values associated with each area (item) of \code{pc}. If the vector elements 
  have names then these are used to match elements of \code{z} to areas defined in \code{pc}. Otherwise 
  \code{pc} and \code{z} are assumed to be in the same order. If \code{z} is \code{NULL} then polygons are not filled. }

  \item{scheme}{One of \code{"heat"} or \code{"grey"}, indicating how to fill the polygons in accordance with the value
   of \code{z}.}

  \item{lab}{label for plot.}
   
  \item{...}{other arguments to pass to plot (currently only if \code{z} is \code{NULL}).}


}

\value{Simply produces a plot.}

\description{ Produces plots of geographic regions defined by polygons, optionally filling the 
polygons with a color or grey shade dependent on a covariate.
}

\details{Any polygon within another polygon counts as a hole in the area. Further nesting is dealt with by 
treating any point that is interior to an odd number of polygons as being within the area, and all other points 
as being exterior. The routine is provided to facilitate plotting with models containing \code{\link{mrf}} smooths.
}

\author{Simon Wood \email{simon.wood@r-project.org}
}



\seealso{
\code{\link{mrf}} and \code{\link{columb.polys}}.
}
\examples{
## see also ?mrf for use of z
require(mgcv)
data(columb.polys)
polys.plot(columb.polys)
}
\keyword{hplot} \keyword{models} \keyword{smooth} \keyword{regression}
