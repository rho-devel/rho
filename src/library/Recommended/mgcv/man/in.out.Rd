\name{in.out}
\alias{in.out}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Which of a set of points lie within a polygon defined region}

\description{Tests whether each of a set of points lie within a region defined by one or more 
(possibly nested) polygons. Points count as `inside' if they are interior to an odd number of polygons.
}
\usage{
in.out(bnd,x)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{bnd}{A two column matrix, the rows of which define the vertices of polygons defining the boundary of a region.
Different polygons should be separated by an \code{NA} row, and the polygons are assumed closed.}
\item{x}{A two column matrix. Each row is a point to test for inclusion in the region defined by \code{bnd}.}
} 


\value{A logical vector of length \code{nrow(x)}. \code{TRUE} if the corresponding row of \code{x} is inside 
the boundary and \code{FALSE} otherwise.
}


\details{ The algorithm works by counting boundary crossings (using compiled C code).
}

\references{


\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}


}


\examples{
library(mgcv)
data(columb.polys)
bnd <- columb.polys[[2]]
plot(bnd,type="n")
polygon(bnd)
x <- seq(7.9,8.7,length=20)
y <- seq(13.7,14.3,length=20)
gr <- as.matrix(expand.grid(x,y))
inside <- in.out(bnd,gr)
points(gr,col=as.numeric(inside)+1)
}








