\name{ls.size}
\alias{ls.size}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Size of list elements}

\description{Produces a named array giving the size, in bytes, of the elements of a list.
}
\usage{
ls.size(x)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{x}{ A list.}
} 


\value{ A numeric vector giving the size in bytes of each element of the list \code{x}. The elements of the array have the 
same names as the elements of the list. If \code{x} is not a list then its size in bytes is returned, un-named.
}

\references{


\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}


}


\examples{
library(mgcv)
b <- list(M=matrix(runif(100),10,10),quote=
"The world is ruled by idiots because only an idiot would want to rule the world.",
fam=binomial())
ls.size(b)
}








