\name{influence.gam}
\alias{influence.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract the diagonal of the influence/hat matrix for a GAM }
\description{ Extracts the leading diagonal of the influence matrix (hat
matrix) of a fitted \code{gam} object.
}
\usage{
\method{influence}{gam}(model,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{model}{ fitted model objects of class \code{gam} as produced by \code{gam()}.}
\item{...}{un-used in this case} 

}
\details{ Simply extracts \code{hat} array from fitted model. (More may follow!)

}

\value{ An array (see above).
}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  \code{\link{gam}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






