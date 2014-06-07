\name{pdIdnot}
\alias{pdIdnot}
\alias{pdConstruct.pdIdnot}
\alias{pdFactor.pdIdnot}
\alias{pdMatrix.pdIdnot}
\alias{coef.pdIdnot}
\alias{corMatrix.pdIdnot}
\alias{Dim.pdIdnot}
\alias{logDet.pdIdnot}
\alias{solve.pdIdnot}
\alias{summary.pdIdnot}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Overflow proof pdMat class for multiples of the identity matrix}
\description{ This set of functions is a modification of the  \code{pdMat} class \code{pdIdent}
from library \code{nlme}. The modification is to replace the log parameterization used in \code{pdMat}
with a \code{\link{notLog2}} parameterization, since the latter avoids
indefiniteness in the likelihood and associated convergence problems: the
parameters also relate to variances rather than standard deviations, for
consistency with the \code{\link{pdTens}} class. The functions are particularly useful for
working with Generalized Additive Mixed Models where variance parameters/smoothing parameters can
be very large or very small, so that overflow or underflow can be a problem.

These functions would not normally be called directly, although unlike the 
\code{\link{pdTens}} class it is easy to do so.
}

\usage{
pdIdnot(value = numeric(0), form = NULL, 
       nam = NULL, data = sys.frame(sys.parent()))
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{value}{Initialization values for parameters. Not normally used.}

\item{form}{A one sided formula specifying the random effects structure. }   

\item{nam}{a names argument, not normally used with this class.}

\item{data}{data frame in which to evaluate formula.}
}
\details{ 
The following functions are provided: \code{Dim.pdIndot}, \code{coef.pdIdnot}, \code{corMatrix.pdIdnot}, 
\code{logDet.pdIdnot}, \code{pdConstruct.pdIdnot}, \code{pdFactor.pdIdnot}, \code{pdMatrix.pdIdnot},
\code{solve.pdIdnot}, \code{summary.pdIdnot}. (e.g. \code{mgcv:::coef.pdIdnot} to access.)

Note that while the \code{pdFactor} and \code{pdMatrix} functions return the inverse of the scaled random 
effect covariance matrix or its factor, the \code{pdConstruct} function is initialised with estimates of the 
scaled covariance matrix itself.

}
\value{ A class \code{pdIdnot} object, or related quantities. See the \code{nlme} documentation for further details.}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{
Pinheiro J.C. and Bates, D.M. (2000) Mixed effects Models in S and S-PLUS. Springer

The \code{nlme} source code.

\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{te}}, \code{\link{pdTens}}, \code{\link{notLog2}}, \code{\link{gamm}}}

\examples{
# see gamm
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









