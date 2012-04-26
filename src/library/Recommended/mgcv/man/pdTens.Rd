\name{pdTens}
\alias{pdTens}
\alias{pdConstruct.pdTens}
\alias{pdFactor.pdTens}
\alias{pdMatrix.pdTens}
\alias{coef.pdTens}
\alias{summary.pdTens}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Functions implementing a pdMat class for tensor product smooths}
\description{This set of functions implements an \code{nlme} library \code{pdMat} class to allow
tensor product smooths to be estimated by \code{lme} as called by \code{gamm}. Tensor product smooths
have a penalty matrix made up of a weighted sum of penalty matrices, where the weights are the smoothing 
parameters. In the mixed model formulation the penalty matrix is the inverse of the covariance matrix for 
the random effects of a term, and the smoothing parameters (times a half) are variance parameters to be estimated. 
It's not possible to transform the problem to make the required random effects covariance matrix look like one of the standard 
\code{pdMat} classes: hence the need for the \code{pdTens} class. A \code{\link{notLog2}} parameterization ensures that 
the parameters are positive. 

These functions (\code{pdTens}, \code{pdConstruct.pdTens},
 \code{pdFactor.pdTens}, \code{pdMatrix.pdTens}, \code{coef.pdTens} and \code{summary.pdTens})
 would not normally be called directly.
}

\usage{
pdTens(value = numeric(0), form = NULL, 
       nam = NULL, data = sys.frame(sys.parent()))
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{value}{Initialization values for parameters. Not normally used.}

\item{form}{A one sided formula specifying the random effects structure. The formula should have
an attribute \code{S} which is a list of the penalty matrices the weighted sum of which gives the inverse of the 
covariance matrix for these random effects.}   

\item{nam}{a names argument, not normally used with this class.}

\item{data}{data frame in which to evaluate formula.}
}
\details{ If using this class directly note that it is worthwhile scaling the
\code{S} matrices to be of `moderate size', for example by dividing each
matrix by its largest singular value: this avoids problems with \code{lme}
defaults (\code{\link{smooth.construct.tensor.smooth.spec}} does this automatically).  

This appears to be the minimum set of functions required to implement a new \code{pdMat} class. 

Note that while the \code{pdFactor} and \code{pdMatrix} functions return the inverse of the scaled random 
effect covariance matrix or its factor, the \code{pdConstruct} function is
sometimes initialised with estimates of the scaled covariance matrix, and
sometimes intialized with its inverse. 

}
\value{ A class \code{pdTens} object, or its coefficients or the matrix it
represents or the factor of 
that matrix. \code{pdFactor} returns the factor as a vector (packed
column-wise) (\code{pdMatrix} always returns a matrix).
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{
Pinheiro J.C. and Bates, D.M. (2000) Mixed effects Models in S and S-PLUS. Springer

The \code{nlme} source code.

\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{te}}  \code{\link{gamm}}}

\examples{
# see gamm
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









