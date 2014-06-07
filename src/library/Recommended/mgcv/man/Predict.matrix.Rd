\name{Predict.matrix}
\alias{Predict.matrix}
\alias{Predict.matrix2}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Prediction methods for smooth terms in a GAM}
\description{ Takes \code{smooth} objects produced by \code{smooth.construct} methods and obtains the matrix mapping 
the parameters associated with such a smooth to the predicted values of the smooth at a set of new covariate values.

In practice this method is often called via the wrapper function \code{\link{PredictMat}}.
}

\usage{
Predict.matrix(object,data)
Predict.matrix2(object,data)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{object}{ is a smooth object produced by a \code{smooth.construct} method function. The object 
contains all the information required to specify the basis for a term of its class, and this information is
used by the appropriate \code{Predict.matrix} function to produce a prediction matrix for new covariate values.
 Further details are given in \code{\link{smooth.construct}}.}

\item{data}{A data frame containing the values of the (named) covariates at which the smooth term is to be 
evaluated. Exact requirements are as for \code{\link{smooth.construct}} and \code{smooth.construct2}}.
}

\value{ A matrix which will map the parameters associated with the smooth to the vector of values of the smooth 
evaluated at the covariate values given in \code{object}. If the smooth class
is one which generates offsets the corresponding offset is returned as
attribute \code{"offset"} of the matrix.}


\details{ Smooth terms in a GAM formula are turned into smooth specification objects of 
class \code{xx.smooth.spec} during processing of the formula. Each of these objects is
converted to a smooth object using an appropriate \code{smooth.construct} function. The \code{Predict.matrix} 
functions are used to obtain the matrix that will map the parameters associated with a smooth term to
the predicted values for the term at new covariate values.

Note that new smooth classes can be added by writing a new \code{smooth.construct} method function and a 
corresponding \code{\link{Predict.matrix}} method function: see the example code provided for 
\code{\link{smooth.construct}} for details.}


\references{ 

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.

}

\author{Simon N. Wood \email{simon.wood@r-project.org}}

\seealso{ \code{\link{gam}},\code{\link{gamm}},
\code{\link{smooth.construct}}, \code{\link{PredictMat}} }

\examples{# See smooth.construct examples
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...

