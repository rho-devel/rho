\name{interpret.gam}
\alias{interpret.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Interpret a GAM formula}
\description{ This is an internal function of package \code{mgcv}. It is a service routine for
 \code{gam} which splits off the strictly parametric part of the model formula, returning 
it as a formula, and interprets the smooth parts of the model formula. 

Not normally called directly.
}

\usage{interpret.gam(gf)}

\arguments{\item{gf}{A GAM formula as supplied to \code{\link{gam}} or \code{\link{gamm}}.}

}

\value{An object of class \code{split.gam.formula} with the following items:

\item{pf}{A model formula for the strictly parametric part of the model.}

\item{pfok}{TRUE if there is a \code{pf} formula.}

\item{smooth.spec}{A list of class \code{xx.smooth.spec} objects where \code{xx} depends on the basis 
specified for the term. (These can be passed to smooth constructor method functions to actually set up
penalties and bases.)}

\item{full.formula}{An expanded version of the model formula in which the options are fully expanded, and 
the options do not depend on variables which might not be available later.}

\item{fake.formula}{A formula suitable for use in evaluating a model frame.}

\item{response}{Name of the response variable.}
}

\references{

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{   \code{\link{gam}} \code{\link{gamm}}}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..



