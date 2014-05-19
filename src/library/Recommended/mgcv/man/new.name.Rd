\name{new.name}
\alias{new.name}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Obtain a name for a new variable that is not already in use}
\description{ \code{\link{gamm}} works by transforming a GAMM into something 
that can be estimated by \code{\link[nlme]{lme}}, but this involves creating new 
variables, the names of which should not clash with the names of other 
variables on which the model depends. This simple service routine checks a 
suggested name against a list of those in use, and if neccesary modifies it 
so that there is no clash.}

\usage{ new.name(proposed,old.names)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{proposed}{a suggested name}

 \item{old.names}{ An array of names that must not be duplicated} 
}

\value{A name that is not in \code{old.names}.}

\references{
\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org} } 

\seealso{  \code{\link{gamm} } }

\examples{
require(mgcv)
old <- c("a","tuba","is","tubby")
new.name("tubby",old)
}
\keyword{models} \keyword{smooth} \keyword{regression} %-- one or more ..






