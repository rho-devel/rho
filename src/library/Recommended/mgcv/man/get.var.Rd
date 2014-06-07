\name{get.var}
\alias{get.var}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Get named variable or evaluate expression from list or data.frame}
\description{ This routine takes a text string and a data frame or list. It first sees if the 
string is the name of a variable in the data frame/ list. If it is then the value of this variable is returned. 
Otherwise the routine tries to evaluate the expression within the data.frame/list (but nowhere else) and if 
successful returns the result. If neither step works then \code{NULL} is returned. The routine is useful for
processing gam formulae. If the variable is a matrix then it is coerced to a numeric vector, by default.}

\usage{ get.var(txt,data,vecMat=TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{txt}{a text string which is either the name of a variable in \code{data} or when 
parsed is an expression that can be evaluated in \code{data}. It can also be neither in which case the
function returns \code{NULL}.}
\item{data}{A data frame or list.} 
\item{vecMat}{Should matrices be coerced to numeric vectors?}
}

\value{The evaluated variable or \code{NULL}. May be coerced to a numeric vector if it's a matrix.}

\references{
\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org} } 

\seealso{  \code{\link{gam} } }

\examples{
require(mgcv)
y <- 1:4;dat<-data.frame(x=5:10)
get.var("x",dat)
get.var("y",dat)
get.var("x==6",dat)
dat <- list(X=matrix(1:6,3,2))
get.var("X",dat)
}
\keyword{models} \keyword{smooth} \keyword{regression} %-- one or more ..






