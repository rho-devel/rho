\name{notExp}
\alias{notExp}
\alias{notLog}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Functions for better-than-log positive parameterization}
\description{ It is common practice in statistical optimization to use log-parameterizations when a 
parameter ought to be positive. i.e. if an optimization parameter \code{a} should be non-negative then 
we use \code{a=exp(b)} and optimize with respect to the unconstrained parameter \code{b}. This often works 
well, but it does imply a rather limited working range for \code{b}: using 8 byte doubles, for example, 
 if \code{b}'s magnitude gets much above 700 then \code{a} overflows or underflows. This can cause 
problems for numerical optimization methods. 

\code{notExp} is a monotonic function for mapping the real line into the positive real line with much less
extreme underflow and overflow behaviour than \code{exp}. It is a piece-wise function, but is continuous 
to second derivative: see the source code for the exact definition, and the example below to see what it 
looks like.

\code{notLog} is the inverse function of \code{notExp}.

The major use of these functions was originally to provide more robust
\code{pdMat} classes for \code{lme} for use by \code{\link{gamm}}. Currently
the \code{\link{notExp2}} and \code{\link{notLog2}} functions are used in
their place, as a result of changes to the nlme optimization routines.
}

\usage{
notExp(x)

notLog(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{x}{Argument array of real numbers (\code{notExp}) or positive real numbers (\code{notLog}).}
}
\value{ An array of function values evaluated at the supplied argument values.}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{

\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{pdTens}}, \code{\link{pdIdnot}},  \code{\link{gamm}}}

\examples{
## Illustrate the notExp function: 
## less steep than exp, but still monotonic.
require(mgcv)
x <- -100:100/10
op <- par(mfrow=c(2,2))
plot(x,notExp(x),type="l")
lines(x,exp(x),col=2)
plot(x,log(notExp(x)),type="l")
lines(x,log(exp(x)),col=2) # redundancy intended
x <- x/4
plot(x,notExp(x),type="l")
lines(x,exp(x),col=2)
plot(x,log(notExp(x)),type="l")
lines(x,log(exp(x)),col=2) # redundancy intended
par(op)
range(notLog(notExp(x))-x) # show that inverse works!
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









