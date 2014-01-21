\name{notExp2}
\alias{notExp2}
\alias{notLog2}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Alternative to log parameterization for variance components}

\description{ \code{notLog2} and \code{notExp2} are alternatives to \code{log}
and \code{exp} or \code{\link{notLog}} and \code{\link{notExp}} for
re-parameterization of variance parameters. They are used by the
\code{\link{pdTens}} and \code{\link{pdIdnot}} classes which in turn implement
smooths for \code{\link{gamm}}.

The functions are typically used to ensure that smoothing parameters are
positive, but the \code{notExp2} is not monotonic: rather it cycles between
`effective zero' and `effective infinity' as its argument changes. The
\code{notLog2} is the inverse function of the \code{notExp2} only over an
interval centered on zero.

Parameterizations using these functions ensure that estimated smoothing
parameters remain positive, but also help to ensure that the likelihood is
never indefinite: once a working parameter pushes a smoothing parameter below
`effetive zero' or above `effective infinity' the cyclic nature of the
\code{notExp2} causes the likelihood to decrease, where otherwise it might
simply have flattened. 

This parameterization is really just a numerical trick, in order to get
\code{lme} to fit \code{gamm} models, without failing due to indefiniteness. 
Note in particular that asymptotic results on the likelihood/REML criterion are 
not invalidated by the trick,
unless parameter estimates end up close to the effective zero or effective
infinity: but if this is the case then the asymptotics would also have been invalid
for a conventional monotonic parameterization.
 
This reparameterization was made necessary by some modifications to the
underlying optimization method in  \code{lme} introduced in nlme 3.1-62. It is
possible that future releases will return to the \code{\link{notExp}} parameterization.

Note that you can reset `effective zero' and `effective infinity': see below.
}

\usage{
notExp2(x,d=.Options$mgcv.vc.logrange,b=1/d)

notLog2(x,d=.Options$mgcv.vc.logrange,b=1/d)
}

%- maybe also `usage' for other objects documented here.
\arguments{
\item{x}{Argument array of real numbers (\code{notExp}) or positive real numbers (\code{notLog}).}
\item{d}{the range of \code{notExp2} runs from \code{exp(-d)} to
\code{exp(d)}. To change the range used by \code{gamm} reset
\code{mgcv.vc.logrange} using \code{\link{options}}.}
\item{b}{determines the period of the cycle of \code{notExp2}.}
}

\value{ An array of function values evaluated at the supplied argument values.}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{

\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{pdTens}}, \code{\link{pdIdnot}},  \code{\link{gamm}}}

\examples{
## Illustrate the notExp2 function:
require(mgcv)
x <- seq(-50,50,length=1000)
op <- par(mfrow=c(2,2))
plot(x,notExp2(x),type="l")
lines(x,exp(x),col=2)
plot(x,log(notExp2(x)),type="l")
lines(x,log(exp(x)),col=2) # redundancy intended
x <- x/4
plot(x,notExp2(x),type="l")
lines(x,exp(x),col=2)
plot(x,log(notExp2(x)),type="l")
lines(x,log(exp(x)),col=2) # redundancy intended
par(op)
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









