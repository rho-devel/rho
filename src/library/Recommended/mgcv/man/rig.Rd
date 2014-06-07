\name{rig}
\alias{rig}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generate inverse Gaussian random deviates}

\description{Generates inverse Gaussian random deviates.
}
\usage{
rig(n,mean,scale)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{n}{the number of deviates required. If this has length > 1 then the length is taken as the number of deviates required.}
\item{mean}{vector of mean values.}
\item{scale}{vector of scale parameter values (lambda, see below)}
} 


\value{ 
A vector of inverse Gaussian random deviates.
}


\details{ If x if the returned vector, then E(x) = \code{mean} while var(x) = \code{scale*mean^3}. For density and distribution functions 
see the \code{statmod} package. The algorithm used is Algorithm 5.7 of Gentle (2003), based on Michael et al. (1976). Note that \code{scale} 
here is the scale parameter in the GLM sense, which is the reciprocal of the usual `lambda' parameter. 
}


\references{

Gentle, J.E. (2003) Random Number Generation and Monte Carlo Methods (2nd ed.) Springer.

Michael, J.R., W.R. Schucany & R.W. Hass (1976) Generating random variates using transformations 
with multiple roots. The American Statistician 30, 88-90.

\url{http://www.maths.bath.ac.uk/~sw283/}
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}

}


\examples{
require(mgcv)
set.seed(7)
## An inverse.gaussian GAM example, by modify `gamSim' output... 
dat <- gamSim(1,n=400,dist="normal",scale=1)
dat$f <- dat$f/4 ## true linear predictor 
Ey <- exp(dat$f);scale <- .5 ## mean and GLM scale parameter
## simulate inverse Gaussian response...
dat$y <- rig(Ey,mean=Ey,scale=.2)
big <- gam(y~ s(x0)+ s(x1)+s(x2)+s(x3),family=inverse.gaussian(link=log),
          data=dat,method="REML")
plot(big,pages=1)
gam.check(big)
summary(big)
}








