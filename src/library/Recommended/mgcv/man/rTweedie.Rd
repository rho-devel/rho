\name{rTweedie}
\alias{rTweedie}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generate Tweedie random deviates}
\description{ Generates Tweedie random deviates, for powers between 1 and 2.
}

\usage{
rTweedie(mu,p=1.5,phi=1)
}
\arguments{
\item{mu}{vector of expected values for the deviates to be generated. One deviate generated for each element of \code{mu}.}

\item{p}{the variance of a deviate is proportional to its mean, \code{mu} to the power \code{p}. \code{p} must
be between 1 and 2. 1 is Poisson like (exactly Poisson if \code{phi=1}), 2 is gamma. }

\item{phi}{The scale parameter. Variance of the deviates is given by is \code{phi*mu^p}.}

}

\value{ A vector of random deviates from a Tweedie distribution, expected value vector \code{mu}, variance vector \code{phi*mu^p}.
}


\details{ A Tweedie random variable with 1<p<2 is a sum of \code{N} gamma random variables 
where \code{N} has a Poisson distribution, with mean \code{mu^(2-p)/((2-p)*phi)}. The Gamma random variables 
that are summed have shape parameter \code{(2-p)/(p-1)} and scale parameter \code{phi*(p-1)*mu^(p-1)} (note that 
this scale parameter is different from the scale parameter for a GLM with Gamma errors). 

This is a restricted, but faster, version of \code{rtweedie} from the \code{tweedie} package.
}

%- maybe also `usage' for other objects documented here.

\author{ Simon N. Wood \email{simon.wood@r-project.org}
}

\references{
Peter K Dunn (2009). tweedie: Tweedie exponential family models. R
  package version 2.0.2. http://CRAN.R-project.org/package=tweedie
}


\seealso{\code{\link{ldTweedie}}, \code{\link{Tweedie}}}

\examples{
 library(mgcv)
 f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
            (10 * x)^3 * (1 - x)^10
 n <- 300
 x <- runif(n)
 mu <- exp(f2(x)/3+.1);x <- x*10 - 4
 y <- rTweedie(mu,p=1.5,phi=1.3)
 b <- gam(y~s(x,k=20),family=Tweedie(p=1.5))
 b
 plot(b) 

}
\keyword{models} \keyword{regression}%-- one or more ..


