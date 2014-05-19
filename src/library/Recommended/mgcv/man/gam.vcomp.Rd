\name{gam.vcomp}
\alias{gam.vcomp}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Report gam smoothness estimates as variance components}
\description{GAMs can be viewed as mixed models, where the smoothing parameters are related to variance 
components. This routine extracts the estimated variance components associated with each smooth term, and if possible returns confidence intervals on the standard deviation scale.
}
\usage{
gam.vcomp(x,rescale=TRUE,conf.lev=.95)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{x}{ a fitted model object of class \code{gam} as produced by \code{gam()}.}

\item{rescale}{ the penalty matrices for smooths are rescaled before fitting, for numerical 
stability reasons, if \code{TRUE} this rescaling is reversed, so that the variance components
are on the original scale.}

\item{conf.lev}{ when the smoothing parameters are estimated by REML or ML, then confidence intervals 
for the variance components can be obtained from large sample likelihood results. This gives the 
confidence level to work at.}
}
\details{The (pseudo) inverse of the penalty matrix penalizing a term is proportional to the 
covariance matrix of the term's coefficients, when these are viewed as random. For single penalty smooths, 
it is possible to compute the variance component for the smooth (which multiplies the 
inverse penalty matrix to obtain the covariance matrix of the smooth's coefficients). This
variance component is given by the scale parameter divided by the smoothing parameter. 

This routine computes such variance components, for \code{gam} models, and associated confidence intervals, if smoothing parameter estimation was likelihood based. Note that variance components are also returned 
for tensor product smooths, but that their interpretation is not so straightforward. 

The routine is particularly useful for model fitted by \code{\link{gam}} in which random effects have 
been incorporated.
}

\value{ Either a vector of variance components for each smooth term (as standard deviations), or a matrix. The first column of the 
matrix gives standard deviations for each term, while the subsequent columns give lower and upper confidence bounds, on the same scale.

For models in which there are more smoothing parameters than actually estimated (e.g. if some were fixed, or smoothing parameters are linked) then a list is returned. The \code{vc} element is as above, the \code{all} element is a vector of variance components for all the smoothing parameters (estimated + fixed or replicated).

The routine prints a table of estimated standard deviations and confidence limits, if these can be 
computed, and reports the numerical rank of the covariance matrix.
}

\author{Simon N. Wood \email{simon.wood@r-project.org}
}

\references{
Wood, S.N. (2008) Fast stable direct fitting and smoothness
 selection for generalized additive models. Journal of the Royal
 Statistical Society (B) 70(3):495-518

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36


}


\seealso{ \code{\link{smooth.construct.re.smooth.spec}}}


\examples{ 
  set.seed(3) 
  require(mgcv)
  ## simulate some data, consisting of a smooth truth + random effects

  dat <- gamSim(1,n=400,dist="normal",scale=2)
  a <- factor(sample(1:10,400,replace=TRUE))
  b <- factor(sample(1:7,400,replace=TRUE))
  Xa <- model.matrix(~a-1)    ## random main effects
  Xb <-  model.matrix(~b-1)
  Xab <- model.matrix(~a:b-1) ## random interaction
  dat$y <- dat$y + Xa\%*\%rnorm(10)*.5 + 
           Xb\%*\%rnorm(7)*.3 + Xab\%*\%rnorm(70)*.7
  dat$a <- a;dat$b <- b

  ## Fit the model using "re" terms, and smoother linkage  
  
  mod <- gam(y~s(a,bs="re")+s(b,bs="re")+s(a,b,bs="re")+s(x0,id=1)+s(x1,id=1)+
               s(x2,k=15)+s(x3),data=dat,method="ML")

  gam.vcomp(mod) 

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






