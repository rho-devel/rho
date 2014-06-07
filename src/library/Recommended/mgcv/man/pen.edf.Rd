\name{pen.edf}
\alias{pen.edf}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract the effective degrees of freedom associated with each penalty in a gam fit}
\description{Finds the coefficients penalized by each penalty and adds up their effective degrees of freedom.
Very useful for \code{\link{t2}} terms, but hard to interpret for terms where the penalties penalize 
overlapping sets of parameters (e.g. \code{\link{te}} terms). 
}
\usage{
pen.edf(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{x}{ an object inheriting from \code{gam}}
}
\details{Useful for models containing \code{\link{t2}} terms, since it splits the EDF for the term up into 
parts due to different components of the smooth. This is useful for figuring out which interaction terms are 
actually needed in a model.  
}

\value{ A vector of EDFs, named with labels identifying which penalty each EDF relates to.

}

\author{  Simon N. Wood \email{simon.wood@r-project.org}
}



\seealso{ \code{\link{t2}}}


\examples{ 
  require(mgcv)
  set.seed(20) 
  dat <- gamSim(1,n=400,scale=2) ## simulate data
  ## following `t2' smooth basically separates smooth 
  ## of x0,x1 into main effects + interaction.... 
  
  b <- gam(y~t2(x0,x1,bs="tp",m=1,k=7)+s(x2)+s(x3),
           data=dat,method="ML")
  pen.edf(b)
  
  ## label "rr" indicates interaction edf (range space times range space)
  ## label "nr" (null space for x0 times range space for x1) is main
  ##            effect for x1.
  ## label "rn" is main effect for x0
  ## clearly interaction is negligible
  
  ## second example with higher order marginals. 
  
  b <- gam(y~t2(x0,x1,bs="tp",m=2,k=7,full=TRUE)
             +s(x2)+s(x3),data=dat,method="ML")
  pen.edf(b)
  
  ## In this case the EDF is negligible for all terms in the t2 smooth
  ## apart from the `main effects' (r2 and 2r). To understand the labels
  ## consider the following 2 examples....
  ## "r1" relates to the interaction of the range space of the first 
  ##      marginal smooth and the first basis function of the null 
  ##      space of the second marginal smooth
  ## "2r" relates to the interaction of the second basis function of 
  ##      the null space of the first marginal smooth with the range 
  ##      space of the second marginal smooth. 
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






