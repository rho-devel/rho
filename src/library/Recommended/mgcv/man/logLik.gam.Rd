\name{logLik.gam}
\alias{logLik.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract the log likelihood for a fitted GAM}
\description{ Function to extract the log-likelihood for a fitted \code{gam}
model (note that the models are usually fitted by penalized likelihood maximization). 
}
\usage{
\method{logLik}{gam}(object,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{object}{ fitted model objects of class \code{gam} as produced by \code{gam()}.}
\item{...}{un-used in this case} 

}
\details{ Modification of \code{logLik.glm} which corrects the degrees of
freedom for use with \code{gam} objects. 

The function is provided so that \code{\link{AIC}} functions correctly with
\code{gam} objects, and uses the appropriate degrees of freedom (accounting
for penalization). Note,  when using \code{AIC} for penalized models, that the
degrees of freedom are the effective degrees of freedom and not the number of
parameters, and the model maximizes the penalized likelihood, not the actual
likelihood! This seems to be reasonably well founded for the known scale
parameter case (see Hastie and Tibshirani, 1990, section 6.8.3 and also Wood 2008), 
and in fact in this case the default smoothing parameter estimation criterion is
effectively this modified AIC.  
}

\value{ Standard \code{logLik} object: see \code{\link{logLik}}.
}


\references{Hastie and Tibshirani, 1990, Generalized Additive Models.

Wood, S.N. (2008) Fast stable direct fitting and smoothness selection for
generalized additive models. J.R.Statist. Soc. B 70(3):495-518



}

\author{ Simon N. Wood \email{simon.wood@r-project.org} based directly on \code{logLik.glm}}


\seealso{  \code{\link{AIC}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






