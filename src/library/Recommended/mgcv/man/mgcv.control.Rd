\name{mgcv.control}
\alias{mgcv.control}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Setting mgcv defaults}
\description{ This is an internal function of package \code{mgcv} which allows control of the numerical 
options for fitting a generalized ridge regression problem using routine \code{mgcv}. 
}
\usage{
mgcv.control(conv.tol=1e-7,max.half=20,target.edf=NULL,min.edf=-1)
}
\arguments{ 
\item{conv.tol}{ The convergence tolerance.}

\item{max.half}{successive step halvings are employed if the Newton method and then the steepest descent backup
fail to improve the UBRE/GCV score. This is how many to use before giving up.}

\item{target.edf}{If this is non-null it indicates that cautious optimization should be used, which 
opts for the local minimum closest to the target model edf if there are multiple local minima in the 
GCV/UBRE score.}

\item{min.edf}{Lower bound on the model edf. Useful for avoiding numerical problems at high smoothing parameter
values. Negative for none.}
}


\references{

Gu and Wahba (1991) Minimizing GCV/GML scores with multiple smoothing parameters via
the Newton method. SIAM J. Sci. Statist. Comput. 12:383-398

Wood, S.N. (2000)  Modelling and Smoothing Parameter Estimation
with Multiple  Quadratic Penalties. J.R.Statist.Soc.B 62(2):413-428

\url{http://www.maths.bath.ac.uk/~sw283/}


}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{   \code{\link{mgcv}} }

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




