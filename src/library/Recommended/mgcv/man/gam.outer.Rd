\name{gam.outer}
\alias{gam.outer}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Minimize GCV or UBRE score of a GAM using `outer' iteration}
\description{Estimation of GAM smoothing parameters is most stable if
optimization of the smoothness selection score (GCV, GACV, UBRE/AIC, REML, ML etc) 
is outer to the penalized iteratively
re-weighted least squares scheme used to estimate the model given smoothing 
parameters.

This routine optimizes a smoothness selection score in this way. Basically the 
score is evaluated for each trial set of smoothing parameters by
estimating the GAM for those smoothing parameters. The score is minimized
w.r.t. the parameters numerically, using \code{newton} (default), \code{bfgs}, \code{optim} or \code{nlm}. Exact
(first and second) derivatives of the score can be used by fitting with
\code{\link{gam.fit3}}. This
improves efficiency and reliability relative to relying on finite
difference derivatives.  

Not normally called directly, but rather a service routine for \code{\link{gam}}.
}
\usage{
gam.outer(lsp,fscale,family,control,method,optimizer,
          criterion,scale,gamma,G,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{lsp}{The log smoothing parameters.}

\item{fscale}{Typical scale of the GCV or UBRE/AIC score.}

\item{family}{the model family.}

\item{control}{control argument to pass to \code{\link{gam.fit}} if pure
finite differencing is being used.}

\item{method}{method argument to \code{\link{gam}} defining the smoothness criterion to use (but depending on whether or not scale known).}

\item{optimizer}{The argument to \code{\link{gam}} defining the numerical optimization method to use. }

\item{criterion}{Which smoothness selction criterion to use. One of \code{"UBRE"},
\code{"GCV"}, \code{"GACV"}, \code{"REML"} or \code{"P-REML"}.   }

\item{scale}{Supplied scale parameter. Positive indicates known.}

\item{gamma}{ The degree of freedom inflation factor for the GCV/UBRE/AIC score.}

\item{G}{List produced by \code{mgcv:::gam.setup}, containing most of what's
needed to actually fit a GAM.}

\item{...}{other arguments, typically for passing on to \code{gam.fit3} (ultimately).}
}
\details{ 
See Wood (2008) for full details on `outer iteration'.
}

\references{
Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36

\url{http://www.maths.bath.ac.uk/~sw283/}

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{\code{\link{gam.fit3}},  \code{\link{gam}}, \code{\link{magic}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...

