\name{gam2objective}
\alias{gam2objective}
\alias{gam2derivative}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Objective functions for GAM smoothing parameter estimation}
\description{Estimation of GAM smoothing parameters is most stable if
optimization of the UBRE/AIC or GCV score is outer to the penalized iteratively
re-weighted least squares scheme used to estimate the model given smoothing 
parameters. These functions evaluate the GCV/UBRE/AIC score of a GAM model, given
smoothing parameters, in a manner suitable for use by \code{\link{optim}} or \code{\link{nlm}}.
Not normally called directly, but rather service routines for \code{\link{gam.outer}}.
}
\usage{
gam2objective(lsp,args,...)
gam2derivative(lsp,args,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{lsp}{The log smoothing parameters.}

\item{args}{List of arguments required to call \code{\link{gam.fit3}}.}


\item{...}{Other arguments for passing to \code{gam.fit3}.}
}
\details{ \code{gam2objective} and \code{gam2derivative} are functions suitable
for calling by \code{\link{optim}}, to evaluate the GCV/UBRE/AIC score and its
derivatives w.r.t. log smoothing parameters.

\code{gam4objective} is an equivalent to \code{gam2objective}, suitable for
optimization by \code{\link{nlm}} - derivatives of the GCV/UBRE/AIC function are
calculated and returned as attributes.

The basic idea of optimizing smoothing parameters `outer' to the P-IRLS loop
was first proposed in O'Sullivan et al. (1986).
}

\references{

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36

O 'Sullivan, Yandall & Raynor (1986) Automatic smoothing of regression
functions in generalized linear models. J. Amer. Statist. Assoc. 81:96-103.

Wood, S.N. (2008) Fast stable direct fitting and smoothness selection for generalized
additive models. J.R.Statist.Soc.B 70(3):495-518

\url{http://www.maths.bath.ac.uk/~sw283/}

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{\code{\link{gam.fit3}},  \code{\link{gam}}, \code{\link{magic}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






