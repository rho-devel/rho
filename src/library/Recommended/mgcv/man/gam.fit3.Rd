\name{gam.fit3}
\alias{gam.fit3}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{P-IRLS GAM estimation with GCV \& UBRE/AIC or RE/ML derivative calculation}
\description{Estimation of GAM smoothing parameters is most stable if
optimization of the UBRE/AIC, GCV, GACV, REML or ML score is outer to the penalized iteratively
re-weighted least squares scheme used to estimate the model given smoothing 
parameters.

This routine estimates a GAM (any quadratically penalized GLM) given log 
smoothing paramaters, and evaluates derivatives of the smoothness selection scores 
of the model with respect to the
log smoothing parameters. Calculation of exact derivatives is generally faster
than approximating them by finite differencing, as well as generally improving
the reliability of GCV/UBRE/AIC/REML score minimization.

The approach is to run the P-IRLS
to convergence, and only then to iterate for first and second derivatives. 

Not normally called directly, but rather service routines for \code{\link{gam}}.
}
\usage{

gam.fit3(x, y, sp, Eb ,UrS=list(), 
         weights = rep(1, nobs), start = NULL, etastart = NULL, 
         mustart = NULL, offset = rep(0, nobs), U1 = diag(ncol(x)), 
         Mp = -1, family = gaussian(), control = gam.control(), 
         intercept = TRUE,deriv=2,gamma=1,scale=1,
         printWarn=TRUE,scoreType="REML",null.coef=rep(0,ncol(x)),
         pearson.extra=0,dev.extra=0,n.true=-1,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{x}{The model matrix for the GAM (or any penalized GLM).}

\item{y}{The response variable.}

\item{sp}{The log smoothing parameters.}

\item{Eb}{A balanced version of the total penalty matrix: usd for numerical rank determination.}

\item{UrS}{List of square root penalties premultiplied by transpose of orthogonal
           basis for the total penalty.}

\item{weights}{prior weights for fitting.}

\item{start}{optional starting parameter guesses.} 

\item{etastart}{optional starting values for the linear predictor.}

\item{mustart}{optional starting values for the mean.}

\item{offset}{the model offset}

\item{U1}{An orthogonal basis for the range space of the penalty --- required for ML smoothness estimation only.}

\item{Mp}{The dimension of the total penalty null space --- required for ML smoothness estimation only.}

\item{family}{the family - actually this routine would never be called with \code{gaussian()}} 

\item{control}{control list as returned from \code{\link{glm.control}}} 

\item{intercept}{does the model have and intercept, \code{TRUE} or
\code{FALSE}}

\item{deriv}{ Should derivatives of the GCV and UBRE/AIC scores be calculated?
             0, 1 or 2,
            indicating the maximum order of differentiation to apply.}

\item{gamma}{The weight given to each degree of freedom in the GCV and UBRE
scores can be varied (usually increased) using this parameter.}

\item{scale}{The scale parameter - needed for the UBRE/AIC score.}

\item{printWarn}{Set to \code{FALSE} to suppress some warnings. Useful in
order to ensure that some warnings are only printed if they apply to the final
fitted model, rather than an intermediate used in optimization.}

\item{scoreType}{specifies smoothing parameter selection criterion to use.}

\item{null.coef}{coefficients for a model which gives some sort of upper bound on deviance.
This allows immediate divergence problems to be controlled.}

\item{pearson.extra}{Extra component to add to numerator of pearson statistic 
       in P-REML/P-ML smoothness selection criteria.}


\item{dev.extra}{Extra component to add to deviance for REML/ML type smoothness selection criteria.}

\item{n.true}{Number of data to assume in smoothness selection criteria. <=0 indicates that it should be the 
number of rows of \code{X}.}

\item{...}{Other arguments: ignored.}
}
\details{ This routine is basically \code{\link{glm.fit}} with some
modifications to allow (i) for quadratic penalties on the log likelihood;
(ii) derivatives of the model coefficients with respect to
log smoothing parameters to be obtained by use of the implicit function theorem and 
(iii) derivatives of the GAM GCV, UBRE/AIC, REML or ML scores to be
evaluated at convergence. 

In addition the routines apply step halving to any step that increases the
penalized deviance substantially.

The most costly parts of the calculations are performed by calls to compiled C
code (which in turn calls LAPACK routines) in place of the compiled code that
would usually perform least squares estimation on the working model in the
IRLS iteration. 

Estimation of smoothing parameters by optimizing GCV scores obtained at
convergence of the P-IRLS iteration was proposed by O'Sullivan et al. (1986),
and is here termed `outer' iteration. 

Note that use of non-standard families with this routine requires modification
of the families as described in \code{\link{fix.family.link}}.

}

\references{

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36

O 'Sullivan, Yandall & Raynor (1986) Automatic smoothing of regression
functions in generalized linear models. J. Amer. Statist. Assoc. 81:96-103.

\url{http://www.maths.bath.ac.uk/~sw283/}

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}

The routine has been modified from \code{glm.fit} in R 2.0.1, written
by the R core (see \code{\link{glm.fit}} for further credits).
}


\seealso{\code{\link{gam.fit}},  \code{\link{gam}}, \code{\link{magic}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






