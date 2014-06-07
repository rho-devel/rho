\name{gam.fit}
\alias{gam.fit}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{GAM P-IRLS estimation with GCV/UBRE smoothness estimation}
\description{ This is an internal function of package \code{mgcv}. It is a modification
  of the function \code{glm.fit}, designed to be called from \code{gam}. The major
  modification is that rather than solving a weighted least squares problem at each IRLS step, 
  a weighted, penalized least squares problem
  is solved at each IRLS step with smoothing parameters associated with each penalty chosen by GCV or UBRE,
  using routine \code{\link{magic}}. 
For further information on usage see code for \code{gam}. Some regularization of the 
IRLS weights is also permitted as a way of addressing identifiability related problems (see 
\code{\link{gam.control}}). Negative binomial parameter estimation is
supported.

The basic idea of estimating smoothing parameters at each step of the P-IRLS
is due to Gu (1992), and is termed `performance iteration' or `performance
oriented iteration'.
}
\usage{ 

gam.fit(G, start = NULL, etastart = NULL, 
        mustart = NULL, family = gaussian(), 
        control = gam.control(),gamma=1,
        fixedSteps=(control$maxit+1),...) 

}
\arguments{ 
\item{G}{An object of the type returned by \code{\link{gam}} when \code{fit=FALSE}.}

\item{start}{Initial values for the model coefficients.}

\item{etastart}{Initial values for the linear predictor.}

\item{mustart}{Initial values for the expected response.}

\item{family}{The family object, specifying the distribution and link to use.}

\item{control}{Control option list as returned by \code{\link{gam.control}}.}

\item{gamma}{Parameter which can be increased to up the cost of each effective degree of freedom in the
GCV or AIC/UBRE objective.}

\item{fixedSteps}{How many steps to take: useful when only using this routine to get rough starting values for other methods.}

\item{...}{Other arguments: ignored.}
}

\value{A list of fit information.}

\references{

Gu (1992) Cross-validating non-Gaussian data. J. Comput. Graph. Statist. 1:169-179

Gu and Wahba (1991) Minimizing GCV/GML scores with multiple smoothing parameters via
the Newton method. SIAM J. Sci. Statist. Comput. 12:383-398

Wood, S.N. (2000)  Modelling and Smoothing Parameter Estimation
with Multiple  Quadratic Penalties. J.R.Statist.Soc.B 62(2):413-428

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. J. Amer. Statist. Ass. 99:637-686

}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{ \code{\link{gam.fit3}},  \code{\link{gam}}, \code{\link{magic}}}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..
