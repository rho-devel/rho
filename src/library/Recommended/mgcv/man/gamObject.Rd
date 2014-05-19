\name{gamObject}
\alias{gamObject}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fitted gam object}
\description{A fitted GAM object returned by function \code{gam} and of class
\code{"gam"} inheriting from classes \code{"glm"} and \code{"lm"}. Method
functions \code{anova}, \code{logLik}, \code{influence}, \code{plot},
\code{predict}, \code{print}, \code{residuals} and \code{summary} exist for
this class.

All compulsory elements of \code{"glm"} and \code{"lm"} objects are present,
but the fitting method for a GAM is different to a linear model or GLM, so
that the elements relating to the QR decomposition of the model matrix are
absent.
}

\value{ A \code{gam} object has the following elements:

\item{aic}{AIC of the fitted model: bear in mind that the degrees of freedom
used to calculate this are the effective degrees of freedom of the model, and
the likelihood is evaluated at the maximum of the penalized likelihood in most
cases, not at the MLE.}

\item{assign}{Array whose elements indicate which model term (listed in
\code{pterms}) each parameter relates to: applies only to non-smooth terms.}

\item{boundary}{did parameters end up at boundary of parameter space?} 

\item{call}{the matched call (allows \code{update} to be used with \code{gam} objects, for example). }

\item{cmX}{column means of the model matrix (with elements corresponding to smooths set to zero )
  --- useful for componentwise CI calculation.}

\item{coefficients}{the coefficients of the fitted model. Parametric
          coefficients are  first, followed  by coefficients for each
          spline term in turn.}

\item{control}{the \code{gam} control list used in the fit.}

\item{converged}{indicates whether or not the iterative fitting method converged.} 

\item{data}{the original supplied data argument (for class \code{"glm"} compatibility). 
            Only included if \code{\link{gam}} \code{control} argument element 
            \code{keepData} is set to \code{TRUE} (default is \code{FALSE}).}

\item{deviance}{model deviance (not penalized deviance).}

\item{df.null}{null degrees of freedom.} 

\item{df.residual}{effective residual degrees of freedom of the model.}

\item{edf}{estimated degrees of freedom for each model parameter. Penalization
means that many of these are less than 1.}

\item{edf1}{similar, but using alternative estimate of EDF. Useful for testing.}

\item{family}{family object specifying distribution and link used.}


\item{fitted.values}{fitted model predictions of expected value for each
          datum.}

\item{formula}{the model formula.}

\item{full.sp}{full array of smoothing parameters multiplying penalties (excluding any contribution 
from \code{min.sp} argument to \code{gam}). May be larger than \code{sp} if some terms share 
smoothing parameters, and/or some smoothing parameter values were supplied in the \code{sp} argument
of \code{\link{gam}}.}

\item{F}{Degrees of freedom matrix. This may be removed at some point, and should probably not be used.}

\item{gcv.ubre}{The minimized smoothing parameter selection score: GCV, UBRE(AIC), GACV, negative log marginal 
likelihood or negative log restricted likelihood.}


\item{hat}{array of elements from the leading diagonal of the `hat' (or `influence') matrix. 
Same length as response data vector.}

\item{iter}{number of iterations of P-IRLS taken to get convergence.}

\item{linear.predictors}{fitted model prediction of link function of
expected value for  each datum.}

\item{method}{One of \code{"GCV"} or \code{"UBRE"}, \code{"REML"}, \code{"P-REML"}, \code{"ML"},
\code{"P-ML"}, \code{"PQL"}, \code{"lme.ML"} or \code{"lme.REML"}, depending on the fitting
criterion used.}

\item{mgcv.conv}{ A list of convergence diagnostics relating to the
\code{"magic"} parts of smoothing parameter estimation - this will not be very meaningful for pure \code{"outer"}
estimation of smoothing parameters. The items are: \code{full.rank}, The apparent rank of the problem given the model matrix and 
constraints; \code{rank}, The numerical rank of the problem;
 \code{fully.converged}, \code{TRUE} is multiple GCV/UBRE converged by meeting 
convergence criteria and \code{FALSE} if method stopped with a steepest descent step 
failure; \code{hess.pos.def}Was the hessian of the GCV/UBRE score positive definite at 
smoothing parameter estimation convergence?; \code{iter} How many iterations were required to find the smoothing parameters?
\code{score.calls}, and how many times did the GCV/UBRE score have to be
evaluated?; \code{rms.grad}, root mean square of the gradient of the GCV/UBRE score at 
convergence.
} % end of mgcv.conv listing

\item{min.edf}{Minimum possible degrees of freedom for whole model.}

\item{model}{model frame containing all variables needed in original model fit.}

\item{na.action}{The \code{\link{na.action}} used in fitting.}

\item{nsdf}{number of parametric, non-smooth, model terms including the
          intercept.}

\item{null.deviance}{deviance for single parameter model.} 

\item{offset}{model offset.}

\item{optimizer}{\code{optimizer} argument to \code{\link{gam}}, or \code{"magic"} if it's a pure 
additive model.}

\item{outer.info}{If `outer' iteration has been used to fit the model (see
\code{\link{gam}} argument \code{optimizer}) then this is present and contains whatever was
returned by the optimization routine used (currently \code{\link{nlm}} or \code{\link{optim}}). }

\item{paraPen}{If the \code{paraPen} argument to \code{\link{gam}} was used then this provides
information on the parametric penalties. \code{NULL} otherwise.}

\item{pred.formula}{one sided formula containing variables needed for prediction, used by \code{predict.gam}}

\item{prior.weights}{prior weights on observations.} 

\item{pterms}{\code{terms} object for strictly parametric part of model.}

\item{R}{Factor R from QR decomposition of weighted model matrix, unpivoted to be in
         same column order as model matrix (so need not be upper triangular).}

\item{rank}{apparent rank of fitted model.}

\item{reml.scale}{The scale (RE)ML scale parameter estimate, if (P-)(RE)ML used 
for smoothness estimation. }

\item{residuals}{the working residuals for the fitted model.}

\item{rV}{If present, \code{rV\%*\%t(rV)*sig2} gives the estimated Bayesian covariance matrix.} 

\item{scale}{when present, the scale (as \code{sig2})}

\item{scale.estimated}{ \code{TRUE} if the scale parameter was estimated, \code{FALSE} otherwise.}

\item{sig2}{estimated or supplied variance/scale parameter.}

\item{smooth}{list of smooth objects, containing the basis information for each term in the 
model formula in the order in which they appear. These smooth objects are what gets returned by
the \code{\link{smooth.construct}} objects.}

\item{sp}{estimated smoothing parameters for the model. These are the underlying smoothing
parameters, subject to optimization. For the full set of smoothing parameters multiplying the 
penalties see \code{full.sp}. Divide the scale parameter by the smoothing parameters to get,
variance components, but note that this is not valid for smooths that have used rescaling to 
improve conditioning.}

\item{terms}{\code{terms} object of \code{model} model frame.}

\item{var.summary}{A named list of summary information on the predictor variables. If
a parametric variable is a matrix, then the summary is a one row matrix, containing the 
observed data value closest to the column median, for each matrix column. If the variable 
is a factor the then summary is the modal factor level, returned as a factor, with levels 
corresponding to those of the data. For numerics and matrix arguments of smooths, the summary 
is the mean, nearest observed value to median and maximum, as a numeric vector. Used by 
\code{\link{vis.gam}}, in particular. }

\item{Ve}{frequentist estimated covariance matrix for the parameter
estimators. Particularly useful for testing whether terms are zero. Not so
useful for CI's as smooths are usually biased.}


\item{Vp}{estimated covariance matrix for the parameters. This is a Bayesian
posterior covariance matrix that results from adopting a particular Bayesian
model of the smoothing process. Paricularly useful for creating
credible/confidence intervals.}

\item{weights}{final weights used in IRLS iteration.}

\item{y}{response data.}

}

\references{

A Key Reference on this implementation:

Wood, S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
& Hall/ CRC, Boca Raton, Florida

Key Reference on GAMs generally:

Hastie (1993) in Chambers and Hastie (1993) Statistical Models in S. Chapman
and Hall.

Hastie and Tibshirani (1990) Generalized Additive Models. Chapman and Hall.

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\section{WARNINGS }{ This model object is different to that described in
Chambers and Hastie (1993) in order to allow smoothing parameter estimation etc.

} 

\seealso{\code{\link{gam}}}




\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..







