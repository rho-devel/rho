\name{gam.control}
\alias{gam.control}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Setting GAM fitting defaults}
\description{ This is an internal function of package \code{mgcv} which allows 
control of the numerical options for fitting a GAM. 
Typically users will want to modify the defaults if model fitting fails to
converge, or if the warnings are generated which suggest a 
loss of numerical stability during fitting.  To change the default
choise of fitting method, see \code{\link{gam}} arguments \code{method} and \code{optimizer}.
}
\usage{
gam.control(nthreads=1,irls.reg=0.0,epsilon = 1e-06, maxit = 100,
            mgcv.tol=1e-7,mgcv.half=15, trace = FALSE,
            rank.tol=.Machine$double.eps^0.5,
            nlm=list(),optim=list(),newton=list(),
            outerPIsteps=0,idLinksBases=TRUE,scalePenalty=TRUE,
            keepData=FALSE) 
}
\arguments{ 
\item{nthreads}{Some parts of some smoothing parameter selection methods (e.g. REML) can use some
parallelization in the C code if your R installation supports openMP, and \code{nthreads} is set to 
more than 1. Note that it is usually better to use the number of physical cores here, rather than the 
number of hyper-threading cores.}
\item{irls.reg}{For most models this should be 0. The iteratively re-weighted least squares method
by which GAMs are fitted  can fail to converge in some circumstances. For example, data with many zeroes can cause 
problems in a model with a log link, because a mean of zero corresponds to an infinite range of linear predictor 
values. Such convergence problems are caused by a fundamental lack of identifiability, but do not show up as 
lack of identifiability in the penalized linear model problems that have to be solved at each stage of iteration.
In such circumstances it is possible to apply a ridge regression penalty to the model to impose identifiability, and 
\code{irls.reg} is the size of the penalty.
}

\item{epsilon}{This is used for judging conversion of the GLM IRLS loop in
\code{\link{gam.fit}} or \code{\link{gam.fit3}}.}

\item{maxit}{Maximum number of IRLS iterations to perform.}


\item{mgcv.tol}{The convergence tolerance parameter to use in GCV/UBRE optimization.}

\item{mgcv.half}{If a step of  the GCV/UBRE optimization method leads 
to a worse GCV/UBRE score, then the step length is halved. This is
the number of halvings to try before giving up.}

\item{trace}{Set this to \code{TRUE} to turn on diagnostic output.}


\item{rank.tol}{The tolerance used to estimate the rank of the fitting
problem.}

\item{nlm}{list of control parameters to pass to \code{\link{nlm}} if this is
used for outer estimation of smoothing parameters (not default). See details.}

\item{optim}{list of control parameters to pass to \code{\link{optim}} if this
is used for outer estimation of smoothing parameters (not default). See details.}

\item{newton}{list of control parameters to pass to default Newton optimizer
used for outer estimation of log smoothing parameters. See details.}

\item{outerPIsteps}{The number of performance interation steps used to
initialize outer iteration.}

\item{idLinksBases}{If smooth terms have their smoothing parameters linked via 
the \code{id} mechanism (see \code{\link{s}}), should they also have the same 
bases. Set this to \code{FALSE} only if you are sure you know what you are doing 
(you should almost surely set \code{scalePenalty} to \code{FALSE} as well in this 
case).}

\item{scalePenalty}{\code{\link{gamm}} is somewhat sensitive to the absolute scaling 
of the penalty matrices of a smooth relative to its model matrix. This option rescales 
the penalty matrices to accomodate this problem. Probably should be set to \code{FALSE} 
if you are linking smoothing parameters but have set \code{idLinkBases} to \code{FALSE}.}

\item{keepData}{Should a copy of the original \code{data} argument be kept in the \code{gam} 
object? Strict compatibility with class \code{glm} would keep it, but it wastes space to
do so. }
}

\details{ 

Outer iteration using \code{newton} is controlled by the list \code{newton}
with the following elements: \code{conv.tol} (default
1e-6) is the relative convergence tolerance; \code{maxNstep} is the maximum
length allowed for an element of the Newton search direction (default 5);
\code{maxSstep} is the maximum length allowed for an element of the steepest
descent direction (only used if Newton fails - default 2); \code{maxHalf} is
the maximum number of step halvings to permit before giving up (default 30).

If outer iteration using \code{\link{nlm}} is used for fitting, then the control list
\code{nlm} stores control arguments for calls to routine
\code{\link{nlm}}. The list has the following named elements: (i) \code{ndigit} is
the number of significant digits in the GCV/UBRE score - by default this is
worked out from \code{epsilon}; (ii) \code{gradtol} is the tolerance used to
judge convergence of the gradient of the GCV/UBRE score to zero - by default
set to \code{10*epsilon}; (iii) \code{stepmax} is the maximum allowable log
smoothing parameter step - defaults to 2; (iv) \code{steptol} is the minimum
allowable step length - defaults to 1e-4; (v) \code{iterlim} is the maximum
number of optimization steps allowed - defaults to 200; (vi)
\code{check.analyticals} indicates whether the built in exact derivative
calculations should be checked numerically - defaults to \code{FALSE}. Any of
these which are not supplied and named in the list are set to their default
values.

Outer iteration using \code{\link{optim}} is controlled using list
\code{optim}, which currently has one element: \code{factr} which takes
default value 1e7.
}


\references{

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. J. Amer. Statist. Ass.99:673-686.


\url{http://www.maths.bath.ac.uk/~sw283/}


}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{\code{\link{gam}}, \code{\link{gam.fit}}, \code{\link{glm.control}} }

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




