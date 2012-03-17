\name{mgcv}
\alias{mgcv}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Multiple Smoothing Parameter Estimation by GCV or UBRE}
\description{
Function to efficiently estimate smoothing parameters in Generalized
Ridge Regression Problem with multiple (quadratic) penalties, by GCV 
or UBRE. The function uses Newton's method in multi-dimensions, backed up by steepest descent to iteratively 
adjust a set of relative smoothing parameters for each penalty. To ensure that the overall level of smoothing
is optimal, and to guard against trapping by local minima, a highly efficient global minimisation with respect to 
one overall smoothing parameter is also made at each iteration. This is the original Wood (2000) method. It has now 
been superceded by the methods in \code{\link{magic}} (Wood, 2004) and \code{\link{gam.fit3}} (Wood, 2011).

For a listing of all routines in the \code{mgcv} package type:\cr
\code{library(help="mgcv")}. For an overview of the \code{mgcv} package see \code{\link{mgcv-package}}.
}
\usage{
mgcv(y,X,sp,S,off,C=NULL,w=rep(1,length(y)),H=NULL,
     scale=1,gcv=TRUE,control=mgcv.control())
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{y}{The response data vector.}

\item{X}{The design matrix for the problem, note that \code{ncol(X)}
            must give the number of model parameters, while \code{nrow(X)} 
            should give the number of data.}

\item{sp}{ An array of smoothing parameters. If \code{control$fixed==TRUE} then these are taken as being the 
smoothing parameters. Otherwise any positive values are assumed to be initial estimates and negative values to
signal auto-initialization.}

\item{S}{A list of penalty matrices. Only the smallest square block containing all non-zero matrix
elements is actually stored, and \code{off[i]} indicates the element of the parameter vector that 
\code{S[[i]][1,1]} relates to.}

\item{off}{ Offset values indicating where in the overall parameter a particular stored penalty starts operating. 
For example if \code{p} is the model parameter vector and \code{k=nrow(S[[i]])-1}, then the ith penalty is given by \cr
\code{t(p[off[i]:(off[i]+k)])\%*\%S[[i]]\%*\%p[off[i]:(off[i]+k)]}.}


\item{C}{Matrix containing any linear equality constraints 
            on the problem (i.e. \eqn{\bf C}{C} in \eqn{ {\bf Cp}={\bf 0} }{Cp=0}).}

\item{w}{A vector of weights for the data (often proportional to the 
           reciprocal of the standard deviation of \code{y}). }

\item{H}{ A single fixed penalty matrix to be used in place of the multiple 
penalty matrices in \code{S}. \code{mgcv} cannot mix fixed and estimated penalties.}

\item{scale}{ This is the known scale parameter/error variance to use with UBRE. 
Note that it is assumed that the variance of \eqn{y_i}{y_i} is 
given by \eqn{\sigma^2/w_i}{\code{scale}/w_i}.}   

\item{gcv}{ If \code{gcv} is TRUE then smoothing parameters are estimated by GCV,
otherwise UBRE is used.}

\item{control}{A list of control options returned by \code{\link{mgcv.control}}.}
}

\details{ 

This is documentation for the code implementing the method described in section 
4 of 
Wood (2000) . The method is a computationally efficient means of applying GCV to 
the 
problem of smoothing parameter selection in generalized ridge regression problems 
of 
the form:
\deqn{ minimise~ \| { \bf W} ({ \bf Xp - y} ) \|^2 \rho +  \sum_{i=1}^m
\lambda_i {\bf p^\prime S}_i{\bf p} }{ min ||W(Xp-y)||^2 rho + 
lambda_1 p'S_1 p + lambda_1 p'S_2 p + . . .}
possibly subject to constraints \eqn{ {\bf Cp}={\bf 0}}{Cp=0}. 
\eqn{ {\bf X}}{X} is a design matrix, \eqn{\bf p}{p} a parameter vector, 
\eqn{\bf y}{y} a data vector, \eqn{\bf W}{W} a diagonal weight matrix,
\eqn{ {\bf S}_i}{S_i} a positive semi-definite matrix  of coefficients
defining the ith penalty and \eqn{\bf C}{C} a matrix of coefficients 
defining any linear equality constraints on the problem. The smoothing
parameters are the \eqn{\lambda_i}{lambda_i} but there is an overall
smoothing parameter \eqn{\rho}{rho} as well. Note that \eqn{ {\bf X}}{X}
must be of full column rank, at least when projected  into the null space
of any equality constraints.  

The method operates by alternating very efficient direct searches for 
\eqn{\rho}{rho}
with Newton or steepest descent updates of the logs of the \eqn{\lambda_i}{lambda_i}. 
Because the GCV/UBRE scores are flat w.r.t. very large or very small \eqn{\lambda_i}{lambda_i}, 
it's important to get good starting parameters, and to be careful not to step into a flat region
of the smoothing parameter space. For this reason the algorithm rescales any Newton step that 
would result in a \eqn{log(\lambda_i)}{log(lambda_i)} change of more than 5. Newton steps are only used
if the Hessian of the GCV/UBRE is postive definite, otherwise steepest descent is used. Similarly steepest 
descent is used if the Newton step has to be contracted too far (indicating that the quadratic model 
underlying Newton is poor). All initial steepest descent steps are scaled so that their largest component is
1. However a step is calculated, it is never expanded if it is successful (to avoid flat portions of the objective), 
but steps are successively halved if they do not decrease the GCV/UBRE score, until they do, or the direction is deemed to have 
failed. \code{M$conv} provides some convergence diagnostics.

The method is coded in \code{C} and is intended to be portable. It should be 
noted that seriously ill conditioned problems (i.e. with close to column rank 
deficiency in the design matrix) may cause problems, especially if weights vary 
wildly between observations.  
}
\value{ An object is returned with the following elements:
  
\item{b}{The best fit parameters given the estimated smoothing parameters.}

\item{scale}{The estimated or supplied scale parameter/error variance.}

\item{score}{The UBRE or GCV score.}

\item{sp}{The estimated (or supplied) smoothing parameters (\eqn{\lambda_i/\rho}{lambda_i/rho})}

\item{Vb}{Estimated covariance matrix of model parameters.}

\item{hat}{diagonal of the hat/influence matrix.}

\item{edf}{array of estimated degrees of freedom for each parameter.}

\item{info}{A list of convergence diagnostics, with the following elements:
\describe{
\item{edf}{Array of whole model estimated degrees of freedom.}
\item{score}{Array of ubre/gcv scores at the edfs for the final set of relative smoothing parameters.}
\item{g}{the gradient of the GCV/UBRE score w.r.t. the smoothing parameters at termination.}
\item{h}{the second derivatives corresponding to \code{g} above - i.e. the leading diagonal of the Hessian.}
\item{e}{the eigenvalues of the Hessian. These should all be non-negative!}
\item{iter}{the number of iterations taken.}
\item{in.ok}{\code{TRUE} if the second smoothing parameter guess improved the GCV/UBRE score. (Please report examples 
where this is \code{FALSE})}
\item{step.fail}{\code{TRUE} if the algorithm terminated by failing to improve the GCV/UBRE score rather than by "converging". 
Not necessarily a problem, but check the above derivative information quite carefully.}
} %info
}
}
\references{

Gu and Wahba (1991) Minimizing GCV/GML scores with multiple smoothing parameters via
the Newton method. SIAM J. Sci. Statist. Comput. 12:383-398

Wood, S.N. (2000)  Modelling and Smoothing Parameter Estimation
with Multiple  Quadratic Penalties. J.R.Statist.Soc.B 62(2):413-428

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. J. Amer. Statist. Ass. 99:673-686

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36


\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\section{WARNING }{ The method may not behave well with near column rank defficient \eqn{ {\bf
X}}{X}
especially in contexts where the weights vary wildly. } 

\seealso{  
\code{\link{gam}},
\code{\link{magic}}
}

\examples{
\dontrun{
library(help="mgcv") # listing of all routines

set.seed(1);n<-400;sig2<-4
x0 <- runif(n, 0, 1);x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1);x3 <- runif(n, 0, 1)
f <- 2 * sin(pi * x0)
f <- f + exp(2 * x1) - 3.75887
f <- f+0.2*x2^11*(10*(1-x2))^6+10*(10*x2)^3*(1-x2)^10-1.396
e <- rnorm(n, 0, sqrt(sig2))
y <- f + e
# set up additive model
G<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),fit=FALSE)
# fit using mgcv
mgfit<-mgcv(G$y,G$X,G$sp,G$S,G$off,C=G$C)
} 
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..





