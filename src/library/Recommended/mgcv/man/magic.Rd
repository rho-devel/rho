\name{magic}
\alias{magic}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Stable Multiple Smoothing Parameter Estimation by GCV or UBRE}
\description{
Function to efficiently estimate smoothing parameters in generalized
ridge regression problems with multiple (quadratic) penalties, by GCV 
or UBRE. The function uses Newton's method in multi-dimensions, backed up by 
steepest descent to iteratively adjust the smoothing parameters for each penalty 
(one penalty may have a smoothing parameter fixed at 1). 

For maximal numerical stability the method is based on orthogonal decomposition methods, 
and attempts to deal with numerical rank deficiency gracefully using a truncated singular 
value decomposition approach. 
} %- end description

\usage{
magic(y,X,sp,S,off,L=NULL,lsp0=NULL,rank=NULL,H=NULL,C=NULL,
      w=NULL,gamma=1,scale=1,gcv=TRUE,ridge.parameter=NULL,
      control=list(tol=1e-6,step.half=25,rank.tol=
      .Machine$double.eps^0.5),extra.rss=0,n.score=length(y),nthreads=1)
}
%- maybe also `usage' for other objects documented here.
\arguments{

\item{y}{is the response data vector.}

\item{X}{is the model matrix (more columns than rows are allowed).}

\item{sp}{is the array of smoothing parameters. The vector \code{L\%*\%log(sp)
+ lsp0} 
contains the logs of the smoothing parameters that actually multiply the penalty matrices stored in 
\code{S} (\code{L} is taken as the identity if \code{NULL}). 
Any \code{sp} values that are negative are autoinitialized, otherwise they are taken as supplying 
starting values. A supplied starting value will be reset to a default starting value if the
gradient of the GCV/UBRE score is too small at the supplied value.  }
 
\item{S}{ is a list of of penalty matrices. \code{S[[i]]} is the ith penalty matrix, but note
that it is not stored as a full matrix, but rather as the smallest square matrix including all 
the non-zero elements of the penalty matrix. Element 1,1 of \code{S[[i]]}  occupies 
element \code{off[i]}, \code{off[i]} of the ith penalty matrix. Each \code{S[[i]]} must be 
positive semi-definite. Set to \code{list()} if there are no smoothing
parameters to be estimated. }

\item{off}{is an array indicating the first parameter in the parameter vector that is 
penalized by the penalty involving \code{S[[i]]}.}

\item{L}{is a matrix mapping \code{log(sp)} to the log smoothing parameters that actually multiply the
penalties defined by the elemts of \code{S}. Taken as the identity, if \code{NULL}. See above under \code{sp}.}

\item{lsp0}{If \code{L} is not \code{NULL} this is a vector of constants in
the linear transformation from \code{log(sp)} to the actual log smoothing
parameters. So the logs of the smoothing parameters multiplying the
\code{S[[i]]} are given by \code{L\%*\%log(sp) + lsp0}. Taken as 0 if \code{NULL}.}

\item{rank}{ is an array specifying the ranks of the penalties. This is useful, but not 
essential, for forming square roots of the penalty matrices.}

\item{H}{ is the optional offset penalty - i.e. a penalty with a smoothing parameter fixed at 
1. This is useful for allowing regularization of the estimation process, fixed smoothing 
penalties etc.}

 \item{C}{ is the optional matrix specifying any linear equality constraints on the fitting 
problem. If \eqn{\bf b}{b} is the parameter vector then the parameters are forced to satisfy 
\eqn{ {\bf Cb} = {\bf 0} }{Cb=0}. }

\item{w}{ the regression weights. If this is a matrix then it is taken as being the 
square root of the inverse of the covariance matrix of \code{y}, specifically 
\eqn{ {\bf V}_y^{-1} = {\bf w}^\prime{\bf w}}{V_y^{-1}=w'w}. If \code{w} is an array then 
it is taken as the diagonal of this matrix, or simply the weight for each element of 
\code{y}. See below for an example using this.}

\item{gamma}{is an inflation factor for the model degrees of freedom in the GCV or UBRE 
score.}

\item{scale}{ is the scale parameter for use with UBRE.}

\item{gcv}{ should be set to \code{TRUE} if GCV is to be used, \code{FALSE} for UBRE.}

\item{ridge.parameter}{It is sometimes useful to apply a ridge penalty to the fitting problem, 
penalizing the parameters in the constrained space directly. Setting this parameter to a value 
greater than zero will cause such a penalty to be used, with the magnitude given by the 
parameter value.}

\item{control}{ is a list of iteration control constants with the following elements:

\describe{

\item{tol}{The tolerance to use in judging convergence.}

\item{step.half}{If a trial step fails then the method tries halving it up to a maximum of 
\code{step.half} times.}

\item{rank.tol}{is a constant used to test for numerical rank deficiency of the problem. 
Basically any singular value less than \code{rank_tol} multiplied by the largest singular value of 
the  problem is set to zero.}
}

} %- end of control


\item{extra.rss}{is a constant to be added to the residual sum of squares
(squared norm) term in the calculation of the GCV, UBRE and scale parameter
estimate. In conjuction with \code{n.score}, this is useful for certain methods for dealing with very large data
sets.}

\item{n.score}{number to use as the number of data in GCV/UBRE score
calculation: usually the actual number of data, but there are methods 
for dealing with very large datasets that change this.}

\item{nthreads}{\code{magic} can make use of multiple threads if this is set to >1.}
}

\details{ 

The method is a computationally efficient means of applying GCV or UBRE (often approximately 
AIC) to the 
problem of smoothing parameter selection in generalized ridge regression problems 
of the form:
\deqn{ minimise~ \| { \bf W} ({ \bf Xb - y} ) \|^2 + {\bf b}^\prime {\bf Hb} + \sum_{i=1}^m
\theta_i {\bf b^\prime S}_i{\bf b} }{ min ||W(Xb-y)||^2 + b'Hb +
theta_1 b'S_1 b + theta_2 b'S_2 b + . . .}
possibly subject to constraints \eqn{ {\bf Cb}={\bf 0}}{Cb=0}. 
\eqn{ {\bf X}}{X} is a design matrix, \eqn{\bf b}{b} a parameter vector, 
\eqn{\bf y}{y} a data vector, \eqn{\bf W}{W} a weight matrix,
\eqn{ {\bf S}_i}{S_i} a positive semi-definite matrix  of coefficients
defining the ith penalty with associated smoothing parameter \eqn{\theta_i}{theta_i}, 
\eqn{\bf H}{H} is the positive semi-definite offset penalty matrix  and \eqn{\bf C}{C} a 
matrix of coefficients defining any linear equality constraints on the problem. 
\eqn{ {\bf X}}{X} need not be of full column rank.

The \eqn{\theta_i}{theta_i} are chosen to minimize either the GCV score:

\deqn{V_g = \frac{n\|{\bf W}({\bf y} - {\bf Ay})\|^2}{[tr({\bf I} - \gamma {\bf A})]^2}}{V_g = n ||W(y-Ay)||^2/[tr(I - g A)]^2}

or the UBRE score:

\deqn{V_u=\|{\bf W}({\bf y}-{\bf Ay})\|^2/n-2 \phi tr({\bf I}-\gamma {\bf A})/n + \phi}{
V_u =||W(y-Ay||^2/n - 2 s tr(I - g A)/n + s }

where \eqn{\gamma}{g} is \code{gamma} the inflation factor for degrees of freedom (usually set to 1) and \eqn{\phi}{s} 
is \code{scale}, the scale parameter. \eqn{\bf A}{A} is the hat matrix (influence matrix) for the fitting problem (i.e
the matrix mapping data to fitted values). Dependence of the scores on the smoothing parameters is through \eqn{\bf A}{A}. 

The method operates by  Newton or steepest descent updates of the logs of the 
\eqn{\theta_i}{theta_i}. A key aspect of the method is stable and economical calculation of the 
first and second derivatives of the scores w.r.t. the log smoothing parameters. 
Because the GCV/UBRE scores are flat w.r.t. very large or very small \eqn{\theta_i}{theta_i}, 
it's important to get good starting parameters, and to be careful not to step into a flat region
of the smoothing parameter space. For this reason the algorithm rescales any Newton step that 
would result in a \eqn{log(\theta_i)}{log(theta_i)} change of more than 5. Newton steps are 
only used if the Hessian of the GCV/UBRE is postive definite, otherwise steepest descent is 
used. Similarly steepest descent is used if the Newton step has to be contracted too far 
(indicating that the quadratic model underlying Newton is poor). All initial steepest descent 
steps are scaled so that their largest component is 1. However a step is calculated, 
it is never expanded if it is successful (to avoid flat portions of the objective), 
but steps are successively halved if they do not decrease the GCV/UBRE score, until 
they do, or the direction is deemed to have failed. (Given the smoothing parameters the optimal 
\eqn{\bf b}{b} parameters are easily found.)


The method is coded in \code{C} with matrix factorizations performed using LINPACK and LAPACK routines.
}
\value{The function returns a list with the following items:

\item{b}{The best fit parameters given the estimated smoothing parameters.}

\item{scale}{the estimated (GCV) or supplied (UBRE) scale parameter.}

\item{score}{the minimized GCV or UBRE score.}

\item{sp}{an array of the estimated smoothing parameters.}

\item{sp.full}{an array of the smoothing parameters that actually multiply the elements of
\code{S} (same as \code{sp} if \code{L} was \code{NULL}). This is \code{exp(L\%*\%log(sp))}.}

\item{rV}{a factored form of the parameter covariance matrix. The (Bayesian)  covariance
matrix of the parametes \code{b} is given by \code{rV\%*\%t(rV)*scale}. }

\item{gcv.info}{is a list of information about the performance of the method with the following elements:
\describe{
\item{full.rank}{The apparent rank of the problem: number of parameters less number of equality constraints.}
\item{rank}{The estimated actual rank of the problem (at the final iteration of the method).}
\item{fully.converged}{is \code{TRUE} if the method converged by satisfying the convergence criteria, and \code{FALSE} if it coverged 
by failing to decrease the score along the search direction.}
\item{hess.pos.def}{is \code{TRUE} if the hessian of the UBRE or GCV score was positive definite at convergence.}
\item{iter}{is the number of Newton/Steepest descent iterations taken.}
\item{score.calls}{is the number of times that the GCV/UBRE score had to be evaluated.}
\item{rms.grad}{is the root mean square of the gradient of the UBRE/GCV score w.r.t. the smoothing parameters.}
\item{R}{The factor R from the QR decomposition of the weighted model matrix. This is un-pivoted so that column 
order corresponds to \code{X}. So it may not be upper triangular.}}
}
Note that some further useful quantities can be obtained using \code{\link{magic.post.proc}}.

}
\references{

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. J. Amer. Statist. Ass. 99:673-686

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  
\code{\link{magic.post.proc}},\code{\link{gam}}
}

\examples{
## Use `magic' for a standard additive model fit ... 
   library(mgcv)
   set.seed(1);n <- 200;sig <- 1
   dat <- gamSim(1,n=n,scale=sig)
   k <- 30
## set up additive model
   G <- gam(y~s(x0,k=k)+s(x1,k=k)+s(x2,k=k)+s(x3,k=k),fit=FALSE,data=dat)
## fit using magic (and gam default tolerance)
   mgfit <- magic(G$y,G$X,G$sp,G$S,G$off,rank=G$rank,
                  control=list(tol=1e-7,step.half=15))
## and fit using gam as consistency check
   b <- gam(G=G)
   mgfit$sp;b$sp  # compare smoothing parameter estimates
   edf <- magic.post.proc(G$X,mgfit,G$w)$edf # get e.d.f. per param
   range(edf-b$edf)  # compare

## p>n example... fit model to first 100 data only, so more
## params than data...

   mgfit <- magic(G$y[1:100],G$X[1:100,],G$sp,G$S,G$off,rank=G$rank)
   edf <- magic.post.proc(G$X[1:100,],mgfit,G$w[1:100])$edf

## constrain first two smooths to have identical smoothing parameters
   L <- diag(3);L <- rbind(L[1,],L)
   mgfit <- magic(G$y,G$X,rep(-1,3),G$S,G$off,L=L,rank=G$rank,C=G$C)

## Now a correlated data example ... 
    library(nlme)
## simulate truth
    set.seed(1);n<-400;sig<-2
    x <- 0:(n-1)/(n-1)
    f <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
## produce scaled covariance matrix for AR1 errors...
    V <- corMatrix(Initialize(corAR1(.6),data.frame(x=x)))
    Cv <- chol(V)  # t(Cv)%*%Cv=V
## Simulate AR1 errors ...
    e <- t(Cv)\%*\%rnorm(n,0,sig) # so cov(e) = V * sig^2
## Observe truth + AR1 errors
    y <- f + e 
## GAM ignoring correlation
    par(mfrow=c(1,2))
    b <- gam(y~s(x,k=20))
    plot(b);lines(x,f-mean(f),col=2);title("Ignoring correlation")
## Fit smooth, taking account of *known* correlation...
    w <- solve(t(Cv)) # V^{-1} = w'w
    ## Use `gam' to set up model for fitting...
    G <- gam(y~s(x,k=20),fit=FALSE)
    ## fit using magic, with weight *matrix*
    mgfit <- magic(G$y,G$X,G$sp,G$S,G$off,rank=G$rank,C=G$C,w=w)
## Modify previous gam object using new fit, for plotting...    
    mg.stuff <- magic.post.proc(G$X,mgfit,w)
    b$edf <- mg.stuff$edf;b$Vp <- mg.stuff$Vb
    b$coefficients <- mgfit$b 
    plot(b);lines(x,f-mean(f),col=2);title("Known correlation")
}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..





