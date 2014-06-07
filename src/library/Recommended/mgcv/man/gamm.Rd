\name{gamm}
\alias{gamm}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generalized Additive Mixed Models}
\description{ Fits the specified  generalized additive mixed model (GAMM) to
data, by a call to \code{lme} in the normal errors identity link case, or by 
a call to \code{gammPQL} (a modification of \code{glmmPQL} from the \code{MASS} library) otherwise. 
In the latter case estimates are only approximately MLEs. The routine is typically 
slower than \code{gam}, and not quite as numerically robust. 

To use \code{lme4} in place of \code{nlme} as the underlying fitting engine, 
see \code{gamm4} from package \code{gamm4}.

Smooths are specified as in a call to \code{\link{gam}} as part of the fixed 
effects model formula, but the wiggly components of the smooth are treated as 
random effects. The random effects structures and correlation structures 
availabale for \code{lme} are used to specify other random effects and 
correlations. 

It is assumed that the random effects and correlation structures are employed 
primarily to model residual correlation in the data and that the prime interest
is in inference about the terms in the fixed effects model formula including 
the smooths. For this reason the routine calculates a posterior covariance 
matrix for the coefficients of all the terms in the fixed effects formula, 
including the smooths.

To use this function effectively it helps to be quite familiar with the use of
\code{\link{gam}} and \code{\link[nlme]{lme}}.

}

\usage{
gamm(formula,random=NULL,correlation=NULL,family=gaussian(),
data=list(),weights=NULL,subset=NULL,na.action,knots=NULL,
control=list(niterEM=0,optimMethod="L-BFGS-B"),
niterPQL=20,verbosePQL=TRUE,method="ML",...)
}

\arguments{ 
\item{formula}{ A GAM formula (see also \code{\link{formula.gam}} and \code{\link{gam.models}}).
This is like the formula for a \code{glm} except that smooth terms (\code{\link{s}} and \code{\link{te}})
can be added to the right hand side of the
formula. Note that \code{id}s for smooths and fixed smoothing parameters are
not supported.} 

\item{random}{The (optional) random effects structure as specified in a call to 
\code{\link[nlme]{lme}}: only the \code{list} form is allowed, to facilitate 
manipulation of the random effects structure within \code{gamm} in order to deal
with smooth terms. See example below.}

\item{correlation}{An optional \code{corStruct} object 
(see \code{\link[nlme]{corClasses}}) as used to define correlation 
structures in \code{\link[nlme]{lme}}. Any grouping factors in the formula for
this object are assumed to be nested within any random effect grouping
factors, without the need to make this explicit in the formula (this is 
slightly different to the behaviour of \code{lme}). This 
is a GEE approach to correlation in the generalized case. See examples below.}

\item{family}{A \code{family} as used in a call to \code{\link{glm}} or \code{\link{gam}}.
The default \code{gaussian} with identity link causes \code{gamm} to fit by a direct call to
\code{\link[nlme]{lme}} procided there is no offset term, otherwise
\code{gammPQL} is used.}

\item{data}{ A data frame or list containing the model response variable and 
covariates required by the formula.  By default the variables are taken 
from \code{environment(formula)}, typically the environment from 
which \code{gamm} is called.} 

\item{weights}{In the generalized case, weights with the same meaning as
\code{\link{glm}} weights. An \code{lme} type weights argument may only be
used in the identity link gaussian case, with no offset (see documentation for \code{lme}
for details of how to use such an argument).}

\item{subset}{ an optional vector specifying a subset of observations to be
          used in the fitting process.}

\item{na.action}{ a function which indicates what should happen when the data
          contain `NA's.  The default is set by the `na.action' setting
          of `options', and is `na.fail' if that is unset.  The
          ``factory-fresh'' default is `na.omit'.}



\item{knots}{this is an optional list containing user specified knot values to be used for basis construction. 
Different terms can use different numbers of knots, unless they share a covariate.
}

\item{control}{A list of fit control parameters for \code{\link[nlme]{lme}} to replace the 
defaults returned by \code{\link[nlme]{lmeControl}}. Note the setting 
for the number of EM iterations used by \code{lme}: smooths are set up using custom \code{pdMat} classes,
which are currently not supported by the EM iteration code. If you supply a list of control values,
it is advisable to include \code{niterEM=0}, as well, and only increase from 0
if you want to perturb the starting values used in model fitting
(usually to worse values!). The \code{optimMethod} option is only used if your
version of R does not have the \code{nlminb} optimizer function.}

\item{niterPQL}{Maximum number of PQL iterations (if any).}

\item{verbosePQL}{Should PQL report its progress as it goes along?}

\item{method}{Which of \code{"ML"} or \code{"REML"} to use in the Gaussian
additive mixed model case when \code{lme} is called directly. Ignored in the
generalized case (or if the model has an offset), in which case \code{gammPQL} is used.}

\item{...}{further arguments for passing on e.g. to \code{lme}} 
}
%- maybe also `usage' for other objects documented here.

\details{ The Bayesian model of spline smoothing introduced by Wahba (1983) and Silverman (1985) opens 
up the possibility of estimating the degree of smoothness of terms in a generalized additive model
as variances of the wiggly components of the smooth terms treated as random effects. Several authors 
have recognised this (see Wang 1998; Ruppert, Wand and Carroll, 2003) and in the normal errors, 
identity link case estimation can 
be performed using general linear mixed effects modelling software such as \code{lme}. In the generalized case only 
approximate inference is so far available, for example using the Penalized Quasi-Likelihood approach of Breslow 
and Clayton (1993) as implemented in \code{glmmPQL} by Venables and Ripley (2002). 
One advantage of this approach is that it allows correlated errors to be dealt with via random effects 
or the correlation structures available in the \code{nlme} library (using correlation structures beyond the 
strictly additive case amounts to using a GEE approach to fitting). 


Some details of how GAMs are represented as mixed models and estimated using
\code{lme} or \code{gammPQL} in \code{gamm} can be found in Wood (2004 ,2006a,b). In addition \code{gamm} obtains a posterior covariance matrix for the parameters of all the fixed effects and the smooth terms. The approach is similar to that described in Lin & Zhang (1999) - the covariance matrix of the data (or pseudodata in the generalized case) implied by the weights, correlation and random effects structure is obtained, based on the estimates of the parameters of these terms and this is used to obtain the posterior covariance matrix of the fixed and smooth effects. 

The bases used to represent smooth terms are the same as those used in \code{\link{gam}}, although adaptive
smoothing bases are not available.

In the event of \code{lme} convergence failures, consider
modifying \code{option(mgcv.vc.logrange)}: reducing it helps to remove
indefiniteness in the likelihood, if that is the problem, but too large a
reduction can force over or undersmoothing. See \code{\link{notExp2}} for more
information on this option. Failing that, you can try increasing the
\code{niterEM} option in \code{control}: this will perturb the starting values
used in fitting, but usually to values with lower likelihood! Note that this
version of \code{gamm} works best with R 2.2.0 or above and \code{nlme}, 3.1-62 and above,
since these use an improved optimizer. 
}


\value{ Returns a list with two items:
\item{gam}{an object of class \code{gam}, less information relating to
GCV/UBRE model selection. At present this contains enough information to use
\code{predict}, \code{summary} and \code{print} methods and \code{vis.gam},
but not to use e.g. the \code{anova} method function to compare models.}
\item{lme}{the fitted model object returned by \code{lme} or \code{gammPQL}. Note that the model formulae and grouping 
structures may appear to be rather bizarre, because of the manner in which the GAMM is split up and the calls to 
\code{lme} and \code{gammPQL} are constructed.}

}

\references{

Breslow, N. E. and Clayton, D. G. (1993) Approximate inference in generalized linear 
mixed models. Journal of the American Statistical Association 88, 9-25.

Lin, X and Zhang, D. (1999) Inference in generalized additive mixed models by using smoothing 
splines. JRSSB. 55(2):381-400

Pinheiro J.C. and Bates, D.M. (2000) Mixed effects Models in S and S-PLUS. Springer

Ruppert, D., Wand, M.P. and Carroll, R.J. (2003) Semiparametric Regression. 
Cambridge

Silverman, B.W. (1985) Some aspects of the spline smoothing approach to nonparametric regression.
JRSSB 47:1-52 

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics
with S. Fourth edition.  Springer.

Wahba, G. (1983) Bayesian confidence intervals for the cross validated smoothing spline. 
JRSSB 45:133-150

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. Journal of the American Statistical Association. 99:673-686

Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

Wood, S.N. (2006a) Low rank scale invariant tensor product smooths for
generalized additive mixed models. Biometrics 62(4):1025-1036

Wood S.N. (2006b) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.


Wang, Y. (1998) Mixed effects smoothing spline analysis of variance. J.R. Statist. Soc. B 60, 159-174

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\section{WARNINGS }{

\code{gamm} performs poorly with binary data, since it uses PQL. It is 
better to use \code{gam} with \code{s(...,bs="re")} terms, or \code{gamm4}.

\code{gamm} assumes that you know what you are doing! For example, unlike 
\code{glmmPQL} from \code{MASS} it will return the complete \code{lme} object
from the working model at convergence of the PQL iteration, including the `log 
likelihood', even though this is not the likelihood of the fitted GAMM. 

The routine will be very slow and memory intensive if correlation structures
are used for the very large groups of data. e.g. attempting to run the
spatial example in the examples section with many 1000's of data is definitely not 
recommended: often the correlations should only apply within clusters that can
be defined by a grouping factor, and provided these clusters do not get too huge
then fitting is usually possible.

Models must contain at least one random effect: either a smooth with non-zero
smoothing parameter, or a random effect specified in argument \code{random}.

\code{gamm} is not as numerically stable as \code{gam}: an \code{lme} call
will occasionally fail. See details section for suggestions, or try the 
`gamm4' package.

\code{gamm} is usually much slower than \code{gam}, and on some platforms you may need to
increase the memory available to R in order to use it with large data sets
(see \code{\link{mem.limits}}).

Note that the weights returned in the fitted GAM object are dummy, and not
those used by the PQL iteration: this makes partial residual plots look odd.

Note that the \code{gam} object part of the returned object is not complete in
the sense of having all the elements defined in \code{\link{gamObject}} and
does not inherit from \code{glm}: hence e.g. multi-model \code{anova} calls will not work.

The parameterization used for the smoothing parameters in \code{gamm}, bounds
them above and below by an effective infinity and effective zero. See
\code{\link{notExp2}} for details of how to change this. 

Linked smoothing parameters and adaptive smoothing are not supported.
} 

\seealso{\code{\link{magic}} for an alternative for correlated data,
\code{\link{te}}, \code{\link{s}}, 
\code{\link{predict.gam}},
  \code{\link{plot.gam}}, \code{\link{summary.gam}}, \code{\link{negbin}}, 
\code{\link{vis.gam}},\code{\link{pdTens}}, \code{gamm4} (
\url{http://cran.r-project.org/package=gamm4})
 }

\examples{
library(mgcv)
## simple examples using gamm as alternative to gam
set.seed(0) 
dat <- gamSim(1,n=400,scale=2)
b <- gamm(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b$gam,pages=1)
summary(b$lme) # details of underlying lme fit
summary(b$gam) # gam style summary of fitted model
anova(b$gam) 
gam.check(b$gam) # simple checking plots

b <- gamm(y~te(x0,x1)+s(x2)+s(x3),data=dat) 
op <- par(mfrow=c(2,2))
plot(b$gam)
par(op)
rm(dat)

## Add a factor to the linear predictor, to be modelled as random
dat <- gamSim(6,n=400,scale=.2,dist="poisson")
b2<-gamm(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
         data=dat,random=list(fac=~1))
plot(b2$gam,pages=1)
fac <- dat$fac
rm(dat)
vis.gam(b2$gam)


## now an example with autocorrelated errors....
n <- 400;sig <- 2
x <- 0:(n-1)/(n-1)
f <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
e <- rnorm(n,0,sig)
for (i in 2:n) e[i] <- 0.6*e[i-1] + e[i]
y <- f + e
op <- par(mfrow=c(2,2))
b <- gamm(y~s(x,k=20),correlation=corAR1())
plot(b$gam);lines(x,f-mean(f),col=2)
b <- gamm(y~s(x,k=20))
plot(b$gam);lines(x,f-mean(f),col=2)
b <- gam(y~s(x,k=20))
plot(b);lines(x,f-mean(f),col=2)

## more complicated autocorrelation example - AR errors
## only within groups defined by `fac'
e <- rnorm(n,0,sig)
for (i in 2:n) e[i] <- 0.6*e[i-1]*(fac[i-1]==fac[i]) + e[i]
y <- f + e
b <- gamm(y~s(x,k=20),correlation=corAR1(form=~1|fac))
plot(b$gam);lines(x,f-mean(f),col=2)
par(op) 

## more complex situation with nested random effects and within
## group correlation 

set.seed(0)
n.g <- 10
n<-n.g*10*4
## simulate smooth part...
dat <- gamSim(1,n=n,scale=2)
f <- dat$f
## simulate nested random effects....
fa <- as.factor(rep(1:10,rep(4*n.g,10)))
ra <- rep(rnorm(10),rep(4*n.g,10))
fb <- as.factor(rep(rep(1:4,rep(n.g,4)),10))
rb <- rep(rnorm(4),rep(n.g,4))
for (i in 1:9) rb <- c(rb,rep(rnorm(4),rep(n.g,4)))
## simulate auto-correlated errors within groups
e<-array(0,0)
for (i in 1:40) {
eg <- rnorm(n.g, 0, sig)
for (j in 2:n.g) eg[j] <- eg[j-1]*0.6+ eg[j]
e<-c(e,eg)
}
dat$y <- f + ra + rb + e
dat$fa <- fa;dat$fb <- fb
## fit model .... 
b <- gamm(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+
  s(x3,bs="cr"),data=dat,random=list(fa=~1,fb=~1),
  correlation=corAR1())
plot(b$gam,pages=1)
summary(b$gam)
vis.gam(b$gam)
## and a "spatial" example...
library(nlme);set.seed(1);n <- 200
dat <- gamSim(2,n=n,scale=0) ## standard example
attach(dat)
old.par<-par(mfrow=c(2,2))
contour(truth$x,truth$z,truth$f)  ## true function
f <- data$f                       ## true expected response
## Now simulate correlated errors...
cstr <- corGaus(.1,form = ~x+z)  
cstr <- Initialize(cstr,data.frame(x=data$x,z=data$z))
V <- corMatrix(cstr) ## correlation matrix for data
Cv <- chol(V)
e <- t(Cv) \%*\% rnorm(n)*0.05 # correlated errors
## next add correlated simulated errors to expected values
data$y <- f + e ## ... to produce response
b<- gamm(y~s(x,z,k=50),correlation=corGaus(.1,form=~x+z),
         data=data)
plot(b$gam) # gamm fit accounting for correlation
# overfits when correlation ignored.....  
b1 <- gamm(y~s(x,z,k=50),data=data);plot(b1$gam) 
b2 <- gam(y~s(x,z,k=50),data=data);plot(b2)
par(old.par)

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..







