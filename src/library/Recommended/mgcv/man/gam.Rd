\name{gam}
\alias{gam}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generalized additive models with integrated smoothness estimation}

\description{ Fits a generalized additive model (GAM) to
data, the term `GAM' being taken to include any quadratically penalized GLM.  
The degree of smoothness of model terms is estimated as part of
fitting. \code{gam} can also fit any GLM subject to multiple quadratic penalties (including 
estimation of degree of penalization). Isotropic or scale invariant smooths of any number of variables are
available as model terms, as are linear functionals of such smooths; confidence/credible intervals are readily
available for any quantity predicted using a fitted model; \code{gam} is extendable: users can add smooths. 

Smooth terms are represented using penalized regression splines (or similar smoothers)
with smoothing parameters selected by GCV/UBRE/AIC/REML or by regression splines with
fixed degrees of freedom (mixtures of the two are permitted). Multi-dimensional smooths are 
available using penalized thin plate regression splines (isotropic) or tensor product splines 
(when an isotropic smooth is inappropriate). For an overview of the smooths available see \code{\link{smooth.terms}}. 
For more on specifying models see \code{\link{gam.models}}, \code{\link{random.effects}} and \code{\link{linear.functional.terms}}. For more on model selection see \code{\link{gam.selection}}. Do read \code{\link{gam.check}} and \code{\link{choose.k}}.

See \link[gam]{gam} from package \code{gam}, for GAMs via the original Hastie and Tibshirani approach (see details for differences to this implementation).

For very large datasets see \code{\link{bam}}, for mixed GAM see \code{\link{gamm}} and \code{\link{random.effects}}.
}
\usage{

gam(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,
    na.action,offset=NULL,method="GCV.Cp",
    optimizer=c("outer","newton"),control=list(),scale=0,
    select=FALSE,knots=NULL,sp=NULL,min.sp=NULL,H=NULL,gamma=1,
    fit=TRUE,paraPen=NULL,G=NULL,in.out,...)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{formula}{ A GAM formula (see \code{\link{formula.gam}} and also \code{\link{gam.models}}). 
This is exactly like the formula for a GLM except that smooth terms, \code{s} and \code{te} can be added 
to the right hand side to specify that the linear predictor depends on smooth functions of predictors 
(or linear functionals of these).
} 

\item{family}{
This is a family object specifying the distribution and link to use in
fitting etc. See \code{\link{glm}} and \code{\link{family}} for more
details. A negative binomial family is provided: see \code{\link{negbin}}. 
\code{quasi} families actually result in the use of extended quasi-likelihood 
if \code{method} is set to a RE/ML method (McCullagh and Nelder, 1989, 9.6).
} 

\item{data}{ A data frame or list containing the model response variable and 
covariates required by the formula. By default the variables are taken 
from \code{environment(formula)}: typically the environment from 
which \code{gam} is called.} 

\item{weights}{ prior weights on the data.}

\item{subset}{ an optional vector specifying a subset of observations to be
          used in the fitting process.}

\item{na.action}{ a function which indicates what should happen when the data
          contain `NA's.  The default is set by the `na.action' setting
          of `options', and is `na.fail' if that is unset.  The
          ``factory-fresh'' default is `na.omit'.}

\item{offset}{Can be used to supply a model offset for use in fitting. Note
that this offset will always be completely ignored when predicting, unlike an offset 
included in \code{formula}: this conforms to the behaviour of
\code{lm} and \code{glm}.}

\item{control}{A list of fit control parameters to replace defaults returned by 
\code{\link{gam.control}}. Values not set assume default values. }

\item{method}{The smoothing parameter estimation method. \code{"GCV.Cp"} to use GCV for unknown scale parameter and
Mallows' Cp/UBRE/AIC for known scale. \code{"GACV.Cp"} is equivalent, but using GACV in place of GCV. \code{"REML"} 
for REML estimation, including of unknown scale, \code{"P-REML"} for REML estimation, but using a Pearson estimate 
of the scale. \code{"ML"} and \code{"P-ML"} are similar, but using maximum likelihood in place of REML. }

\item{optimizer}{An array specifying the numerical optimization method to use to optimize the smoothing 
parameter estimation criterion (given by \code{method}). \code{"perf"} for performance iteration. \code{"outer"} 
for the more stable direct approach. \code{"outer"} can use several alternative optimizers, specified in the 
second element of \code{optimizer}: \code{"newton"} (default), \code{"bfgs"}, \code{"optim"}, \code{"nlm"} 
and \code{"nlm.fd"} (the latter is based entirely on finite differenced derivatives and is very slow). }

\item{scale}{ If this is positive then it is taken as the known scale parameter. Negative signals that the 
scale parameter is unknown. 0 signals that the scale parameter is 1  for Poisson and binomial and unknown otherwise. 
Note that (RE)ML methods can only work with scale parameter 1 for the Poisson and binomial cases.    
} 

\item{select}{ If this is \code{TRUE} then \code{gam} can add an extra penalty to each term so 
that it can be penalized to zero.  This means that the smoothing parameter estimation that is 
part of fitting can completely remove terms from the model. If the corresponding 
smoothing parameter is estimated as zero then the extra penalty has no effect.
}

\item{knots}{this is an optional list containing user specified knot values to be used for basis construction. 
For most bases the user simply supplies the knots to be used, which must match up with the \code{k} value
supplied (note that the number of knots is not always just \code{k}). 
See \code{\link{tprs}} for what happens in the \code{"tp"/"ts"} case. 
Different terms can use different numbers of knots, unless they share a covariate.
}

\item{sp}{A vector of smoothing parameters can be provided here.
 Smoothing parameters must be supplied in the order that the smooth terms appear in the model 
formula. Negative elements indicate that the parameter should be estimated, and hence a mixture 
of fixed and estimated parameters is possible. If smooths share smoothing parameters then \code{length(sp)} 
must correspond to the number of underlying smoothing parameters.}

\item{min.sp}{Lower bounds can be supplied for the smoothing parameters. Note
that if this option is used then the smoothing parameters \code{full.sp}, in the 
returned object, will need to be added to what is supplied here to get the 
 smoothing parameters actually multiplying the penalties. \code{length(min.sp)} should 
always be the same as the total number of penalties (so it may be longer than \code{sp},
if smooths share smoothing parameters).}

\item{H}{A user supplied fixed quadratic penalty on the parameters of the 
GAM can be supplied, with this as its coefficient matrix. A common use of this term is 
to add a ridge penalty to the parameters of the GAM in circumstances in which the model
is close to un-identifiable on the scale of the linear predictor, but perfectly well
defined on the response scale.}

\item{gamma}{It is sometimes useful to inflate the model degrees of 
freedom in the GCV or UBRE/AIC score by a constant multiplier. This allows 
such a multiplier to be supplied. } 

\item{fit}{If this argument is \code{TRUE} then \code{gam} sets up the model and fits it, but if it is
\code{FALSE} then the model is set up and an object \code{G} containing what
would be required to fit is returned is returned. See argument \code{G}.}

\item{paraPen}{optional list specifying any penalties to be applied to parametric model terms. 
\code{\link{gam.models}} explains more.}

\item{G}{Usually \code{NULL}, but may contain the object returned by a previous call to \code{gam} with 
\code{fit=FALSE}, in which case all other arguments are ignored except for
\code{gamma}, \code{in.out}, \code{scale}, \code{control}, \code{method} \code{optimizer} and \code{fit}.}

\item{in.out}{optional list for initializing outer iteration. If supplied
then this must contain two elements: \code{sp} should be an array of
initialization values for all smoothing parameters (there must be a value for
all smoothing parameters, whether fixed or to be estimated, but those for
fixed s.p.s are not used); \code{scale} is the typical scale of the GCV/UBRE function,
for passing to the outer optimizer, or the the initial value of the scale parameter, if this is to
be estimated by RE/ML.}

\item{...}{further arguments for 
passing on e.g. to \code{gam.fit} (such as \code{mustart}). }

}

\value{ 
If \code{fit=FALSE} the function returns a list \code{G} of items needed to
fit a GAM, but doesn't actually fit it. 

Otherwise the function returns an object of class \code{"gam"} as described in \code{\link{gamObject}}.
}


\details{ 
A generalized additive model (GAM) is a generalized linear model (GLM) in which the linear 
predictor is given by a user specified sum of smooth functions of the covariates plus a 
conventional parametric component of the linear predictor. A simple example is:
\deqn{\log(E(y_i)) = f_1(x_{1i})+f_2(x_{2i})}{log(E(y_i))=f_1(x_1i)+f_2(x_2i)}
where the (independent) response variables \eqn{y_i \sim {\rm Poi }}{y_i~Poi}, and
\eqn{f_1}{f_1} and \eqn{f_2}{f_2} are smooth functions of covariates \eqn{x_1}{x_1} and 
\eqn{x_2}{x_2}. The log is an example of a link function. 

If absolutely any smooth functions were allowed in model fitting then maximum likelihood 
estimation of such models would invariably result in complex overfitting estimates of 
\eqn{f_1}{f_1}  and \eqn{f_2}{f_2}. For this reason the models are usually fit by 
penalized likelihood 
maximization, in which the model (negative log) likelihood is modified by the addition of 
a penalty for each smooth function, penalizing its `wiggliness'. To control the tradeoff 
between penalizing wiggliness and penalizing badness of fit each penalty is multiplied by 
an associated smoothing parameter: how to estimate these parameters, and 
how to practically represent the smooth functions are the main statistical questions 
introduced by moving from GLMs to GAMs. 

The \code{mgcv} implementation of \code{gam} represents the smooth functions using 
penalized regression splines, and by default uses basis functions for these splines that 
are designed to be optimal, given the number basis functions used. The smooth terms can be 
functions of any number of covariates and the user has some control over how smoothness of 
the functions is measured. 

\code{gam} in \code{mgcv} solves the smoothing parameter estimation problem by using the 
Generalized Cross Validation (GCV) criterion
\deqn{n D / (n - DoF)^2}{n D/(n - DoF)^2}
 or an Un-Biased Risk Estimator (UBRE )criterion
\deqn{D/n + 2 s DoF / n - s }{D/n + 2 s DoF / n -s} 
where \eqn{D}{D} is the deviance, \eqn{n}{n} the number of data, \eqn{s}{s}
the scale parameter and 
\eqn{DoF}{DoF} the effective degrees of freedom of the model. Notice that UBRE is effectively
just AIC rescaled, but is only used when \eqn{s}{s} is known. 

Alternatives are GACV, or a Laplace approximation to REML. There
is some evidence that the latter may actually be the most effective choice. 

Smoothing parameters are chosen to 
minimize the GCV, UBRE/AIC, GACV or REML scores for the model, and the main computational challenge solved 
by the \code{mgcv} package is to do this efficiently and reliably. Various
alternative numerical methods are provided which can be set by argument \code{optimizer}.


Broadly \code{gam} works by first constructing basis functions and one or more quadratic penalty 
coefficient matrices for each smooth term in the model formula, obtaining a model matrix for 
the strictly parametric part of the model formula, and combining these to obtain a 
complete model matrix (/design matrix) and a set of penalty matrices for the smooth terms. 
Some linear identifiability constraints are also obtained at this point. The model is 
fit using \code{\link{gam.fit}}, a modification of \code{\link{glm.fit}}. The GAM 
penalized likelihood maximization problem is solved by Penalized Iteratively 
Reweighted  Least Squares (P-IRLS) (see e.g. Wood 2000). 
Smoothing parameter selection is integrated in one of two ways. (i)
`Performance iteration' uses the fact that at each P-IRLS iteration a penalized 
weighted least squares problem is solved, and the smoothing parameters of that problem can 
estimated by GCV or UBRE. Eventually, in most cases, both model parameter estimates and smoothing 
parameter estimates converge. (ii) Alternatively the P-IRLS scheme is iterated to
convergence for each trial set of smoothing parameters, and GCV, UBRE or REML scores
are only evaluated on convergence - optimization is then `outer' to the P-IRLS
loop: in this case the P-IRLS iteration has to be differentiated, to
facilitate optimization, and \code{\link{gam.fit3}} is used in place of
\code{gam.fit}. The default is the second method, outer iteration.


Several alternative basis-penalty types  are built in for representing model
smooths, but alternatives can easily be added (see \code{\link{smooth.terms}} 
for an overview and \code{\link{smooth.construct}} for how to add smooth classes). The choice of the basis dimension 
(\code{k} in the \code{s}, \code{te}, \code{ti} and \code{t2} terms) is something that should be considered carefully 
(the exact value is not critical, but it is important not to make it restrictively small, nor very large and 
computationally costly). The basis should 
be chosen to be larger than is believed to be necessary to approximate the smooth function concerned. 
The effective degrees of freedom for the smooth will then be controlled by the smoothing penalty on 
the term, and (usually) selected automatically (with an upper limit set by
\code{k-1} or occasionally \code{k}). Of course 
the \code{k} should not be made too large, or computation will be slow (or in
extreme cases there will be more 
coefficients to estimate than there are data).

Note that \code{gam} assumes a very inclusive definition of what counts as a GAM: 
basically any penalized GLM can be used: to this end \code{gam} allows the non smooth model 
components to be penalized via argument \code{paraPen} and allows the linear predictor to depend on 
general linear functionals of smooths, via the summation convention mechanism described in 
\code{\link{linear.functional.terms}}.
     
Details of the default underlying fitting methods are given in Wood (2011
and 2004). Some alternative methods are discussed in Wood (2000 and 2006). 

\code{gam()} is not a clone of Trevor Hastie's oroginal (as supplied in S-PLUS or package \link[gam]{gam}) The major
differences are (i) that by default estimation of the
degree of smoothness of model terms is part of model fitting, (ii) a
Bayesian approach to variance estimation is employed that makes for easier
confidence interval calculation (with good coverage probabilities), (iii) that the model
can depend on any (bounded) linear functional of smooth terms, (iv) the parametric part of the model can be penalized, 
(v) simple random effects can be incorporated, and 
(vi) the facilities for incorporating smooths of more than one variable are
different: specifically there are no \code{lo} smooths, but instead (a) \code{\link{s}}
terms can have more than one argument, implying an isotropic smooth and (b) \code{\link{te}} or \code{\link{t2}} smooths are
provided as an effective means for modelling smooth interactions of any
number of variables via scale invariant tensor product smooths. Splines on the sphere, Duchon splines 
and Gaussian Markov Random Fields are also available. See \link[gam]{gam} 
from package \code{gam}, for GAMs via the original Hastie and Tibshirani approach.
}


\references{

Key References on this implementation:

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36

Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for
generalized additive models. J. Amer. Statist. Ass. 99:673-686. [Default
method for additive case by GCV (but no longer for generalized)]

Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

Wood, S.N. (2006a) Low rank scale invariant tensor product smooths for
generalized additive mixed models. Biometrics 62(4):1025-1036

Wood S.N. (2006b) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.

Wood S.N., F. Scheipl and J.J. Faraway (2012) Straightforward intermediate rank tensor product smoothing
in mixed models. Statistical Computing.

Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for Generalized Additive
Model Components. Scandinavian Journal of Statistics, 39(1), 53-74.


Key Reference on GAMs and related models:

Hastie (1993) in Chambers and Hastie (1993) Statistical Models in S. Chapman
and Hall.

Hastie and Tibshirani (1990) Generalized Additive Models. Chapman and Hall.

Wahba (1990) Spline Models of Observational Data. SIAM 

Wood, S.N. (2000)  Modelling and Smoothing Parameter Estimation
with Multiple Quadratic Penalties. J.R.Statist.Soc.B 62(2):413-428 [The original
mgcv paper, but no longer the default methods.]

Background References:

Green and Silverman (1994) Nonparametric Regression and Generalized  Linear Models. Chapman and Hall.
  
Gu and Wahba (1991) Minimizing GCV/GML scores with multiple smoothing parameters via
the Newton method. SIAM J. Sci. Statist. Comput. 12:383-398

Gu (2002) Smoothing Spline ANOVA Models, Springer.

McCullagh and Nelder (1989) Generalized Linear Models 2nd ed. Chapman & Hall.

O'Sullivan, Yandall and Raynor (1986) Automatic smoothing of regression
functions in generalized linear models.
J. Am. Statist.Ass. 81:96-103 

Wood (2001) mgcv:GAMs and Generalized Ridge Regression for R. R News 1(2):20-25
   
Wood and Augustin (2002) GAMs with integrated model selection using penalized regression splines and applications 
to environmental modelling. Ecological Modelling 157:157-177

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}

Front end design inspired by the S function of the same name based on the work
of Hastie and Tibshirani (1990). Underlying methods owe much to the work of
Wahba (e.g. 1990) and Gu (e.g. 2002).

}

\section{WARNINGS }{

The default basis dimensions used for smooth terms are essentially arbitrary, and 
it should be checked that they are not too small. See \code{\link{choose.k}} and
\code{\link{gam.check}}. 

You must have more unique combinations of covariates than the model has total
parameters. (Total parameters is sum of basis dimensions plus sum of non-spline 
terms less the number of spline terms). 

Automatic smoothing parameter selection is not likely to work well when 
fitting models to very few response data.

For data with many  zeroes clustered together in the covariate space it is quite easy to set up 
GAMs which suffer from identifiability problems, particularly when using Poisson or binomial
families. The problem is that with e.g. log or logit links, mean value zero corresponds to
an infinite range on the linear predictor scale.
} 

\seealso{\code{\link{mgcv-package}}, \code{\link{gamObject}}, \code{\link{gam.models}}, \code{\link{smooth.terms}},
\code{\link{linear.functional.terms}}, \code{\link{s}},
\code{\link{te}} \code{\link{predict.gam}},
\code{\link{plot.gam}}, \code{\link{summary.gam}}, \code{\link{gam.side}},
\code{\link{gam.selection}}, \code{\link{gam.control}}
\code{\link{gam.check}}, \code{\link{linear.functional.terms}} \code{\link{negbin}}, \code{\link{magic}},\code{\link{vis.gam}}
 }

\examples{
library(mgcv)
set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
summary(b)
plot(b,pages=1,residuals=TRUE)  ## show partial residuals
plot(b,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(b)

## same fit in two parts .....
G <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),fit=FALSE,data=dat)
b <- gam(G=G)
print(b)

## change the smoothness selection method to REML
b0 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat,method="REML")
plot(b0,pages=1,scheme=1)

## Would a smooth interaction of x0 and x1 be better?
## Use tensor product smooth of x0 and x1, basis 
## dimension 49 (see ?te for details, also ?t2).
bt <- gam(y~te(x0,x1,k=7)+s(x2)+s(x3),data=dat,
          method="REML")
plot(bt,pages=1) 
plot(bt,pages=1,scheme=2) ## alternative visualization
AIC(b0,bt) ## interaction worse than additive

## Alternative: test for interaction with a smooth ANOVA 
## decomposition (this time between x2 and x1)
bt <- gam(y~s(x0)+s(x1)+s(x2)+s(x3)+ti(x1,x2,k=6),
            data=dat,method="REML")
summary(bt)

## If it is believed that x0 and x1 are naturally on 
## the same scale, and should be treated isotropically 
## then could try...
bs <- gam(y~s(x0,x1,k=50)+s(x2)+s(x3),data=dat,
          method="REML")
plot(bs,pages=1)
AIC(b0,bt,bs) ## additive still better. 

## Now do automatic terms selection as well
b1 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat,
       method="REML",select=TRUE)
plot(b1,pages=1)


## set the smoothing parameter for the first term, estimate rest ...
bp <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1),data=dat)
plot(bp,pages=1,scheme=1)
## alternatively...
bp <- gam(y~s(x0,sp=.01)+s(x1)+s(x2)+s(x3),data=dat)


# set lower bounds on smoothing parameters ....
bp<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),
        min.sp=c(0.001,0.01,0,10),data=dat) 
print(b);print(bp)

# same with REML
bp<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),
        min.sp=c(0.1,0.1,0,10),data=dat,method="REML") 
print(b0);print(bp)


## now a GAM with 3df regression spline term & 2 penalized terms

b0<-gam(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15),data=dat)
plot(b0,pages=1)

## now simulate poisson data...
dat <- gamSim(1,n=4000,dist="poisson",scale=.1)

## use "cr" basis to save time, with 4000 data...
b2<-gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+
        s(x3,bs="cr"),family=poisson,data=dat,method="REML")
plot(b2,pages=1)

## drop x3, but initialize sp's from previous fit, to 
## save more time...

b2a<-gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr"),
         family=poisson,data=dat,method="REML",
         in.out=list(sp=b2$sp[1:3],scale=1))
par(mfrow=c(2,2))
plot(b2a)

par(mfrow=c(1,1))
## similar example using performance iteration
dat <- gamSim(1,n=400,dist="poisson",scale=.25)

b3<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
        data=dat,optimizer="perf")
plot(b3,pages=1)

## repeat using GACV as in Wood 2008...

b4<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
        data=dat,method="GACV.Cp",scale=-1)
plot(b4,pages=1)

## repeat using REML as in Wood 2011...

b5<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
        data=dat,method="REML")
plot(b5,pages=1)

 
## a binary example (see later for large dataset version)...

dat <- gamSim(1,n=400,dist="binary",scale=.33)

lr.fit <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
              data=dat,method="REML")

## plot model components with truth overlaid in red
op <- par(mfrow=c(2,2))
fn <- c("f0","f1","f2","f3");xn <- c("x0","x1","x2","x3")
for (k in 1:4) {
  plot(lr.fit,residuals=TRUE,select=k)
  ff <- dat[[fn[k]]];xx <- dat[[xn[k]]]
  ind <- sort.int(xx,index.return=TRUE)$ix
  lines(xx[ind],(ff-mean(ff))[ind]*.33,col=2)
}
par(op)
anova(lr.fit)
lr.fit1 <- gam(y~s(x0)+s(x1)+s(x2),family=binomial,
               data=dat,method="REML")
lr.fit2 <- gam(y~s(x1)+s(x2),family=binomial,
               data=dat,method="REML")
AIC(lr.fit,lr.fit1,lr.fit2)

## A Gamma example, by modify `gamSim' output...
 
dat <- gamSim(1,n=400,dist="normal",scale=1)
dat$f <- dat$f/4 ## true linear predictor 
Ey <- exp(dat$f);scale <- .5 ## mean and GLM scale parameter
## Note that `shape' and `scale' in `rgamma' are almost
## opposite terminology to that used with GLM/GAM...
dat$y <- rgamma(Ey*0,shape=1/scale,scale=Ey*scale)
bg <- gam(y~ s(x0)+ s(x1)+s(x2)+s(x3),family=Gamma(link=log),
          data=dat,method="REML")
plot(bg,pages=1,scheme=1)

## For inverse Gaussian, see ?rig

## now a 2D smoothing example...

eg <- gamSim(2,n=500,scale=.1)
attach(eg)

op <- par(mfrow=c(2,2),mar=c(4,4,1,1))

contour(truth$x,truth$z,truth$f) ## contour truth
b4 <- gam(y~s(x,z),data=data) ## fit model
fit1 <- matrix(predict.gam(b4,pr,se=FALSE),40,40)
contour(truth$x,truth$z,fit1)   ## contour fit
persp(truth$x,truth$z,truth$f)    ## persp truth
vis.gam(b4)                     ## persp fit
detach(eg)
par(op)

##################################################
## largish dataset example with user defined knots
##################################################

par(mfrow=c(2,2))
eg <- gamSim(2,n=10000,scale=.5)
attach(eg)

ind<-sample(1:10000,1000,replace=FALSE)
b5<-gam(y~s(x,z,k=50),data=data,
        knots=list(x=data$x[ind],z=data$z[ind]))
## various visualizations
vis.gam(b5,theta=30,phi=30)
plot(b5)
plot(b5,scheme=1,theta=50,phi=20)
plot(b5,scheme=2)

par(mfrow=c(1,1))
## and a pure "knot based" spline of the same data
b6<-gam(y~s(x,z,k=100),data=data,knots=list(x= rep((1:10-0.5)/10,10),
        z=rep((1:10-0.5)/10,rep(10,10))))
vis.gam(b6,color="heat",theta=30,phi=30)

## varying the default large dataset behaviour via `xt'
b7 <- gam(y~s(x,z,k=50,xt=list(max.knots=1000,seed=2)),data=data)
vis.gam(b7,theta=30,phi=30)
detach(eg)

################################################################
## Approximate large dataset logistic regression for rare events
## based on subsampling the zeroes, and adding an offset to
## approximately allow for this.
## Doing the same thing, but upweighting the sampled zeroes
## leads to problems with smoothness selection, and CIs.
################################################################
n <- 100000  ## simulate n data 
dat <- gamSim(1,n=n,dist="binary",scale=.33)
p <- binomial()$linkinv(dat$f-6) ## make 1's rare
dat$y <- rbinom(p,1,p)      ## re-simulate rare response

## Now sample all the 1's but only proportion S of the 0's
S <- 0.02                   ## sampling fraction of zeroes
dat <- dat[dat$y==1 | runif(n) < S,] ## sampling

## Create offset based on total sampling fraction
dat$s <- rep(log(nrow(dat)/n),nrow(dat))

lr.fit <- gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+s(x3,bs="cr")+
              offset(s),family=binomial,data=dat,method="REML")

## plot model components with truth overlaid in red
op <- par(mfrow=c(2,2))
fn <- c("f0","f1","f2","f3");xn <- c("x0","x1","x2","x3")
for (k in 1:4) {
       plot(lr.fit,select=k,scale=0)
       ff <- dat[[fn[k]]];xx <- dat[[xn[k]]]
       ind <- sort.int(xx,index.return=TRUE)$ix
       lines(xx[ind],(ff-mean(ff))[ind]*.33,col=2)
}
par(op)
rm(dat)

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..
\concept{Varying coefficient model}
\concept{Functional linear model}
\concept{Penalized GLM}
\concept{Generalized Additive Model}
\concept{Penalized regression}
\concept{Spline smoothing}
\concept{Penalized regression spline}
\concept{Generalized Cross Validation}
\concept{Smoothing parameter selection}
\concept{tensor product smoothing}
\concept{thin plate spline}
\concept{P-spline}
\concept{Generalized ridge regression}






