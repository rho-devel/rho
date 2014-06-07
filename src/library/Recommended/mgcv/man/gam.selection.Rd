\name{gam.selection}
\alias{gam.selection}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generalized Additive Model Selection}
\description{ This page is intended to provide some more information on
how to select GAMs. In particular, it gives a brief overview of smoothness 
selection, and then discusses how this can be extended to select inclusion/exclusion 
of terms. Hypothesis testing approaches to the latter problem are also discussed.
}


\section{Smoothness selection criteria}{
Given a model structure specified by a gam model formula,
\code{gam()} attempts to find the appropriate smoothness for each applicable model 
term using prediction error criteria or likelihood based methods. The prediction error 
criteria used are Generalized (Approximate) Cross Validation (GCV or GACV) when the scale 
parameter is unknown or an Un-Biased Risk Estimator (UBRE) when it is known. UBRE is essentially
scaled AIC (Generalized case) or Mallows' Cp (additive model case). 
GCV and UBRE are covered in Craven and Wahba (1979) and Wahba (1990). 
Alternatively REML of maximum likelihood (ML) may be used for smoothness 
selection, by viewing the smooth components as random effects (in this case the variance component for each 
smooth random effect will be given by the scale parameter divided by the smoothing parameter --- for 
smooths with multiple penalties, there will be multiple variance components). 
The \code{method} argument to \code{\link{gam}} selects the smoothness selection criterion.


Automatic smoothness selection is unlikely to be successful with few data, particularly with
multiple terms to be selected. In addition GCV and UBRE/AIC score can occasionally
display local minima that can trap the minimisation algorithms. GCV/UBRE/AIC
scores become constant with changing smoothing parameters at very low or very
high smoothing parameters, and on occasion these `flat' regions can be
separated from regions of lower score by a small `lip'. This seems to be the
most common form of local minimum, but is usually avoidable by avoiding
extreme smoothing parameters as starting values in optimization, and by
avoiding big jumps in smoothing parameters while optimizing. Never the less, if you are
suspicious of smoothing parameter estimates, try changing fit method (see
\code{\link{gam}} arguments \code{method} and \code{optimizer}) and see if the estimates change, or try changing
some or all of the smoothing parameters `manually' (argument \code{sp} of \code{\link{gam}},
or \code{sp} arguments to \code{\link{s}} or \code{\link{te}}). 

REML and ML are less prone to local minima than the other criteria, and may therefore be preferable.
} 

\section{Automatic term selection}{

Unmodified smoothness selection by GCV, AIC, REML etc. will not usually remove 
a smooth from a model. This is because most smoothing penalties view some space
of (non-zero) functions as `completely smooth' and once a term is penalized heavily 
enough that it is in this space, further penalization does not change it. 

However it is straightforward to modify smooths so that under heavy penalization they 
are penalized to the zero function and thereby `selected out' of the model. There are 
two approaches.

The first approach is to modify the smoothing penalty with an additional shrinkage term. 
Smooth classes\code{cs.smooth} and \code{tprs.smooth} (specified by \code{"cs"} and
\code{"ts"} respectively) have smoothness penalties which include a small
shrinkage component, so that for large enough smoothing parameters the smooth 
becomes identically zero. This allows automatic smoothing parameter selection
methods to effectively remove the term from the model altogether. The
shrinkage component of the penalty is set at a level that usually makes negligable
contribution to the penalization of the model, only becoming effective when
the term is effectively `completely smooth' according to the conventional penalty.

The second approach leaves the original smoothing penalty unchanged, but constructs an 
additional penalty for each smooth, which penalizes only functions in the null space of the 
original penalty (the `completely smooth' functions). Hence, if all the smoothing parameters 
for a term tend to infinity, the term will be selected out of the model. This latter approach 
is more expensive computationally, but has the advantage that it can be applied automatically 
to any smooth term. The \code{select} argument to \code{\link{gam}} turns on this method.

In fact, as implemented, both approaches operate by eigen-decomposiong the original penalty matrix. 
A new penalty is created on the null space: it is the matrix with the same eigenvectors as the
original penalty, but with the originally postive egienvalues set to zero, and the originally 
zero eigenvalues set to something positive. The first approach just addes a multiple of this 
penalty to the original penalty, where the multiple is chosen so that the new penalty can not 
dominate the original. The second approach treats the new penalty as an extra penalty, with its 
own smoothing parameter. 


Of course, as with all model selection methods, some care must be take to ensure that the 
automatic selection is sensible, and a decision about the effective degrees of freedom at 
which to declare a term `negligible' has to be made. 

}

\section{Interactive term selection}{
In general the most logically consistent method to use for deciding which
terms to include in the model is to compare GCV/UBRE/ML scores for models with 
and without the term (REML scores should not be used to compare models with 
different fixed effects structures). When UBRE is the smoothness selection method this will
give the same result as comparing by \code{\link{AIC}} (the AIC in this case
uses the model EDF in place of the usual model DF). Similarly, comparison via
GCV score and via AIC seldom yields different answers. Note that the negative
binomial with estimated \code{theta} parameter is a special case: the GCV
score is not informative, because of the \code{theta} estimation
scheme used. More generally the score for the model with a smooth 
term can be compared to the score for the model with 
the smooth term replaced by appropriate parametric terms. Candidates for 
replacement by parametric terms are  smooth terms with estimated 
degrees of freedom close to their minimum possible.


Candidates for removal can also be identified by reference to the 
approximate p-values provided by \code{summary.gam}, and by looking at the
extent to which the confidence band for an estimated term includes the zero 
function. It is perfectly possible to perform backwards selection using p-values
in the usual way: that is by sequentially dropping the single term with the highest 
non-significant p-value from the model and re-fitting, until all terms are 
significant. This suffers from the same problems as stepwise procedures for any
GLM/LM, with the additional caveat that the p-values are only approximate. If adopting 
this approach, it is probably best to use ML smoothness selection.

Note that GCV and UBRE are not appropriate for comparing models using different families: 
in that case AIC should be used.
}

\section{Caveats/platitudes}{
  Formal model selection methods are only appropriate for selecting between reasonable models.
If formal model selection is attempted starting from a model that simply doesn't fit the data, 
then it is unlikely to provide meaningful results.  

The more thought is given to appropriate model structure up front, the more successful model
selection is likely to be. Simply starting with a hugely flexible model with `everything in' 
and hoping that automatic selection will find the right structure is not often successful.

}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{

Marra, G. and S.N. Wood (2011) Practical variable selection for generalized additive models.
Computational Statistics and Data Analysis 55,2372-2387.

Craven and Wahba (1979) Smoothing Noisy Data with Spline Functions. Numer. Math. 31:377-403

Venables and Ripley (1999) Modern Applied Statistics with S-PLUS

Wahba (1990) Spline Models of Observational Data. SIAM.

Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

Wood, S.N. (2008) Fast stable direct fitting and smoothness selection for
generalized additive models. J.R.Statist. Soc. B 70(3):495-518

Wood, S.N. (2011) Fast stable restricted maximum likelihood 
and marginal likelihood estimation of semiparametric generalized linear 
models. Journal of the Royal Statistical Society (B) 73(1):3-36



\url{http://www.maths.bath.ac.uk/~sw283/}
}


\seealso{\code{\link{gam}}, \code{\link{step.gam}}}

\examples{
## an example of automatic model selection via null space penalization
library(mgcv)
set.seed(3);n<-200
dat <- gamSim(1,n=n,scale=.15,dist="poisson") ## simulate data
dat$x4 <- runif(n, 0, 1);dat$x5 <- runif(n, 0, 1) ## spurious

b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3)+s(x4)+s(x5),data=dat,
         family=poisson,select=TRUE,method="REML")
summary(b)
plot(b,pages=1)
}

\keyword{models} \keyword{regression}%-- one or more ..


