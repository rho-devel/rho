\name{mgcv-FAQ}
\alias{mgcv-FAQ}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Frequently Asked Questions for package mgcv}
\description{ This page provides answers to some of the questions that get asked most often about mgcv}


\section{FAQ list}{
\enumerate{
\item \bold{How can I compare gamm models?} In the identity link normal errors case, then AIC and hypotheis 
testing based methods are fine. Otherwise it is best to work out a strategy based on the \code{\link{summary.gam}} 
Alternatively, simple random effects can be fitted with \code{\link{gam}}, which makes comparison straightforward. 
Package \code{gamm4} is an alternative, which allows AIC type model selection for generalized models.

\item \bold{How do I get the equation of an estimated smooth?} This slightly misses the point 
of semi-parametric modelling: the idea is that we estimate the form of the function from data
without assuming that it has a particular simple functional form. Of course for practical computation
the functions do have underlying mathematical representations, but they are not very helpful, when written 
down. If you do need the functional forms then see chapter 4 of Wood (2006). However for most purposes it 
is better to use \code{\link{predict.gam}} to evaluate the function for whatever argument values you need. 
If derivatives are required then the simplest approach is to use finite differencing (which also allows 
SEs etc to be calculated).

\item \bold{Some of my smooths are estimated to be straight lines and their confidence intervals vanish at some point in the middle. What is wrong?} Nothing. Smooths are subject to sum-to-zero identifiability constraints. If a smooth is estimated to be a straight line then it consequently has one degree of freedom, and there is no choice about where it passes through zero --- so the CI must vanish at that point. 

\item \bold{Some code from Wood (2006) causes an error: why?} The book was written using mgcv version 1.3. To allow for 
REML estimation of smoothing parameters in versions 1.5, some changes had to be made to the syntax. In particular the 
function \code{gam.method} no longer exists. The smoothness selection method (GCV, REML etc) is now controlled by the
\code{method} argument to \code{gam} while the optimizer is selected using the \code{optimizer} argument. 
See \code{\link{gam}} and \url{http://www.maths.bath.ac.uk/~sw283/igam/index.html} for details.

\item \bold{Why is a model object saved under a previous mgcv version not usable with the current mgcv version?} 
I'm sorry about this issue, I know it's really annoying. Here's my defence. 
Each mgcv version is run through an extensive test suite before release, to ensure that it gives the same results 
as before, unless there are good statistical reasons why not (e.g. improvements to p-value approximation, fixing 
of an error). However it is sometimes necessary to modify the internal structure of model objects in a way that makes an old style object unusable with a newer version. For example, bug fixes or new R features sometimes require changes in the way 
that things are computed which in turn require modification of the object structure. Similarly improvements, such as the 
ability to compute smoothing parameters by RE/ML require object level changes. The only fix to this problem is to access the old object using the original mgcv version (available on CRAN), or to recompute the fit using the current mgcv version.

\item \bold{When using \code{gamm} or \code{gamm4}, the reported AIC is different for the \code{gam} object and 
the \code{lme} or \code{lmer} object. Why is this?} There are several reasons for this. The most important is that the 
models being used are actually different in the two representations. When treating the GAM as a mixed model, you are 
implicitly assuming that if you gathered a replicate dataset, the smooths in your model would look completely 
different to the smooths from the original model, except for having the same degree of smoothness. Technically you would expect 
the smooths to be drawn afresh from their distribution under the random effects model. When viewing the gam from the 
usual penalized regression perspective, you would expect smooths to look broadly similar under replication of the data. 
i.e. you are really using Bayesian model for the smooths, rather than a random effects model (it's just that the frequentist random 
effects and Bayesian computations happen to coincide for computing the estimates). As a result of the different assumptions 
about the data generating process, AIC model comparisons can give rather different answers depending on the model adopted. 
Which you use should depend on which model you really think is appropriate. In addition the computations of the AICs are 
different. The mixed model AIC uses the marginal liklihood and the corresponding number of model parameters. The gam model 
uses the penalized likelihood and the effective degrees of freedom. 

\item \bold{What does 'mgcv' stand for?} '\bold{M}ixed \bold{G}AM \bold{C}omputation \bold{V}ehicle', is my current best effort (let me know if you can do better). Originally it stood for `Multiple GCV', which has long since ceased to be usefully descriptive, (and I can't really change 'mgcv' now without causing disruption).


\item \bold{My new method is failing to beat mgcv, what can I do?} If speed is the problem, then make sure that you use the 
slowest basis possible (\code{"tp"}) with a large sample size, and experiment with different optimizers to find one that is 
slow for your problem. For prediction error/MSE, then leaving the smoothing basis dimensions at their 
arbitrary defaults, when these are inappropriate for the problem setting, is a good way of reducing performance. 
Similarly, using p-splines in place of derivative penalty based splines will often shave a little more 
from the performance here. Unlike REML/ML, prediction error based smoothness selection criteria such as Mallows Cp and GCV 
often produce a small proportion of severe overfits, so careful choise of smoothness selection method can help further. 
In particular GCV etc. usually result in worse confidence interval and p-value performance than ML or REML. If all this 
fails, try using a really odd simulation setup for which mgcv is clearly not suited: for example poor performance is almost 
guaranteed for small noisy datasets with large numbers of predictors.

}

}
\references{

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\keyword{models} \keyword{regression}%-- one or more ..


