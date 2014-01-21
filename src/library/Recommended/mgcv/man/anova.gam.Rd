\name{anova.gam}
\alias{anova.gam}
\alias{print.anova.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Approximate hypothesis tests related to  GAM fits}
\description{ Performs hypothesis tests relating to one or more fitted
\code{gam} objects. For a single fitted \code{gam} object, Wald tests of
the significance of each parametric and smooth term are performed, so interpretation 
is analogous to \code{\link{drop1}} rather than \code{anova.lm} (i.e. it's like type III ANOVA, 
rather than a sequential type I ANOVA). Otherwise
the fitted models are compared using an analysis of deviance table: this latter approach 
should not be use to test the significance of terms which can be penalized 
to zero. See details.
}
\usage{
\method{anova}{gam}(object, ..., dispersion = NULL, test = NULL,
                    freq = FALSE,p.type=0)
\method{print}{anova.gam}(x, digits = max(3, getOption("digits") - 3),...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{object,...}{ fitted model objects of class \code{gam} as produced by \code{gam()}.}
\item{x}{an \code{anova.gam} object produced by a single model call to \code{anova.gam()}.} 
\item{dispersion}{ a value for the dispersion parameter: not normally used.}
\item{test}{what sort of test to perform for a multi-model call. One of
\code{"Chisq"}, \code{"F"} or \code{"Cp"}. }
\item{freq}{whether to use frequentist or Bayesian approximations for parametric term 
p-values. See \code{\link{summary.gam}} for details.}
\item{p.type}{selects exact test statistic to use for single smooth term p-values. See
 \code{\link{summary.gam}} for details.}
\item{digits}{number of digits to use when printing output.}
}
\details{ If more than one fitted model is provided than \code{anova.glm} is
used, with the difference in model degrees of freedom being taken as the difference 
in effective degress of freedom. The p-values resulting from this are only approximate, 
and must be used with care. The approximation is most accurate when the comparison 
relates to unpenalized terms, or smoothers with a null space of dimension greater than zero.
(Basically we require that the difference terms could be well approximated by unpenalized 
terms with degrees of freedom approximately the effective degrees of freedom). In simulations the 
p-values are usually slightly too low. For terms with a zero-dimensional null space 
(i.e. those which can be penalized to zero) the approximation is often very poor, and significance 
can be greatly overstated: i.e. p-values are often substantially too low. This case applies to random effect terms. 

Note also that in the multi-model call to \code{anova.gam}, it is quite possible for a model with more terms to end up with lower effective degrees of freedom, but better fit, than the notionally null model with fewer terms. In such cases it is very rare that it makes sense to perform any sort of test, since there is then no basis on which to accept the notional null model. 

If only one model is provided then the significance of each model term
is assessed using Wald tests, conditional on the smoothing parameter estimates: see \code{\link{summary.gam}} 
and Wood (in press) for details. The p-values  provided here are better justified than in the multi model case, and have close to the 
correct distribution under the null, unless smoothing parameters are poorly identified. ML or REML smoothing parameter selection leads to 
the best results in simulations as they tend to avoid occasional severe undersmoothing. In replication of the full simulation study of Scheipl et al. 
(2008) the tests give almost indistinguishable power to the method recommended there, but slightly too low p-values under the null in their 
section 3.1.8 test for a smooth interaction (the Scheipl et al. recommendation is not used directly, because it only applies in the Gaussian case,
and requires model refits, but it is available in package \code{RLRsim}). 

In the single model case \code{print.anova.gam} is used as the printing method. 

By default the p-values for parametric model terms are also based on Wald tests using the Bayesian 
covariance matrix for the coefficients. This is appropriate when there are "re" terms present, and is 
otherwise rather similar to the results using the frequentist covariance matrix (\code{freq=TRUE}), since 
the parametric terms themselves are usually unpenalized. Default P-values for parameteric terms that are 
penalized using the \code{paraPen} argument will not be good.

}

\value{In the multi-model case \code{anova.gam} produces output identical to
\code{\link{anova.glm}}, which it in fact uses.

In the single model case an object of class \code{anova.gam} is produced,
which is in fact an object returned from \code{\link{summary.gam}}.

\code{print.anova.gam} simply produces tabulated output.

}

\references{

Scheipl, F., Greven, S. and Kuchenhoff, H. (2008) Size and power of tests for a zero random effect variance or polynomial 
regression in additive and linear mixed models. Comp. Statist. Data Anal. 52, 3283-3299

Wood, S.N. (2013) On p-values for smooth components of an extended generalized additive model. Biometrika 100:221-228
}

\author{ Simon N. Wood \email{simon.wood@r-project.org} with substantial
improvements by Henric Nilsson.}

\section{WARNING}{ If models 'a' and 'b' differ only in terms with no un-penalized components then 
p values from anova(a,b) are unreliable, and usually much too low.

Default P-values will usually be wrong for parametric terms penalized using `paraPen': use freq=TRUE
to obtain better p-values when the penalties are full rank and represent conventional random effects.

For a single model, interpretation is similar to drop1, not anova.lm.
} 

\seealso{  \code{\link{gam}}, \code{\link{predict.gam}},
\code{\link{gam.check}}, \code{\link{summary.gam}} }

\examples{
library(mgcv)
set.seed(0)
dat <- gamSim(5,n=200,scale=2)

b<-gam(y ~ x0 + s(x1) + s(x2) + s(x3),data=dat)
anova(b)
b1<-gam(y ~ x0 + s(x1) + s(x2),data=dat)
anova(b,b1,test="F")
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






