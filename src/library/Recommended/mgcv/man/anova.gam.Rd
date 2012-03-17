\name{anova.gam}
\alias{anova.gam}
\alias{print.anova.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Approximate hypothesis tests related to  GAM fits}
\description{ Performs hypothesis tests relating to one or more fitted
\code{gam} objects. For a single fitted \code{gam} object, Wald tests of
the significance of each parametric and smooth term are performed. Otherwise
the fitted models are compared using an analysis of deviance table. The tests
are usually approximate, unless the models are un-penalized. Simulation evidence 
suggests that best p-value performance results from using ML estimated smoothing parameters. 

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
\item{freq}{whether to use frequentist or Bayesian approximations for single smooth term 
p-values. See \code{\link{summary.gam}} for details.}
\item{p.type}{selects exact test statistic to use for single smooth term p-values. See
 \code{\link{summary.gam}} for details.}
\item{digits}{number of digits to use when printing output.}
}
\details{ If more than one fitted model is provided than \code{anova.glm} is
used. If only one model is provided then the significance of each model term
is assessed using Wald tests: see \code{\link{summary.gam}} for details of the
actual computations.
In the latter case \code{print.anova.gam} is used as the
printing method.  Note that the p-values for smooth terms are approximate only:
simulation evidence suggests that they work best with ML smoothness selection 
(REML coming a close second). 

}

\value{In the multi-model case \code{anova.gam} produces output identical to
\code{\link{anova.glm}}, which it in fact uses.

In the single model case an object of class \code{anova.gam} is produced,
which is in fact an object returned from \code{\link{summary.gam}}.

\code{print.anova.gam} simply produces tabulated output.

}


\author{ Simon N. Wood \email{simon.wood@r-project.org} with substantial
improvements by Henric Nilsson.}

\section{WARNING}{ P-values for smooth terms are only approximate.
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






