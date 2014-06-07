% file MASS/man/stepAIC.Rd
% copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
%
\name{stepAIC}
\alias{stepAIC}
\alias{extractAIC.gls}
\alias{terms.gls}
\alias{extractAIC.lme}
\alias{terms.lme}
\title{
  Choose a model by AIC in a Stepwise Algorithm
}
\description{
  Performs stepwise model selection by AIC.
}
\usage{
stepAIC(object, scope, scale = 0,
        direction = c("both", "backward", "forward"),
        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
        k = 2, \dots)
}
\arguments{
  \item{object}{
    an object representing a model of an appropriate class.
    This is used as the initial model in the stepwise search.
  }
  \item{scope}{
    defines the range of models examined in the stepwise search.
    This should be either a single formula, or a list containing
    components \code{upper} and \code{lower}, both formulae.  See the
    details for how to specify the formulae and how they are used.
  }
  \item{scale}{
    used in the definition of the AIC statistic for selecting the models,
    currently only for \code{\link{lm}} and \code{\link{aov}} models
    (see \code{\link{extractAIC}} for details).
  }
  \item{direction}{
    the mode of stepwise search, can be one of \code{"both"},
    \code{"backward"}, or \code{"forward"}, with a default of \code{"both"}.
    If the \code{scope} argument is missing the default for
    \code{direction} is \code{"backward"}.
  }
  \item{trace}{
    if positive, information is printed during the running of
    \code{stepAIC}.
    Larger values may give more information on the fitting process.
  }
  \item{keep}{
    a filter function whose input is a fitted model object and the
    associated \code{AIC} statistic, and whose output is arbitrary.
    Typically \code{keep} will select a subset of the components of
    the object and return them. The default is not to keep anything.
  }
  \item{steps}{
    the maximum number of steps to be considered.  The default is 1000
    (essentially as many as required).  It is typically used to stop the
    process early.
  }
  \item{use.start}{
    if true the updated fits are done starting at the linear predictor for
    the currently selected model. This may speed up the iterative
    calculations for \code{glm} (and other fits), but it can also slow them
    down. \bold{Not used} in \R.
  }
  \item{k}{
    the multiple of the number of degrees of freedom used for the penalty.
    Only \code{k = 2} gives the genuine AIC: \code{k = log(n)} is
    sometimes referred to as BIC or SBC.
  }
  \item{\dots}{
    any additional arguments to \code{extractAIC}. (None are currently used.)
}}
\value{
  the stepwise-selected model is returned, with up to two additional
  components.  There is an \code{"anova"} component corresponding to the
  steps taken in the search, as well as a \code{"keep"} component if the
  \code{keep=} argument was supplied in the call. The
  \code{"Resid. Dev"} column of the analysis of deviance table refers
  to a constant minus twice the maximized log likelihood: it will be a
  deviance only in cases where a saturated model is well-defined
  (thus excluding \code{lm}, \code{aov} and \code{survreg} fits,
  for example).
}
\details{
  The set of models searched is determined by the \code{scope} argument.
  The right-hand-side of its \code{lower} component is always included
  in the model, and right-hand-side of the model is included in the
  \code{upper} component.  If \code{scope} is a single formula, it
  specifies the \code{upper} component, and the \code{lower} model is
  empty.  If \code{scope} is missing, the initial model is used as the
  \code{upper} model.

  Models specified by \code{scope} can be templates to update
  \code{object} as used by \code{\link{update.formula}}.

  There is a potential problem in using \code{\link{glm}} fits with a
  variable \code{scale}, as in that case the deviance is not simply
  related to the maximized log-likelihood. The \code{glm} method for
  \code{\link{extractAIC}} makes the
  appropriate adjustment for a \code{gaussian} family, but may need to be
  amended for other cases. (The \code{binomial} and \code{poisson}
  families have fixed \code{scale} by default and do not correspond
  to a particular maximum-likelihood problem for variable \code{scale}.)

  Where a conventional deviance exists (e.g. for \code{lm}, \code{aov}
  and \code{glm} fits) this is quoted in the analysis of variance table:
  it is the \emph{unscaled} deviance.
}
\note{
  The model fitting must apply the models to the same dataset.  This may
  be a problem if there are missing values and an \code{na.action} other than
  \code{na.fail} is used (as is the default in \R).
  We suggest you remove the missing values first.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{addterm}}, \code{\link{dropterm}}, \code{\link{step}}
}
\examples{
quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
quine.stp <- stepAIC(quine.nxt,
    scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
    trace = FALSE)
quine.stp$anova

cpus1 <- cpus
for(v in names(cpus)[2:7])
  cpus1[[v]] <- cut(cpus[[v]], unique(quantile(cpus[[v]])),
                    include.lowest = TRUE)
cpus0 <- cpus1[, 2:8]  # excludes names, authors' predictions
cpus.samp <- sample(1:209, 100)
cpus.lm <- lm(log10(perf) ~ ., data = cpus1[cpus.samp,2:8])
cpus.lm2 <- stepAIC(cpus.lm, trace = FALSE)
cpus.lm2$anova

example(birthwt)
birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
birthwt.step <- stepAIC(birthwt.glm, trace = FALSE)
birthwt.step$anova
birthwt.step2 <- stepAIC(birthwt.glm, ~ .^2 + I(scale(age)^2)
    + I(scale(lwt)^2), trace = FALSE)
birthwt.step2$anova

quine.nb <- glm.nb(Days ~ .^4, data = quine)
quine.nb2 <- stepAIC(quine.nb)
quine.nb2$anova
}
\keyword{models}
