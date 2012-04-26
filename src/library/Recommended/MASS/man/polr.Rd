% file MASS/man/polr.Rd
% copyright (C) 1994-2011 W. N. Venables and B. D. Ripley
%
\name{polr}
\alias{polr}

\title{
  Ordered Logistic or Probit Regression
}
\description{
  Fits a logistic or probit regression model to an ordered factor
  response.  The default logistic case is \emph{proportional odds
    logistic regression}, after which the function is named.
}
\usage{
polr(formula, data, weights, start, \dots, subset, na.action,
     contrasts = NULL, Hess = FALSE, model = TRUE,
     method = c("logistic", "probit", "cloglog", "cauchit"))
}
\arguments{
  \item{formula}{
    a formula expression as for regression models, of the form
    \code{response ~ predictors}. The response should be a factor
    (preferably an ordered factor), which will be interpreted as an
    ordinal response, with levels ordered as in the factor.  
    The model must have an intercept: attempts to remove one will
    lead to a warning and be ignored.  An offset may be used.  See the
    documentation of \code{\link{formula}} for other details.
  }
  \item{data}{
    an optional data frame in which to interpret the variables occurring
    in \code{formula}.
  }
  \item{weights}{
    optional case weights in fitting.  Default to 1.
  }
  \item{start}{
    initial values for the parameters.  This is in the format
    \code{c(coefficients, zeta)}: see the Values section.
  }
  \item{\dots}{
    additional arguments to be passed to \code{\link{optim}}, most often a
    \code{control} argument.
  }
  \item{subset}{
    expression saying which subset of the rows of the data should  be used
    in the fit.  All observations are included by default.
  }
  \item{na.action}{
    a function to filter missing data.
  }
  \item{contrasts}{
    a list of contrasts to be used for some or all of
    the factors appearing as variables in the model formula.
  }
  \item{Hess}{
    logical for whether the Hessian (the observed information matrix)
    should be returned.  Use this if you intend to call \code{summary} or
    \code{vcov} on the fit.
  }
  \item{model}{
    logical for whether the model matrix should be returned.
  }
  \item{method}{
    logistic or probit or complementary log-log or cauchit (corresponding
    to a Cauchy latent variable).
  }
}
\details{
  This model is what Agresti (2002) calls a \emph{cumulative link}
  model.  The basic interpretation is as a \emph{coarsened} version of a
  latent variable \eqn{Y_i} which has a logistic or normal or
  extreme-value or Cauchy distribution with scale parameter one and a
  linear model for the mean.  The ordered factor which is observed is
  which bin \eqn{Y_i} falls into with breakpoints
  \deqn{\zeta_0 = -\infty < \zeta_1 < \cdots < \zeta_K = \infty}{zeta_0 = -Inf < zeta_1 < \dots < zeta_K = Inf}
  This leads to the model
  \deqn{\mbox{logit} P(Y \le k | x) = \zeta_k - \eta}{logit P(Y <= k | x) = zeta_k - eta}
  with \emph{logit} replaced by \emph{probit} for a normal latent
  variable, and \eqn{\eta}{eta} being the linear predictor, a linear
  function of the explanatory variables (with no intercept).  Note
  that it is quite common for other software to use the opposite sign
  for \eqn{\eta}{eta} (and hence the coefficients \code{beta}).

  In the logistic case, the left-hand side of the last display is the
  log odds of category \eqn{k} or less, and since these are log odds
  which differ only by a constant for different \eqn{k}, the odds are
  proportional.  Hence the term \emph{proportional odds logistic
    regression}.

  In the complementary log-log case, we have a \emph{proportional
    hazards} model for grouped survival times.

  There are methods for the standard model-fitting functions, including
  \code{\link{predict}}, \code{\link{summary}}, \code{\link{vcov}},
  \code{\link{anova}}, \code{\link{model.frame}} and an
  \code{extractAIC} method for use with \code{\link{stepAIC}} (and
  \code{\link{step}}.  There are also \code{\link{profile}} and
  \code{\link{confint}} methods.
}
\value{
  A object of class \code{"polr"}.  This has components

  \item{coefficients}{the coefficients of the linear predictor, which has no
    intercept.}
  \item{zeta}{the intercepts for the class boundaries.}
  \item{deviance}{the residual deviance.}
  \item{fitted.values}{a matrix, with a column for each level of the response.}
  \item{lev}{the names of the response levels.}
  \item{terms}{the \code{terms} structure describing the model.}
  \item{df.residual}{the number of residual degrees of freedoms,
    calculated using the weights.}
  \item{edf}{the (effective) number of degrees of freedom used by the model}
  \item{n, nobs}{the (effective) number of observations, calculated using the
    weights. (\code{nobs} is for use by \code{\link{stepAIC}}.}
  \item{call}{the matched call.}
  \item{method}{the matched method used.}
  \item{convergence}{the convergence code returned by \code{optim}.}
  \item{niter}{the number of function and gradient evaluations used by
    \code{optim}.}
  \item{lp}{the linear predictor (including any offset).}
  \item{Hessian}{(if \code{Hess} is true).  Note that this is a
    numerical approximation derived from the optimization proces.}
  \item{model}{(if \code{model} is true).}
}
\note{
  The \code{\link{vcov}} method uses the approximate Hessian: for
  reliable results the model matrix should be sensibly scaled with all
  columns having range the order of one.
}
\references{
  Agresti, A. (2002) \emph{Categorical Data.} Second edition.  Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link{optim}}, \code{\link{glm}}, \code{\link[nnet]{multinom}}.
}
\examples{
options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr
summary(house.plr, digits = 3)
## slightly worse fit from
summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
## although it is not really appropriate, can fit
summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)

predict(house.plr, housing, type = "p")
addterm(house.plr, ~.^2, test = "Chisq")
house.plr2 <- stepAIC(house.plr, ~.^2)
house.plr2$anova
anova(house.plr, house.plr2)

house.plr <- update(house.plr, Hess=TRUE)
pr <- profile(house.plr)
confint(pr)
plot(pr)
pairs(pr)
}
\keyword{models}
