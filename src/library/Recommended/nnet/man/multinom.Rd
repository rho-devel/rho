% file nnet/man/multinom.Rd
% copyright (C) 1994-2006 W. N. Venables and B. D. Ripley
%
\name{multinom}
\alias{multinom}
\alias{add1.multinom}
\alias{anova.multinom}
\alias{coef.multinom}
\alias{drop1.multinom}
\alias{extractAIC.multinom}
\alias{predict.multinom}
\alias{print.multinom}
\alias{summary.multinom}
\alias{print.summary.multinom}
\alias{vcov.multinom}
\alias{model.frame.multinom}
\alias{logLik.multinom}
\concept{multiple logistic}
\title{
Fit Multinomial Log-linear Models
}
\description{
Fits multinomial log-linear models via neural networks.
}
\usage{
multinom(formula, data, weights, subset, na.action,
         contrasts = NULL, Hess = FALSE, summ = 0, censored = FALSE,
         model = FALSE, \dots)
}
\arguments{
\item{formula}{
a formula expression as for regression models, of the form
\code{response ~ predictors}. The response should be a factor or a
matrix with K columns, which will be interpreted as counts for each of
K classes.
A log-linear model is fitted, with coefficients zero for the first
class. An offset can be included: it should be a numeric matrix with K columns
if the response is either a matrix with K columns or a factor with K > 2
classes, or a numeric vector for a response factor with 2 levels.
See the documentation of \code{\link{formula}()} for other details.
}
\item{data}{
an optional data frame in which to interpret the variables occurring
in \code{formula}.
}
\item{weights}{
optional case weights in fitting.
}
\item{subset}{
expression saying which subset of the rows of the data should  be used
in the fit. All observations are included by default.
}
\item{na.action}{
a function to filter missing data.
}
\item{contrasts}{
a list of contrasts to be used for some or all of
the factors appearing as variables in the model formula.
}
\item{Hess}{
logical for whether the Hessian (the observed/expected information matrix)
should be returned.
}
\item{summ}{
integer; if non-zero summarize by deleting duplicate rows and adjust weights.
Methods 1 and 2 differ in speed (2 uses \code{C}); method 3 also combines rows
with the same X and different Y, which changes the baseline for the
deviance.
}
\item{censored}{
If Y is a matrix with \code{K > 2} columns, interpret the entries as one
for possible classes, zero for impossible classes, rather than as
counts.
}
\item{model}{
  logical. If true, the model frame is saved as component \code{model}
  of the returned object.
}
\item{\dots}{
additional arguments for \code{nnet}
}}
\details{
  \code{multinom} calls \code{\link{nnet}}.  The variables on the rhs of
  the formula should be roughly scaled to [0,1] or the fit will be slow
  or may not converge at all.
}
\value{
A \code{nnet} object with additional components:

\item{deviance}{
the residual deviance, compared to the full saturated model (that
explains individual observations exactly).  Also, minus twice log-likelihood.
}
\item{edf}{
the (effective) number of degrees of freedom used by the model
}
\item{AIC}{
the AIC for this fit.
}
\item{Hessian}{
(if \code{Hess} is true).
}
\item{model}{
(if \code{model} is true).
}}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{nnet}}
}
\examples{
options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)
(bwt.mu <- multinom(low ~ ., bwt))
\dontrun{Call:
multinom(formula = low ~ ., data = bwt)

Coefficients:
 (Intercept)         age         lwt raceblack raceother
    0.823477 -0.03724311 -0.01565475  1.192371 0.7406606
     smoke      ptd        ht        ui       ftv1     ftv2+
  0.7555234 1.343648 1.913213 0.6802007 -0.4363238 0.1789888


Residual Deviance: 195.4755
AIC: 217.4755
}
}
\keyword{neural}
\keyword{models}
