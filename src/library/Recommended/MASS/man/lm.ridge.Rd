% file MASS/man/lm.ridge.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lm.ridge}
\alias{lm.ridge}
\alias{plot.ridgelm}
\alias{print.ridgelm}
\alias{select}
\alias{select.ridgelm}
\title{
Ridge Regression
}
\description{
Fit a linear model by ridge regression.
}
\usage{
lm.ridge(formula, data, subset, na.action, lambda = 0, model = FALSE,
         x = FALSE, y = FALSE, contrasts = NULL, \dots)
}
\arguments{
\item{formula}{
  a formula expression as for regression models, of the form
  \code{response ~ predictors}. See the documentation of \code{formula}
  for other details. \code{\link{offset}} terms are allowed.
}
\item{data}{
an optional data frame in which to interpret the variables occurring
in \code{formula}.
}
\item{subset}{
expression saying which subset of the rows of the data should  be used
in the fit.  All observations are included by default.
}
\item{na.action}{
a function to filter missing data.
}
\item{lambda}{
A scalar or vector of ridge constants.
}
\item{model}{
should the model frame be returned?  Not implemented.
}
\item{x}{
should the design matrix be returned?  Not implemented.
}
\item{y}{
should the response be returned?  Not implemented.
}
\item{contrasts}{
a list of contrasts to be used for some or all of factor terms in the
formula. See the \code{contrasts.arg} of \code{\link{model.matrix.default}}.
}
\item{\dots}{
additional arguments to \code{\link{lm.fit}}.
}}
\details{
  If an intercept is present in the model, its coefficient is not
  penalized.  (If you want to penalize an intercept, put in your own
  constant term and remove the intercept.)
}
\value{
A list with components

\item{coef}{
  matrix of coefficients, one row for each value of \code{lambda}.
  Note that these are not on the original scale and are for use by the
  \code{\link{coef}} method.
}
\item{scales}{
scalings used on the X matrix.
}
\item{Inter}{
was intercept included?
}
\item{lambda}{
vector of lambda values
}
\item{ym}{
mean of \code{y}
}
\item{xm}{
column means of \code{x} matrix
}
\item{GCV}{
vector of GCV values
}
\item{kHKB}{
HKB estimate of the ridge constant.
}
\item{kLW}{
L-W estimate of the ridge constant.
}}
\references{
Brown, P. J. (1994)
\emph{Measurement, Regression and Calibration}
Oxford.
}
\seealso{
\code{\link{lm}}
}
\examples{
longley # not the same as the S-PLUS dataset
names(longley)[1] <- "y"
lm.ridge(y ~ ., longley)
plot(lm.ridge(y ~ ., longley,
              lambda = seq(0,0.1,0.001)))
select(lm.ridge(y ~ ., longley,
               lambda = seq(0,0.1,0.0001)))
}
\keyword{models}
