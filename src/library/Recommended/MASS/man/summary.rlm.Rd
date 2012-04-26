% file MASS/man/summary.rlm.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{summary.rlm}
\alias{summary.rlm}
\alias{print.summary.rlm}
\title{
Summary Method for Robust Linear Models
}
\description{
\code{summary} method for objects of class \code{"rlm"}
}
\usage{
\method{summary}{rlm}(object, method = c("XtX", "XtWX"), correlation = FALSE, \dots)
}
\arguments{
\item{object}{
the fitted model.
This is assumed to be the result of some fit that produces
an object inheriting from the class \code{rlm}, in the sense that
the components returned by the \code{rlm} function will be available.
}
\item{method}{
Should the weighted (by the IWLS weights) or unweighted cross-products
matrix be used?
}
\item{correlation}{
logical. Should correlations be computed (and printed)?
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
If printing takes place, only a null value is returned.
Otherwise, a list is returned with the following components.
Printing always takes place if this function is invoked automatically
as a method for the \code{summary} function.

\item{correlation}{
The computed correlation coefficient matrix for the coefficients in the model.
}
\item{cov.unscaled}{
The unscaled covariance matrix; i.e, a matrix such that multiplying it by
an estimate of the error variance produces an estimated covariance matrix
for the coefficients.
}
\item{sigma}{
The scale estimate.
}
\item{stddev}{
A scale estimate used for the standard errors.
}
\item{df}{
The number of degrees of freedom for the model and for residuals.
}
\item{coefficients}{
A matrix with three columns, containing the coefficients, their standard errors
and the corresponding t statistic.
}
\item{terms}{
The terms object used in fitting this model.
}}
\details{
This function is a method for the generic function
\code{summary()} for class \code{"rlm"}.
It can be invoked by calling \code{summary(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{summary.rlm(x)} regardless of the
class of the object.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{summary}}
}
\examples{
summary(rlm(calls ~ year, data = phones, maxit = 50))
\dontrun{
Call:
rlm(formula = calls ~ year, data = phones, maxit = 50)

Residuals:
   Min     1Q Median     3Q    Max
-18.31  -5.95  -1.68  26.46 173.77

Coefficients:
            Value    Std. Error t value
(Intercept) -102.622   26.553   -3.86
year           2.041    0.429    4.76

Residual standard error: 9.03 on 22 degrees of freedom

Correlation of Coefficients:
[1] -0.994

}}
\keyword{robust}
