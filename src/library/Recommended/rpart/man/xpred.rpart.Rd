\name{xpred.rpart}
\alias{xpred.rpart}
\title{
  Return Cross-Validated Predictions 
}
\description{
  Gives the predicted values for an \code{rpart} fit, under
  cross validation, for a set of complexity parameter values.
}
\usage{
xpred.rpart(fit, xval=10, cp)
}
\arguments{
\item{fit}{
a \code{rpart} object. 
}
\item{xval}{
  number of cross-validation groups.  This may also be an explicit list of
  integers that define the cross-validation groups.
}
\item{cp}{
  the desired list of complexity values.  By default it is taken from the
  \code{cptable} component of the fit.
}}
\value{
  A matrix with one row for each observation and one column for each complexity
  value.
}
\details{
  Complexity penalties are actually ranges, not values.  If the
  \code{cp} values found in the table were \eqn{.36}, \eqn{.28},
  and \eqn{.13}, for instance, this means that the first row of the
  table holds for all complexity penalties in the range \eqn{[.36, 1]},
  the second row for \code{cp} in the range \eqn{[.28, .36)} and
  the third row for \eqn{[.13,.28)}.  By default, the geometric mean
  of each interval is used for cross validation.
}
\seealso{
\code{\link{rpart}} 
}
\examples{
fit <- rpart(Mileage ~ Weight, car.test.frame)
xmat <- xpred.rpart(fit)
xerr <- (xmat - car.test.frame$Mileage)^2
apply(xerr, 2, sum)   # cross-validated error estimate

# approx same result as rel. error from printcp(fit)
apply(xerr, 2, sum)/var(car.test.frame$Mileage) 
printcp(fit)
}
\keyword{tree}
