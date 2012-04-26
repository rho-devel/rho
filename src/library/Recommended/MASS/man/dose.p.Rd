\name{dose.p}
\alias{dose.p}
\alias{print.glm.dose}
\title{
Predict Doses for Binomial Assay model
}
\description{
Calibrate binomial assays, generalizing the calculation of LD50.
}
\usage{
dose.p(obj, cf = 1:2, p = 0.5)
}
\arguments{
\item{obj}{
A fitted model object of class inheriting from \code{"glm"}.
}
\item{cf}{
The terms in the coefficient vector giving the intercept and
coefficient of (log-)dose
}
\item{p}{
Probabilities at which to predict the dose needed.
}}
\value{
  An object of class \code{"glm.dose"} giving the prediction (attribute
  \code{"p"} and standard error (attribute \code{"SE"}) at each response
  probability.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  Springer.
}
\examples{
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg0 <- glm(SF ~ sex + ldose - 1, family = binomial)

dose.p(budworm.lg0, cf = c(1,3), p = 1:3/4)
dose.p(update(budworm.lg0, family = binomial(link=probit)),
       cf = c(1,3), p = 1:3/4)
}
\keyword{regression}
\keyword{models}
