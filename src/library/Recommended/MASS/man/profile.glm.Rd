% file MASS/man/profile.glm.Rd
% copyright (C) 1999-2008 W. N. Venables and B. D. Ripley

\name{profile.glm}
\alias{profile.glm}
\title{Method for Profiling glm Objects}
\description{
  Investigates the profile log-likelihood function for a fitted model of
  class \code{"glm"}.
}
\usage{
\method{profile}{glm}(fitted, which = 1:p, alpha = 0.01, maxsteps = 10,
        del = zmax/5, trace = FALSE, \dots)
}
\arguments{
  \item{fitted}{the original fitted model object.}
  \item{which}{the original model parameters which should be profiled.
    This can be a numeric or character vector.
    By default, all parameters are profiled.}
  \item{alpha}{highest significance level allowed for the
    profile t-statistics.}
  \item{maxsteps}{maximum number of points to be used for profiling each
    parameter.}
  \item{del}{suggested change on the scale of the profile
    t-statistics.  Default value chosen to allow profiling at about
    10 parameter values.}
  \item{trace}{logical: should the progress of profiling be reported?}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A list of classes \code{"profile.glm"} and \code{"profile"} with an
  element for each parameter being profiled.  The elements are
  data-frames with two variables
  \item{par.vals}{a matrix of parameter values for each fitted model.}
  \item{tau}{the profile t-statistics.}
}
\details{
  The profile t-statistic is defined as the square root of change in
  sum-of-squares divided by residual standard error with an
  appropriate sign.
}
\author{
  Originally, D. M. Bates and W. N. Venables.  (For S in 1996.)
}
\seealso{
  \code{\link{glm}}, \code{\link{profile}}, \code{\link{plot.profile}}
}
\examples{
options(contrasts = c("contr.treatment", "contr.poly"))
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
pr1 <- profile(budworm.lg)
plot(pr1)
pairs(pr1)
}
\keyword{regression}
\keyword{models}
