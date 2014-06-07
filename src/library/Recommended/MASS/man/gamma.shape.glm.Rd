% file MASS/man/gamma.shape.glm.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{gamma.shape}
\alias{gamma.shape}
\alias{gamma.shape.glm}
\alias{print.gamma.shape}
\title{
Estimate the Shape Parameter of the Gamma Distribution in a GLM Fit
}
\description{
Find the maximum likelihood estimate of the shape parameter of
the gamma distribution after fitting a \code{Gamma} generalized
linear model.
}
\usage{
\method{gamma.shape}{glm}(object, it.lim = 10,
            eps.max = .Machine$double.eps^0.25, verbose = FALSE, \dots)
}
\arguments{
\item{object}{
Fitted model object from a \code{Gamma} family or \code{quasi} family with
\code{variance = "mu^2"}.
}
\item{it.lim}{
Upper limit on the number of iterations.
}
\item{eps.max}{
Maximum discrepancy between approximations for the iteration
process to continue.
}
\item{verbose}{
If \code{TRUE}, causes successive iterations to be printed out.  The
initial estimate is taken from the deviance.
}
\item{\dots}{
further arguments passed to or from other methods.
}}
\value{
List of two components

\item{alpha}{
the maximum likelihood estimate
}
\item{SE}{
the approximate standard error, the square-root of the reciprocal of
the observed information.
}}
\details{
A glm fit for a Gamma family correctly calculates the maximum
likelihood estimate of the mean parameters but provides only a
crude estimate of the dispersion parameter.  This function takes
the results of the glm fit and solves the maximum likelihood
equation for the reciprocal of the dispersion parameter, which is
usually called the shape (or exponent) parameter.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{gamma.dispersion}}
}
\examples{
clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
clot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
gamma.shape(clot1)

gm <- glm(Days + 0.1 ~ Age*Eth*Sex*Lrn,
          quasi(link=log, variance="mu^2"), quine,
          start = c(3, rep(0,31)))
gamma.shape(gm, verbose = TRUE)
summary(gm, dispersion = gamma.dispersion(gm))  # better summary
}
\keyword{models}
