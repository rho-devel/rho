% file MASS/man/theta.md.Rd
% copyright (C) 1994-2005 W. N. Venables and B. D. Ripley
%
\name{theta.md}
\alias{theta.md}
\alias{theta.ml}
\alias{theta.mm}
\title{
Estimate theta of the Negative Binomial
}
\description{
  Given the estimated mean vector, estimate \code{theta} of the
  Negative Binomial Distribution.
}
\usage{
theta.md(y, mu, dfr, weights, limit = 20, eps = .Machine$double.eps^0.25)

theta.ml(y, mu, n, weights, limit = 10, eps = .Machine$double.eps^0.25,
         trace = FALSE)

theta.mm(y, mu, dfr, weights, limit = 10, eps = .Machine$double.eps^0.25)
}
\arguments{
  \item{y}{
    Vector of observed values from the Negative Binomial.
  }
  \item{mu}{
    Estimated mean vector.
  }
  \item{n}{
    Number of data points (defaults to the sum of \code{weights})
  }
  \item{dfr}{
    Residual degrees of freedom (assuming \code{theta} known).  For
    a weighted fit this is the sum of the weights minus the number of
    fitted parameters.
  }
  \item{weights}{
    Case weights.  If missing, taken as 1.
  }
  \item{limit}{
    Limit on the number of iterations.
  }
  \item{eps}{
    Tolerance to determine convergence.
  }
  \item{trace}{
    logical: should iteration progress be printed?
  }
}
\details{
  \code{theta.md} estimates by equating the deviance to the residual
  degrees of freedom, an analogue of a moment estimator.

  \code{theta.ml} uses maximum likelihood.

  \code{theta.mm} calculates the moment estimator of \code{theta} by
  equating the Pearson chi-square
  \eqn{\sum (y-\mu)^2/(\mu+\mu^2/\theta)}{sum((y-mu)^2/(mu+mu^2/theta))}
  to the residual degrees of freedom.
}
\value{
  The required estimate of \code{theta}, as a scalar.
  For \code{theta.ml}, the standard error is given as attribute \code{"SE"}.
}
\seealso{
  \code{\link{glm.nb}}
}
\examples{
quine.nb <- glm.nb(Days ~ .^2, data = quine)
theta.md(quine$Days, fitted(quine.nb), dfr = df.residual(quine.nb))
theta.ml(quine$Days, fitted(quine.nb))
theta.mm(quine$Days, fitted(quine.nb), dfr = df.residual(quine.nb))

## weighted example
yeast <- data.frame(cbind(numbers = 0:5, fr = c(213, 128, 37, 18, 3, 1)))
fit <- glm.nb(numbers ~ 1, weights = fr, data = yeast)
summary(fit)
mu <- fitted(fit)
theta.md(yeast$numbers, mu, dfr = 399, weights = yeast$fr)
theta.ml(yeast$numbers, mu, limit = 15, weights = yeast$fr)
theta.mm(yeast$numbers, mu, dfr = 399, weights = yeast$fr)
}
\keyword{models}
