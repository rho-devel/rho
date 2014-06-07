% file MASS/man/rlm.Rd
% copyright (C) 1998,9, 2004 B. D. Ripley
%
\name{rlm}
\alias{rlm}
\alias{rlm.default}
\alias{rlm.formula}
\alias{print.rlm}
\alias{predict.rlm}
\alias{psi.bisquare}
\alias{psi.hampel}
\alias{psi.huber}
\title{
  Robust Fitting of Linear Models
}
\description{
  Fit a linear model by robust regression using an M estimator.
}
\usage{
rlm(x, \dots)

\method{rlm}{formula}(formula, data, weights, \dots, subset, na.action,
    method = c("M", "MM", "model.frame"),
    wt.method = c("inv.var", "case"),
    model = TRUE, x.ret = TRUE, y.ret = FALSE, contrasts = NULL)

\method{rlm}{default}(x, y, weights, \dots, w = rep(1, nrow(x)),
    init = "ls", psi = psi.huber,
    scale.est = c("MAD", "Huber", "proposal 2"), k2 = 1.345,
    method = c("M", "MM"), wt.method = c("inv.var", "case"),
    maxit = 20, acc = 1e-4, test.vec = "resid", lqs.control = NULL)

psi.huber(u, k = 1.345, deriv = 0)
psi.hampel(u, a = 2, b = 4, c = 8, deriv = 0)
psi.bisquare(u, c = 4.685, deriv = 0)
}
\arguments{
  \item{formula}{
    a formula of the form \code{y ~ x1 + x2 + \dots}.
  }
  \item{data}{
    data frame from which variables specified in \code{formula} are
    preferentially to be taken.
  }
  \item{weights}{
    a vector of prior weights for each case.
  }
  \item{subset}{
    An index vector specifying the cases to be used in fitting.
  }
  \item{na.action}{
    A function to specify the action to be taken if \code{NA}s are found.
    The \sQuote{factory-fresh} default action in \R is
    \code{\link{na.omit}}, and can be changed by
    \code{\link{options}(na.action=)}.
  }
  \item{x}{
    a matrix or data frame containing the explanatory variables.
  }
  \item{y}{
    the response: a vector of length the number of rows of \code{x}.
  }
  \item{method}{
    currently either M-estimation or MM-estimation or (for the
    \code{formula} method only) find the model frame.  MM-estimation
    is M-estimation with Tukey's biweight initialized by a specific
    S-estimator.  See the \sQuote{Details} section.
  }
  \item{wt.method}{
    are the weights case weights (giving the relative importance of case,
    so a weight of 2 means there are two of these) or the inverse of the
    variances, so a weight of two means this error is half as variable?
  }
  \item{model}{
    should the model frame be returned in the object?
  }
  \item{x.ret}{
    should the model matrix be returned in the object?
  }
  \item{y.ret}{
    should the response be returned in the object?
  }
  \item{contrasts}{
    optional contrast specifications: se \code{\link{lm}}.
  }
  \item{w}{
    (optional) initial down-weighting for each case.
  }
  \item{init}{
    (optional) initial values for the coefficients OR a method to find
    initial values OR the result of a fit with a \code{coef} component.  Known
    methods are \code{"ls"} (the default) for an initial least-squares fit
    using weights \code{w*weights}, and \code{"lts"} for an unweighted
    least-trimmed squares fit with 200 samples.
  }
  \item{psi}{
    the psi function is specified by this argument.  It must give
    (possibly by name) a function \code{g(x, \dots, deriv)} that for
    \code{deriv=0} returns psi(x)/x and for \code{deriv=1} returns
    psi'(x).  Tuning constants will be passed in via \code{\dots}.
  }
  \item{scale.est}{
    method of scale estimation: re-scaled MAD of the residuals (default)
    or Huber's proposal 2 (which can be selected by either \code{"Huber"}
    or \code{"proposal 2"}).
  }
  \item{k2}{
    tuning constant used for Huber proposal 2 scale estimation.
  }
  \item{maxit}{
    the limit on the number of IWLS iterations.
  }
  \item{acc}{
    the accuracy for the stopping criterion.
  }
  \item{test.vec}{
    the stopping criterion is based on changes in this vector.
  }
  \item{\dots}{
    additional arguments to be passed to \code{rlm.default} or to the \code{psi}
    function.
  }
  \item{lqs.control}{
    An optional list of control values for \code{\link{lqs}}.
  }
  \item{u}{
    numeric vector of evaluation points.
  }
  \item{k, a, b, c}{
    tuning constants.
  }
  \item{deriv}{
    \code{0} or \code{1}: compute values of the psi function or of its
    first derivative.
  }
}
\value{
  An object of class \code{"rlm"} inheriting from \code{"lm"}.
  Note that the \code{df.residual} component is deliberately set to
  \code{NA} to avoid inappropriate estimation of the residual scale from
  the residual mean square by \code{"lm"} methods.
  
  The additional components not in an \code{lm} object are

  \item{s}{
    the robust scale estimate used
  }
  \item{w}{
    the weights used in the IWLS process
  }
  \item{psi}{
    the psi function with parameters substituted
  }
  \item{conv}{
    the convergence criteria at each iteration
  }
  \item{converged}{
    did the IWLS converge?
  }
  \item{wresid}{
    a working residual, weighted for \code{"inv.var"} weights only.
  }
}
\details{
  Fitting is done by iterated re-weighted least squares (IWLS).

  Psi functions are supplied for the Huber, Hampel and Tukey bisquare
  proposals as \code{psi.huber}, \code{psi.hampel} and
  \code{psi.bisquare}. Huber's corresponds to a convex optimization
  problem and gives a unique solution (up to collinearity). The other
  two will have multiple local minima, and a good starting point is
  desirable.

  Selecting \code{method = "MM"} selects a specific set of options which
  ensures that the estimator has a high breakdown point. The initial set
  of coefficients and the final scale are selected by an S-estimator
  with \code{k0 = 1.548}; this gives (for \eqn{n \gg p}{n >> p})
  breakdown point 0.5.
  The final estimator is an M-estimator with Tukey's biweight and fixed
  scale that will inherit this breakdown point provided \code{c > k0};
  this is true for the default value of \code{c} that corresponds to
  95\% relative efficiency at the normal.  Case weights are not
  supported for \code{method = "MM"}.
}
\references{
  P. J. Huber (1981)
  \emph{Robust Statistics}.
  Wiley.

  F. R. Hampel, E. M. Ronchetti, P. J. Rousseeuw and W. A. Stahel (1986)
  \emph{Robust Statistics: The Approach based on Influence Functions}.
  Wiley.

  A. Marazzi (1993)
  \emph{Algorithms, Routines and S Functions for Robust Statistics}.
  Wadsworth & Brooks/Cole.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}  Fourth edition.  Springer.
}
\seealso{
  \code{\link{lm}}, \code{\link{lqs}}.
}
\examples{
summary(rlm(stack.loss ~ ., stackloss))
rlm(stack.loss ~ ., stackloss, psi = psi.hampel, init = "lts")
rlm(stack.loss ~ ., stackloss, psi = psi.bisquare)
}
\keyword{models}
\keyword{robust}
