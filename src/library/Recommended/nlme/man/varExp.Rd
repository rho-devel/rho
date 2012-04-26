% File nlme/man/varExp.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varExp.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{varExp}
\title{Exponential Variance Function}
\usage{
varExp(value, form, fixed)
}
\alias{varExp}
\arguments{
  \item{value}{an optional numeric vector, or list of numeric values,
    with the variance function coefficients. \code{Value} must have
    length one, unless a grouping factor is specified in \code{form}.
    If \code{value} has length greater than one, it must have names
    which identify its elements to the levels of the grouping factor
    defined in \code{form}. If a grouping factor is present in
    \code{form} and \code{value} has length one, its value will be
    assigned to all grouping levels. Default is \code{numeric(0)}, which
    results in a vector of zeros of appropriate length being assigned to
    the coefficients when \code{object} is initialized (corresponding
    to constant variance equal to one).}
  \item{form}{an optional one-sided formula of the form \code{~ v}, or
    \code{~ v | g}, specifying a variance covariate \code{v} and,
    optionally, a grouping factor \code{g} for the coefficients. The
    variance covariate must evaluate to a numeric vector and may involve
    expressions using \code{"."}, representing  a fitted model object
    from which fitted values (\code{fitted(.)}) and residuals
    (\code{resid(.)}) can be extracted (this allows the variance
    covariate to be updated during the optimization of an object
    function). When a grouping factor is present in \code{form},
    a different coefficient value is used for each of its
    levels. Several grouping variables may be
    simultaneously specified, separated by the \code{*} operator, like
    in \code{~ v | g1 * g2 * g3}. In this case, the levels of each
    grouping variable are pasted together and the resulting factor is
    used to group the observations. Defaults to \code{~ fitted(.)}
    representing a variance covariate given by the fitted values of a
    fitted model object and no grouping factor. }
  \item{fixed}{an optional numeric vector, or list of numeric values,
    specifying the values at which some or all of the  coefficients in
    the variance function should be fixed. If a grouping factor is
    specified in \code{form}, \code{fixed} must have names identifying
    which coefficients are to be fixed. Coefficients included in
    \code{fixed} are not allowed to vary during the optimization of an
    objective function. Defaults to \code{NULL}, corresponding to no
    fixed coefficients.}
}
\description{
  This function is a constructor for the \code{varExp} class,
  representing an exponential variance function structure. Letting
  \eqn{v} denote the variance covariate and \eqn{\sigma^2(v)}{s2(v)}
  denote the variance function evaluated at \eqn{v}, the exponential
  variance function is defined as \eqn{\sigma^2(v) = \exp(2\theta
    v)}{s2(v) = exp(2* t * v)}, where \eqn{\theta}{t} is the variance
  function coefficient. When a grouping factor is present, a different
  \eqn{\theta}{t} is used for each factor level.
}
\value{
  a \code{varExp} object representing an exponential variance function
  structure, also inheriting from class \code{varFunc}. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{varClasses}},
  \code{\link{varWeights.varFunc}},
  \code{\link{coef.varExp}}}

\examples{
vf1 <- varExp(0.2, form = ~age|Sex)
}
\keyword{models}
