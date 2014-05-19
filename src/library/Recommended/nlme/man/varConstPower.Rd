% File nlme/man/varConstPower.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varConstPower.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varConstPower}
\title{Constant Plus Power Variance Function}
\usage{
varConstPower(const, power, form, fixed)
}
\alias{varConstPower}
\arguments{
  \item{const, power}{optional numeric vectors, or lists of numeric
    values, with, respectively, the coefficients for the constant 
    and the power terms. Both arguments must have length one, unless a
    grouping factor is specified in \code{form}. If either argument has
    length greater than one, it must have names which identify its
    elements to the levels of the grouping factor defined in
    \code{form}. If a grouping factor is present in 
    \code{form} and the argument has length one, its value will be
    assigned to all grouping levels. Only positive values are allowed
    for \code{const}. Default is \code{numeric(0)}, which
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
    simultaneously specified, separated by the \code{*} operator, as
    in \code{~ v | g1 * g2 * g3}. In this case, the levels of each
    grouping variable are pasted together and the resulting factor is
    used to group the observations. Defaults to \code{~ fitted(.)}
    representing a variance covariate given by the fitted values of a
    fitted model object and no grouping factor. }
  \item{fixed}{an optional list with components \code{const} and/or
    \code{power}, consisting of numeric vectors, or lists of numeric
    values, specifying the values at which some or all of the
    coefficients in the variance function should be fixed. If a grouping
    factor is specified in \code{form}, the components of \code{fixed}
    must have names identifying which coefficients are to be
    fixed. Coefficients included in \code{fixed} are not allowed to vary
    during the optimization of an objective function. Defaults to
    \code{NULL}, corresponding to no fixed coefficients.}
}
\description{
  This function is a constructor for the \code{varConstPower} class,
  representing a constant plus power variance function
  structure. Letting \eqn{v} denote the variance covariate and
  \eqn{\sigma^2(v)}{s2(v)} denote the variance function evaluated at
  \eqn{v}, the constant plus power variance function is defined as
  \eqn{\sigma^2(v) = (\theta_1 + |v|^\theta_2)^2}{s2(v) = (t1 +
    |v|^t2)^2}, where \eqn{\theta_1,\theta_2}{t1, t2} are the variance  
  function coefficients. When a grouping factor is present, different
  \eqn{\theta_1,\theta_2}{t1, t2} are used for each factor level.
}
\value{
  a \code{varConstPower} object representing a constant plus power
  variance function structure, also inheriting from class
  \code{varFunc}.  
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{varClasses}},
  \code{\link{varWeights.varFunc}},
  \code{\link{coef.varConstPower}}}

\examples{
vf1 <- varConstPower(1.2, 0.2, form = ~age|Sex)
}
\keyword{models}
