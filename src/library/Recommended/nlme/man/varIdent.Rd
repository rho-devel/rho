% File nlme/man/varIdent.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varIdent.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varIdent}
\title{Constant Variance Function}
\usage{
varIdent(value, form, fixed)
}
\alias{varIdent}
\arguments{
  \item{value}{an optional numeric vector, or list of numeric values,
    with the variance function coefficients. If no grouping factor is
    present in \code{form}, this argument is ignored, as the resulting
    variance function contains no coefficients. If \code{value} has
    length one, its value is repeated for all coefficients in the
    variance function. If \code{value} has length greater than one, it
    must have length equal to the number of grouping levels minus one
    and names which identify its elements to the levels of the grouping
    factor. Only positive values are allowed for this argument. Default
    is \code{numeric(0)}, which results in a vector of zeros of
    appropriate length being assigned to the coefficients when
    \code{object} is initialized (corresponding to constant variance 
    equal to one).}  
  \item{form}{an optional one-sided formula of the form \code{~ v}, or
    \code{~ v | g}, specifying a variance covariate \code{v} and,
    optionally, a grouping factor \code{g} for the coefficients. The
    variance covariate is ignored in this variance function. When a
    grouping factor is present in \code{form}, a different coefficient
    value is used for each of its levels less one reference level (see
    description section below). Several grouping variables may be
    simultaneously specified, separated by the \code{*} operator, like
    in \code{~ v | g1 * g2 * g3}. In this case, the levels of each
    grouping variable are pasted together and the resulting factor is
    used to group the observations. Defaults to \code{~ 1}. }
  \item{fixed}{an optional numeric vector, or list of numeric values,
    specifying the values at which some or all of the  coefficients in
    the variance function should be fixed. It must have names
    identifying which coefficients are to be fixed. Coefficients
    included in \code{fixed} are not allowed to vary during the
    optimization of an objective function. Defaults to \code{NULL},
    corresponding to no fixed coefficients.}
}
\description{
  This function is a constructor for the \code{varIdent} class,
  representing a constant variance function structure. If no grouping
  factor is present in \code{form}, the variance function is constant
  and equal to one, and no coefficients required to represent it. When
  \code{form} includes a grouping factor with \eqn{M > 1} levels, the
  variance function allows M different variances, one  for each level of
  the factor. For identifiability reasons, the coefficients of the
  variance function represent the ratios between the variances and a
  reference variance (corresponding to a reference group
  level). Therefore, only \eqn{M-1} coefficients are needed to represent
  the variance function. By default, if the elements in \code{value} are
  unnamed, the first group level is taken as the reference level.
}
\value{
  a \code{varIdent} object representing a constant variance function
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
  \code{\link{coef.varIdent}}}

\examples{
vf1 <- varIdent(c(Female = 0.5), form = ~ 1 | Sex)
}
\keyword{models}
