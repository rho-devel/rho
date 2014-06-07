% File nlme/man/nlmeObject.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlmeObject.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{nlmeObject}
\title{Fitted nlme Object}
\alias{nlmeObject}
\description{
  An object returned by the \code{nlme} function, inheriting from class
  \code{nlme}, also inheriting from class \code{lme}, and representing a
  fitted nonlinear mixed-effects model. Objects of this class have
  methods for the generic functions  \code{anova}, \code{coef},
  \code{fitted}, \code{fixed.effects}, \code{formula}, \code{getGroups},
  \code{getResponse}, \code{intervals}, \code{logLik}, \code{pairs},
  \code{plot}, \code{predict}, \code{print}, \code{random.effects},
  \code{residuals}, \code{summary}, and \code{update}.   

}
\value{
  The following components must be included in a legitimate \code{nlme}
  object. 
  \item{apVar}{an approximate covariance matrix for the
    variance-covariance coefficients. If \code{apVar = FALSE} in the list
    of control values used in the call to \code{nlme}, this
    component is equal to \code{NULL}.}
  \item{call}{a list containing an image of the \code{nlme} call that
    produced the object.}
  \item{coefficients}{a list with two components, \code{fixed} and
    \code{random}, where the first is a vector containing the estimated
    fixed effects and the second is a list of matrices with the estimated
    random effects for each level of grouping. For each matrix in the
    \code{random} list, the columns refer to the random effects and the
    rows to the groups.}
  \item{contrasts}{a list with the contrasts used to represent factors
    in the fixed effects formula  and/or random effects formula. This
    information is important for making predictions from a new data
    frame in which not all levels of the original factors are
    observed. If no factors are used in the nlme model, this component
    will be an empty list.}
  \item{dims}{a list with basic dimensions used in the nlme fit,
    including the components \code{N} - the number of observations in
    the data, \code{Q} - the number of grouping levels, \code{qvec} -
    the number of random effects at each level from innermost to
    outermost (last two values are equal to zero and correspond to the
    fixed effects and the response), \code{ngrps} - the number of groups
    at each level from innermost to outermost (last two values are one
    and correspond to the fixed effects and the response), and
    \code{ncol} - the number of columns in the model matrix for each
    level of grouping from innermost to outermost (last two values are
    equal to the number of fixed effects and one).}
  \item{fitted}{a data frame with the fitted values as columns. The
    leftmost column corresponds to the population fixed effects
    (corresponding to the fixed effects only) and successive columns
    from left to right correspond to increasing levels of grouping.}
  \item{fixDF}{a list with components \code{X} and \code{terms}
    specifying the denominator degrees of freedom for, respectively,
    t-tests for the individual fixed effects and F-tests for the
    fixed-effects terms in the models.}
  \item{groups}{a data frame with the grouping factors as
    columns. The grouping level increases from left to right.}
  \item{logLik}{the (restricted) log-likelihood at convergence.}
  \item{map}{a list with components \code{fmap}, \code{rmap},
    \code{rmapRel}, and \code{bmap}, specifying various mappings for the
    fixed and random effects, used to generate predictions from the
    fitted object.}
  \item{method}{the estimation method: either \code{"ML"} for maximum
    likelihood, or \code{"REML"} for restricted maximum likelihood.}
  \item{modelStruct}{an object inheriting from class \code{nlmeStruct},
    representing a list of mixed-effects model components, such
    as \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.} 
  \item{numIter}{the number of iterations used in the iterative
    algorithm.} 
  \item{residuals}{a data frame with the residuals as columns. The
    leftmost column corresponds to the population residuals
    and successive columns from left to right correspond to increasing
    levels of grouping.} 
  \item{sigma}{the estimated within-group error standard deviation.}
  \item{varFix}{an approximate covariance matrix of the
    fixed effects estimates.}
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{nlme}}, \code{nlmeStruct}}

\keyword{models}
