% File nlme/man/qqnorm.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: qqnorm.lme.Rd,v 1.8.2.1 2003/04/17 22:28:45 bates Exp $
\name{qqnorm.lme}
\title{Normal Plot of Residuals or Random Effects from an lme Object}
\usage{
\method{qqnorm}{lme}(y, form, abline, id, idLabels, grid, \dots)
}
\alias{qqnorm.lm}
\alias{qqnorm.lme}
\alias{qqnorm.lmList}
\alias{qqnorm.nls}
\arguments{
 \item{y}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model or from class \code{lmList},
   representing a list of \code{lm} objects, or from class \code{lm},
   representing a fitted linear model, or from class \code{nls},
   representing a nonlinear least squares fitted model.}
 \item{form}{an optional one-sided formula specifying the desired type of
   plot. Any variable present in the original data frame used to obtain
   \code{y} can be referenced. In addition, \code{y} itself
   can be referenced in the formula using the symbol
   \code{"."}. Conditional expressions on the right of a \code{|}
   operator can be used to define separate panels in a Trellis
   display. The expression on the right hand side of \code{form} and to
   the left of a \code{|} operator must evaluate to a residuals vector,
   or a random effects matrix. Default is \code{~ resid(., type = "p")},
   corresponding to a normal plot of the standardized residuals
   evaluated at the innermost level of nesting.}  
 \item{abline}{an optional numeric value, or numeric vector of length
   two. If given as a single value, a horizontal line will be added to the
   plot at that coordinate; else, if given as a vector, its values are
   used as the intercept and slope for a line added to the plot. If
   missing, no lines are added to the plot.}
 \item{id}{an optional numeric value, or one-sided formula. If given as
   a value, it is used as a significance level for a two-sided outlier
   test for the standardized residuals (random effects). Observations with
   absolute standardized residuals (random effects) greater than the
   \eqn{1 - value/2} quantile of the standard normal distribution are
   identified in the plot using \code{idLabels}. If given as a one-sided
   formula, its right hand side must evaluate to a  logical, integer, or
   character vector which is used to identify observations in the
   plot. If missing, no observations are identified.}
 \item{idLabels}{an optional vector, or one-sided formula. If given as a
   vector, it is converted to character and used to label the
   observations identified according to \code{id}. If given as a
   one-sided formula, its right hand side must evaluate to a vector
   which is converted to character and used to label the identified
   observations. Default is the innermost grouping factor.}
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default is \code{FALSE}.}
 \item{\dots}{optional arguments passed to the Trellis plot function.}
}
\description{
  Diagnostic plots for assessing the normality of residuals and random
  effects in the linear mixed-effects fit are obtained. The
  \code{form} argument gives considerable flexibility in the type of
  plot specification. A conditioning expression (on the right side of a
  \code{|} operator) always implies that different panels are used for
  each level of the conditioning factor, according to a Trellis
  display. 
}
\value{
  a diagnostic Trellis plot for assessing normality of residuals or
  random effects.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lme}}, \code{\link{plot.lme}}}
\examples{
\dontrun{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
# normal plot of standardized residuals by gender
qqnorm(fm1, ~ resid(., type = "p") | Sex, abline = c(0, 1))
# normal plots of random effects
qqnorm(fm1, ~ranef(.))
}
}
\keyword{models}
