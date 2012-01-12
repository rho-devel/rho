% File nlme/man/qqnorm.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: qqnorm.gls.Rd,v 1.9.2.1 2003/04/17 22:28:45 bates Exp $
\name{qqnorm.gls}
\title{Normal Plot of Residuals from a gls Object}
\usage{
\method{qqnorm}{gls}(y, form, abline, id, idLabels, grid, \dots)
}
\alias{qqnorm.gls}
\arguments{
 \item{y}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted model.}
 \item{form}{an optional one-sided formula specifying the desired type of
   plot. Any variable present in the original data frame used to obtain
   \code{y} can be referenced. In addition, \code{y} itself
   can be referenced in the formula using the symbol
   \code{"."}. Conditional expressions on the right of a \code{|}
   operator can be used to define separate panels in a Trellis
   display. The expression on the right hand side of \code{form} and to
   the left of a \code{|} operator must evaluate to a residuals
   vector. Default is \code{~ resid(., type = "p")}, 
   corresponding to a normal plot of the standardized residuals.}  
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
   be added to plot. Default depends on the type of Trellis plot used:
   if \code{xyplot} defaults to \code{TRUE}, else defaults to
   \code{FALSE}.}
 \item{\dots}{optional arguments passed to the Trellis plot function.}
}
\description{
  Diagnostic plots for assessing the normality of residuals the
  generalized least squares fit are obtained. The \code{form} argument
  gives considerable flexibility in the type of plot specification. A
  conditioning expression (on the right side of a \code{|} operator)
  always implies that different panels are used for each level of the
  conditioning factor, according to a Trellis display. 
}
\value{
  a diagnostic Trellis plot for assessing normality of residuals.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{gls}}, \code{\link{plot.gls}}}
\examples{
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
qqnorm(fm1, abline = c(0,1))
}
\keyword{models}
