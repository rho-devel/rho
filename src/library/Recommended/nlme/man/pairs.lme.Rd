% File nlme/man/pairs.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pairs.lme.Rd,v 1.10 2002/04/16 14:28:53 bates Exp $
\name{pairs.lme}
\title{Pairs Plot of an lme Object}
\usage{
\method{pairs}{lme}(x, form, label, id, idLabels, grid, \dots)
}
\alias{pairs.lme}
\arguments{
 \item{x}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{form}{an optional one-sided formula specifying the desired type of
   plot. Any variable present in the original data frame used to obtain
   \code{x} can be referenced. In addition, \code{x} itself
   can be referenced in the formula using the symbol
   \code{"."}. Conditional expressions on the right of a \code{|}
   operator can be used to define separate panels in a Trellis
   display. The expression on the right hand side of \code{form}, and to
   the left of the \code{|} operator, must evaluate to a data frame with
   at least two columns. Default is \code{~ coef(.) }, corresponding to
   a pairs plot of the coefficients evaluated at the innermost level of
   nesting.}    
 \item{label}{an optional character vector of labels for the variables
   in the pairs plot.}
 \item{id}{an optional numeric value, or one-sided formula. If given as
   a value, it is used as a significance level for an outlier
   test based on the Mahalanobis distances of the estimated random
   effects. Groups with random effects distances greater than the
   \eqn{1-value} percentile of the appropriate chi-square distribution
   are identified in the plot using \code{idLabels}. If given as a
   one-sided formula, its right hand side must evaluate to a  logical,
   integer, or character vector which is used to identify points in the
   plot. If missing, no points are identified.}
 \item{idLabels}{an optional vector, or one-sided formula. If given as a
   vector, it is converted to character and used to label the
   points identified according to \code{id}. If given as a
   one-sided formula, its right hand side must evaluate to a vector
   which is converted to character and used to label the identified
   points. Default is the innermost grouping factor. 
 }
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default is \code{FALSE}.}
 \item{\dots}{optional arguments passed to the Trellis plot function.}
}
\description{
  Diagnostic plots for the linear mixed-effects fit are obtained. The
  \code{form} argument gives considerable flexibility in the type of
  plot specification. A conditioning expression (on the right side of a
  \code{|} operator) always implies that different panels are used for
  each level of the conditioning factor, according to a Trellis
  display. The expression on the right hand side of the formula, before
  a \code{|} operator, must evaluate to a data frame with at least two
  columns. If the data frame has two columns, a scatter plot of the two
  variables is displayed (the Trellis function \code{xyplot} is
  used). Otherwise, if more than two columns are present, a scatter plot
  matrix with pairwise scatter plots of the columns in the data frame is
  displayed (the Trellis function \code{splom} is used). 
}
\value{
  a diagnostic Trellis plot.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lme}},
  \code{\link{pairs.compareFits}}, 
  \code{\link{pairs.lmList}}, 
  \code{\link{xyplot}},
  \code{\link{splom}}
}
\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
# scatter plot of coefficients by gender, identifying unusual subjects
pairs(fm1, ~coef(., augFrame = TRUE) | Sex, id = 0.1, adj = -0.5)    
# scatter plot of estimated random effects
\dontrun{
pairs(fm1, ~ranef(.))
}
}
\keyword{models}
