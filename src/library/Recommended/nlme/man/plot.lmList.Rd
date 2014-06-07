% File nlme/man/plot.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.lmList.Rd,v 1.10.2.1 2003/04/17 22:28:45 bates Exp $
\name{plot.lmList}
\title{Plot an lmList Object}
\usage{
\method{plot}{lmList}(x, form, abline, id, idLabels, grid, \dots)
}
\alias{plot.lmList}
\arguments{
 \item{x}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.}
 \item{form}{an optional formula specifying the desired type of
   plot. Any variable present in the original data frame used to obtain
   \code{x} can be referenced. In addition, \code{x} itself
   can be referenced in the formula using the symbol
   \code{"."}. Conditional expressions on the right of a \code{|}
   operator can be used to define separate panels in a Trellis
   display. Default is \code{resid(., type = "pool") ~ fitted(.) },
   corresponding to a plot of the standardized residuals (using a pooled
   estimate for the residual standard error) versus fitted values.
 } 
 \item{abline}{an optional numeric value, or numeric vector of length
   two. If given as a single value, a horizontal line will be added to the
   plot at that coordinate; else, if given as a vector, its values are
   used as the intercept and slope for a line added to the plot. If
   missing, no lines are added to the plot.
 }
 \item{id}{an optional numeric value, or one-sided formula. If given as
   a value, it is used as a significance level for a two-sided outlier
   test for the standardized residuals. Observations with
   absolute standardized residuals greater than the \eqn{1 - value/2}
   quantile of the standard normal distribution are identified in the
   plot using \code{idLabels}. If given as a one-sided formula, its
   right hand side must evaluate to a  logical, integer, or character
   vector which is used to identify observations in the plot. If
   missing, no observations are identified.
 }
 \item{idLabels}{an optional vector, or one-sided formula. If given as a
   vector, it is converted to character and used to label the
   observations identified according to \code{id}. If given as a
   one-sided formula, its right hand side must evaluate to a vector
   which is converted to character and used to label the identified
   observations. Default is \code{getGroups(x)}. 
 }
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default depends on the type of Trellis plot used:
   if \code{xyplot} defaults to \code{TRUE}, else defaults to
   \code{FALSE}.
 }
 \item{\dots}{optional arguments passed to the Trellis plot function.}
}
\description{
  Diagnostic plots for the linear model fits corresponding to the
  \code{x}  components are obtained. The \code{form} argument gives
  considerable flexibility in the type of  plot specification. A
  conditioning expression (on the right side of a  \code{|} operator)
  always implies that different panels are used for  each level of the
  conditioning factor, according to a Trellis  display. If \code{form}
  is a one-sided formula, histograms of the  variable on the right hand
  side of the formula, before a \code{|}  operator, are displayed (the
  Trellis function \code{histogram} is  used). If \code{form} is
  two-sided and both its left and  right hand side variables are
  numeric, scatter plots are displayed  (the Trellis function
  \code{xyplot} is used). Finally, if \code{form}  is two-sided and its
  left had side variable is a factor, box-plots of  the right hand side
  variable by the levels of the left hand side  variable are displayed
  (the Trellis function  \code{bwplot} is used).  
}
\value{
  a diagnostic Trellis plot.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lmList}},\code{\link{predict.lm}},
  \code{\link{xyplot}}, \code{\link{bwplot}}, \code{\link{histogram}}
}
\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
# standardized residuals versus fitted values by gender
plot(fm1, resid(., type = "pool") ~ fitted(.) | Sex, abline = 0, id = 0.05)
# box-plots of residuals by Subject
plot(fm1, Subject ~ resid(.))
# observed versus fitted values by Subject
plot(fm1, distance ~ fitted(.) | Subject, abline = c(0,1))
}
\keyword{models}
