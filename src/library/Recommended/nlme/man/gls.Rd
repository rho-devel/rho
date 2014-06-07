% File nlme/man/gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: gls.Rd,v 1.6 2002/01/28 03:44:17 bates Exp $
\name{gls}
\title{Fit Linear Model Using Generalized Least Squares}
\alias{gls}
\alias{update.gls}
\usage{
gls(model, data, correlation, weights, subset, method, na.action,
    control, verbose)
\method{update}{gls}(object, model., \dots, evaluate = TRUE)
}
\arguments{
 \item{object}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted linear model.}
  \item{model}{a two-sided linear formula object describing the
    model, with the response on the left of a \code{~} operator and the
    terms, separated by \code{+} operators, on the right.}
 \item{model.}{Changes to the model -- see \code{update.formula} for
   details.}
 \item{data}{an optional data frame containing the variables named in
   \code{model}, \code{correlation}, \code{weights}, and
   \code{subset}. By default the variables are taken from the
   environment from which \code{gls} is called.}
 \item{correlation}{an optional \code{corStruct} object describing the
   within-group correlation structure. See the documentation of
   \code{corClasses} for a description of the available \code{corStruct}
   classes. If a grouping variable is to be used, it must be specified in
   the \code{form} argument to the \code{corStruct}
   constructor. Defaults to \code{NULL}, corresponding to uncorrelated 
   errors.}  
 \item{weights}{an optional \code{varFunc} object or one-sided formula
   describing the within-group heteroscedasticity structure. If given as
   a formula, it is used as the argument to \code{varFixed},
   corresponding to fixed variance weights. See the documentation on
   \code{varClasses} for a description of the available \code{varFunc}
   classes. Defaults to \code{NULL}, corresponding to homoscedastic
   errors.} 
 \item{subset}{an optional expression indicating which subset of the rows of
   \code{data} should  be  used in the fit. This can be a logical
   vector, or a numeric vector indicating which observation numbers are
   to be included, or a  character  vector of the row names to be
   included.  All observations are included by default.}
 \item{method}{a character string.  If \code{"REML"} the model is fit by
   maximizing the restricted log-likelihood.  If \code{"ML"} the
   log-likelihood is maximized.  Defaults to \code{"REML"}.}
 \item{na.action}{a function that indicates what should happen when the
   data contain \code{NA}s.  The default action (\code{na.fail}) causes
   \code{gls} to print an error message and terminate if there are any
   incomplete observations.}
 \item{control}{a list of control values for the estimation algorithm to
   replace the default values returned by the function \code{glsControl}.
   Defaults to an empty list.}
 \item{verbose}{an optional logical value. If \code{TRUE} information on
   the evolution of the iterative algorithm is printed. Default is
   \code{FALSE}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
 \item{evaluate}{If \code{TRUE} evaluate the new call else return the call.}
}
\description{
  This function fits a linear model using generalized least
  squares. The errors are allowed to be correlated and/or have unequal
  variances.  
}
\value{
  an object of class \code{gls} representing the linear model
  fit. Generic functions such as \code{print}, \code{plot}, and 
  \code{summary} have methods to show the results of the fit. See
  \code{glsObject} for the components of the fit. The functions
  \code{resid}, \code{coef}, and \code{fitted} can be used to extract
  some of its components. 
}
\references{
 The different correlation structures available for the
 \code{correlation} argument are described in Box, G.E.P., Jenkins,
 G.M., and Reinsel G.C. (1994), Littel, R.C., Milliken, G.A., Stroup,
 W.W., and Wolfinger, R.D. (1996), and Venables, W.N. and Ripley,
 B.D. (2002). The use of variance functions for linear 
 and nonlinear models is presented in detail in Carroll, R.J. and Ruppert,
 D. (1988) and Davidian, M. and Giltinan, D.M. (1995).  

 Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
 Analysis: Forecasting and Control", 3rd Edition, Holden-Day. 

 Carroll, R.J. and Ruppert, D. (1988) "Transformation and Weighting in
 Regression", Chapman and Hall.

 Davidian, M. and Giltinan, D.M. (1995) "Nonlinear Mixed Effects Models
 for Repeated Measurement Data", Chapman and Hall.

 Littel, R.C., Milliken, G.A., Stroup, W.W., and Wolfinger, R.D. (1996)
 "SAS Systems for Mixed Models", SAS Institute.

 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. pp. 100, 461.  

 Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
 S", 4th Edition, Springer-Verlag.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}} 
\seealso{
  \code{\link{corClasses}},
  \code{\link{glsControl}},
  \code{\link{glsObject}},
  \code{\link{glsStruct}},
  \code{\link{plot.gls}}, 
  \code{\link{predict.gls}}, 
  \code{\link{qqnorm.gls}}, 
  \code{\link{residuals.gls}}, 
  \code{\link{summary.gls}}, 
  \code{\link{varClasses}},
  \code{\link{varFunc}}
}
\examples{
# AR(1) errors within each Mare
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
# variance increases as a power of the absolute fitted values
fm2 <- update(fm1, weights = varPower())
}
\keyword{models}
