% File nlme/man/gnls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: gnls.Rd,v 1.7.2.4 2003/08/09 22:45:17 bates Exp $
\name{gnls}
\title{Fit Nonlinear Model Using Generalized Least Squares}
\usage{
gnls(model, data, params, start, correlation, weights, subset,
     na.action, naPattern, control, verbose)
%\method{update}{gnls}(object, model, data, params, start, correlation, 
%       weights, subset, na.action, naPattern, control, verbose, \dots) 
}
\alias{gnls}
%\alias{update.gnls}
\arguments{
  \item{model}{a two-sided formula object describing the
    model, with the response on the left of a \code{~} operator and 
    a nonlinear expression involving parameters and covariates on the
    right. If \code{data} is given, all names used in the formula should
    be defined as parameters or variables in the data frame.} 
 \item{data}{an optional data frame containing the variables named in
   \code{model}, \code{correlation}, \code{weights}, 
   \code{subset}, and \code{naPattern}. By default the variables are 
   taken from the environment from which \code{gnls} is called.}
 \item{params}{an optional two-sided linear formula of the form
   \code{p1+...+pn~x1+...+xm}, or list of two-sided formulas of the form
   \code{p1~x1+...+xm}, with possibly different models for each
   parameter. The \code{p1,\dots,pn} represent parameters included on the
   right hand side of \code{model} and \code{x1+...+xm} define a linear
   model for the parameters (when the left hand side of the formula
   contains several parameters, they are all assumed to follow the same
   linear model described by the right hand side expression). A \code{1}
   on the right hand side of the formula(s) indicates a single fixed
   effects for the corresponding parameter(s). By default, the
   parameters are obtained from the names of \code{start}.} 
 \item{start}{an optional named list, or numeric vector, with the
   initial values for the parameters in \code{model}. It can be omitted
   when a \code{selfStarting} function is used in \code{model}, in which
   case the starting estimates will be obtained from a single call to the
   \code{nls} function.}
 \item{correlation}{an optional \code{corStruct} object describing the
   within-group correlation structure. See the documentation of
   \code{corClasses} for a description of the available \code{corStruct}
   classes. If a grouping variable is to be used, it must be specified
   in the \code{form} argument to the \code{corStruct}
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
 \item{na.action}{a function that indicates what should happen when the
   data contain \code{NA}s.  The default action (\code{na.fail}) causes
   \code{gnls} to print an error message and terminate if there are any
   incomplete observations.}
 \item{naPattern}{an expression or formula object, specifying which returned
   values are to be regarded as missing.}
 \item{control}{a list of control values for the estimation algorithm to
   replace the default values returned by the function \code{gnlsControl}.
   Defaults to an empty list.}
 \item{verbose}{an optional logical value. If \code{TRUE} information on
   the evolution of the iterative algorithm is printed. Default is
   \code{FALSE}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This function fits a nonlinear model using generalized least
  squares. The errors are allowed to be correlated and/or have unequal
  variances.  
}
\value{
  an object of class \code{gnls}, also inheriting from class \code{gls},
  representing the nonlinear model fit. Generic functions such as
  \code{print}, \code{plot} and  \code{summary} have methods to show the
  results of the fit. See \code{gnlsObject} for the components of the
  fit. The functions \code{resid}, \code{coef}, and \code{fitted} can be
  used to extract some of its components.  
}
\references{
 The different correlation structures available for the
 \code{correlation} argument are described in Box, G.E.P., Jenkins,
 G.M., and Reinsel G.C. (1994), Littel, R.C., Milliken, G.A., Stroup,
 W.W., and Wolfinger, R.D. (1996), and Venables, W.N. and Ripley,
 B.D. (1997). The use of variance functions for linear 
 and nonlinear models is presented in detail in Carrol, R.J. and Rupert,
 D. (1988) and Davidian, M. and Giltinan, D.M. (1995).  

 Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
 Analysis: Forecasting and Control", 3rd Edition, Holden-Day. 

 Carrol, R.J. and Rupert, D. (1988) "Transformation and Weighting in
 Regression", Chapman and Hall.

 Davidian, M. and Giltinan, D.M. (1995) "Nonlinear Mixed Effects Models
 for Repeated Measurement Data", Chapman and Hall.

 Littel, R.C., Milliken, G.A., Stroup, W.W., and Wolfinger, R.D. (1996)
 "SAS Systems for Mixed Models", SAS Institute.

 Venables, W.N. and Ripley, B.D. (1997) "Modern Applied Statistics with
 S-plus", 2nd Edition, Springer-Verlag.

 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  

}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{corClasses}},
  \code{\link{gnlsControl}}, \code{\link{gnlsObject}},
  \code{\link{gnlsStruct}},
  \code{\link{predict.gnls}},
  \code{\link{varClasses}},
  \code{\link{varFunc}}
}
\examples{
# variance increases with a power of the absolute fitted values
fm1 <- gnls(weight ~ SSlogis(Time, Asym, xmid, scal), Soybean,
            weights = varPower())
summary(fm1)
}
\keyword{models}
