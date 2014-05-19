% File nlme/man/lme.groupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: lme.groupedData.Rd,v 1.8.2.1 2003/01/30 18:35:14 bates Exp $
\name{lme.groupedData}
\title{LME fit from groupedData Object}
\usage{
\method{lme}{groupedData}(fixed, data, random, correlation, weights, 
    subset, method, na.action, control, contrasts, keep.data = TRUE)
}
\alias{lme.groupedData}
\arguments{
  \item{fixed}{a data frame inheriting from class \code{groupedData}.}
  \item{data}{this argument is included for consistency with the generic
    function. It is ignored in this method function.}
 \item{random}{optionally, any of the following: (i) a one-sided formula
   of the form \code{~x1+...+xn | g1/.../gm}, with \code{x1+...+xn}
   specifying the model for the random effects and \code{g1/.../gm} the
   grouping structure (\code{m} may be equal to 1, in which case no
   \code{/} is required). The random effects formula will be repeated
   for all levels of grouping, in the case of multiple levels of
   grouping; (ii) a list of one-sided formulas of the form
   \code{~x1+...+xn | g}, with possibly different random effects models
   for each grouping level. The order of nesting will be assumed the
   same as the order of the elements in the list; (iii) a one-sided
   formula of the form \code{~x1+...+xn}, or a \code{pdMat} object with
   a formula (i.e. a non-\code{NULL} value for \code{formula(object)}),
   or a list of such formulas or \code{pdMat} objects. In this case, the
   grouping structure formula will be derived from the data used to
   fit the linear mixed-effects model, which should inherit from class
   \code{groupedData}; (iv) a named list of formulas or \code{pdMat}
   objects as in (iii), with the grouping factors as names. The order of
   nesting will be assumed the same as the order of the order of the
   elements in the list; (v) an \code{reStruct} object. See the
   documentation on \code{pdClasses} for a description of the available
   \code{pdMat} classes. Defaults to a formula consisting of the right
   hand side of \code{fixed}.}  
 \item{correlation}{an optional \code{corStruct} object describing the
   within-group correlation structure. See the documentation of
   \code{corClasses} for a description of the available \code{corStruct}
   classes. Defaults to \code{NULL},
   corresponding to no within-group correlations.}
 \item{weights}{an optional \code{varFunc} object or one-sided formula
   describing the within-group heteroscedasticity structure. If given as
   a formula, it is used as the argument to \code{varFixed},
   corresponding to fixed variance weights. See the documentation on
   \code{varClasses} for a description of the available \code{varFunc}
   classes. Defaults to \code{NULL}, corresponding to homoscedastic
   within-group errors.} 
 \item{subset}{an optional expression indicating the subset of the rows of
   \code{data} that should be used in the fit. This can be a logical
   vector, or a numeric vector indicating which observation numbers are
   to be included, or a  character  vector of the row names to be
   included.  All observations are included by default.}
 \item{method}{a character string.  If \code{"REML"} the model is fit by
   maximizing the restricted log-likelihood.  If \code{"ML"} the
   log-likelihood is maximized.  Defaults to \code{"REML"}.}
 \item{na.action}{a function that indicates what should happen when the
   data contain \code{NA}s.  The default action (\code{na.fail}) causes
   \code{lme} to print an error message and terminate if there are any
   incomplete observations.}
 \item{control}{a list of control values for the estimation algorithm to
   replace the default values returned by the function \code{lmeControl}.
   Defaults to an empty list.}
 \item{contrasts}{an optional list. See the \code{contrasts.arg}
   of \code{model.matrix.default}.}
 \item{keep.data}{logical: should the \code{data} argument (if supplied
   and a data frame) be saved as part of the model object?} 
}
\description{
  The response variable and primary covariate in \code{formula(fixed)}
  are used to construct the fixed effects model formula. This formula
  and the   \code{groupedData} object are passed as the \code{fixed} and
  \code{data} arguments to \code{lme.formula}, together with any other
  additional arguments in the function call. See the documentation on
  \code{lme.formula} for a description of that function.
}
\value{
  an object of class \code{lme} representing the linear mixed-effects
  model fit. Generic functions such as \code{print}, \code{plot} and
  \code{summary} have methods to show the results of the fit. See
  \code{lmeObject} for the components of the fit. The functions
  \code{resid}, \code{coef}, \code{fitted}, \code{fixed.effects}, and
  \code{random.effects}  can be used to extract some of its components.
}
\references{
 The computational methods follow on the general framework of Lindstrom,
 M.J. and Bates, D.M. (1988). The model formulation is described in
 Laird, N.M. and Ware, J.H. (1982).  The variance-covariance
 parametrizations are described in Pinheiro, J.C. and Bates., D.M.
 (1996).  The different correlation structures available for the
 \code{correlation} argument are described in Box, G.E.P., Jenkins,
 G.M., and Reinsel G.C. (1994), Littel, R.C., Milliken, G.A., Stroup,
 W.W., and Wolfinger, R.D. (1996), and Venables, W.N. and Ripley,
 B.D. (2002). The use of variance functions for linear and nonlinear
 mixed effects models is presented in detail in Davidian, M. and
 Giltinan, D.M. (1995).

 Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
 Analysis: Forecasting and Control", 3rd Edition, Holden-Day. 

 Davidian, M. and Giltinan, D.M. (1995) "Nonlinear Mixed Effects Models
 for Repeated Measurement Data", Chapman and Hall.

 Laird, N.M. and Ware, J.H. (1982) "Random-Effects Models for
 Longitudinal Data", Biometrics, 38, 963-974.  

 Lindstrom, M.J. and Bates, D.M. (1988) "Newton-Raphson and EM
 Algorithms for Linear Mixed-Effects Models for Repeated-Measures
 Data", Journal of the American Statistical Association, 83,
 1014-1022. 

 Littel, R.C., Milliken, G.A., Stroup, W.W., and Wolfinger, R.D. (1996)
 "SAS Systems for Mixed Models", SAS Institute.

 Pinheiro, J.C. and Bates., D.M.  (1996) "Unconstrained
 Parametrizations for Variance-Covariance Matrices", Statistics and
 Computing, 6, 289-296.

 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
 
 Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
 S", 4th Edition, Springer-Verlag.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}, \code{\link{groupedData}},
  \code{\link{lmeObject}} } 

\examples{
fm1 <- lme(Orthodont)
summary(fm1)
}
\keyword{models}
