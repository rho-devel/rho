% File nlme/man/nlme.nlsList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlme.nlsList.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{nlme.nlsList}
\title{NLME fit from nlsList Object}
\usage{
\method{nlme}{nlsList}(model, data, fixed, random, groups, start, correlation, weights,
     subset, method, na.action, naPattern, control, verbose)
}
\alias{nlme.nlsList}
\arguments{
  \item{model}{an object inheriting from class \code{nlsList},
    representing a list of \code{nls} fits with a common model.}
  \item{data}{this argument is included for consistency with the generic
    function. It is ignored in this method function.}
  \item{fixed}{this argument is included for consistency with the generic
    function. It is ignored in this method function.}
  \item{random}{an optional one-sided linear formula with no conditioning
   expression, or a \code{pdMat} object with a \code{formula}
   attribute. Multiple levels of grouping are not allowed with this
   method function.  Defaults to a formula consisting of the right hand
   side of \code{formula(fixed)}.} 
 \item{groups}{an optional one-sided formula of the form \code{~g1}
   (single level of nesting) or \code{~g1/.../gQ} (multiple levels of
   nesting), specifying the partitions of the data over which the random
   effects vary. \code{g1,...,gQ} must evaluate to factors in
   \code{data}. The order of nesting, when multiple levels are present,
   is taken from left to right (i.e. \code{g1} is the first level,
   \code{g2} the second, etc.).}
 \item{start}{an optional numeric vector, or list of initial estimates
   for the fixed effects and random effects. If declared as a numeric
   vector, it is converted internally to a list with a single component
   \code{fixed}, given by the vector. The \code{fixed} component
   is required, unless the model function inherits from class
   \code{selfStart}, in which case initial values will be derived from a
   call to \code{nlsList}. An optional \code{random} component is used to specify
   initial values for the random effects and should consist of a matrix,
   or a list of matrices with length equal to the number of grouping
   levels. Each matrix should have as many rows as the number of groups
   at the corresponding level and as many columns as the number of
   random effects in that level.}
 \item{correlation}{an optional \code{corStruct} object describing the
   within-group correlation structure. See the documentation of
   \code{corClasses} for a description of the available \code{corStruct}
   classes. Defaults to \code{NULL}, corresponding to no within-group
   correlations.} 
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
   log-likelihood is maximized.  Defaults to \code{"ML"}.}
 \item{na.action}{a function that indicates what should happen when the
   data contain \code{NA}s.  The default action (\code{na.fail}) causes
   \code{nlme} to print an error message and terminate if there are any
   incomplete observations.}
 \item{naPattern}{an expression or formula object, specifying which returned
   values are to be regarded as missing.}
 \item{control}{a list of control values for the estimation algorithm to
   replace the default values returned by the function \code{nlmeControl}.
   Defaults to an empty list.}
 \item{verbose}{an optional logical value. If \code{TRUE} information on
   the evolution of the iterative algorithm is printed. Default is
   \code{FALSE}.}
}
\description{
  If the random effects names defined in \code{random} are a subset of
  the \code{lmList} object coefficient names, initial estimates for the
  covariance matrix of the random effects are obtained (overwriting any
  values given in \code{random}). \code{formula(fixed)} and the
  \code{data} argument in the calling sequence used to obtain
  \code{fixed} are passed as the \code{fixed} and \code{data} arguments
  to \code{nlme.formula}, together with any other additional arguments in
  the function call. See the documentation on \code{nlme.formula} for a
  description of that function. 
}
\value{
  an object of class \code{nlme} representing the linear mixed-effects
  model fit. Generic functions such as \code{print}, \code{plot} and
  \code{summary} have methods to show the results of the fit. See
  \code{nlmeObject} for the components of the fit. The functions
  \code{resid}, \code{coef}, \code{fitted}, \code{fixed.effects}, and
  \code{random.effects}  can be used to extract some of its components.
}
\references{
 The computational methods follow on the general framework of Lindstrom,
 M.J. and Bates, D.M. (1988). The model formulation is described in
 Laird, N.M. and Ware, J.H. (1982).  The variance-covariance
 parametrizations are described in <Pinheiro, J.C. and Bates., D.M.
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

 Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
 S", 4th Edition, Springer-Verlag.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{nlme}}, \code{\link{lmList}},
  \code{\link{nlmeObject}} } 

\examples{
fm1 <- nlsList(SSasymp, data = Loblolly)
fm2 <- nlme(fm1, random = Asym ~ 1)
summary(fm1)
summary(fm2)
}
\keyword{models}
