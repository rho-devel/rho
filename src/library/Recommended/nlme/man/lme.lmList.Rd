% File nlme/man/lme.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{lme.lmList}
\title{LME fit from lmList Object}
\usage{
\method{lme}{lmList}(fixed, data, random, correlation, weights, subset, method,
    na.action, control, contrasts, keep.data)
}
\alias{lme.lmList}
\arguments{
  \item{fixed}{an object inheriting from class \code{lmList},
    representing a list of \code{lm} fits with a common model.}
  \item{data}{this argument is included for consistency with the generic
    function. It is ignored in this method function.}
 \item{random}{an optional one-sided linear formula with no conditioning
   expression, or a \code{pdMat} object with a \code{formula}
   attribute. Multiple levels of grouping are not allowed with this
   method function.  Defaults to a formula consisting of the right hand
   side of \code{formula(fixed)}.} 
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
  If the random effects names defined in \code{random} are a subset of
  the \code{lmList} object coefficient names, initial estimates for the
  covariance matrix of the random effects are obtained (overwriting any
  values given in \code{random}). \code{formula(fixed)} and the
  \code{data} argument in the calling sequence used to obtain
  \code{fixed} are passed as the \code{fixed} and \code{data} arguments
  to \code{lme.formula}, together with any other additional arguments in
  the function call. See the documentation on \code{lme.formula} for a
  description of that function. 
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
 
  The computational methods follow the general framework of Lindstrom
  and Bates (1988). The model formulation is described in Laird and Ware
  (1982).  The variance-covariance parametrizations are described in
  Pinheiro and Bates (1996).  The different correlation structures
  available for the \code{correlation} argument are described in Box,
  Jenkins and Reinse (1994), Littel \emph{et al} (1996), and Venables and
  Ripley, (1997). The use of variance functions for linear and nonlinear
  mixed effects models is presented in detail in Davidian and Giltinan
  (1995).

  Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
  Analysis: Forecasting and Control", 3rd Edition, Holden--Day. 

  Davidian, M. and Giltinan, D.M. (1995) "Nonlinear Mixed Effects Models
  for Repeated Measurement Data", Chapman and Hall.

  Laird, N.M. and Ware, J.H. (1982) "Random-Effects Models for
  Longitudinal Data", Biometrics, 38, 963--974.  

  Lindstrom, M.J. and Bates, D.M. (1988) "Newton-Raphson and EM
  Algorithms for Linear Mixed-Effects Models for Repeated-Measures
  Data", Journal of the American Statistical Association, 83,
  1014--1022. 

  Littel, R.C., Milliken, G.A., Stroup, W.W., and Wolfinger, R.D. (1996)
  "SAS Systems for Mixed Models", SAS Institute.

  Pinheiro, J.C. and Bates., D.M.  (1996) "Unconstrained
  Parametrizations for Variance-Covariance Matrices", Statistics and
  Computing, 6, 289--296.

  Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
  S", 4th Edition, Springer-Verlag.
}

\author{
  Jose Pinheiro and Douglas
  Bates \email{bates@stat.wisc.edu}
}

\seealso{\code{\link{lme}}, \code{\link{lmList}},
  \code{\link{lmeObject}} } 

\examples{
fm1 <- lmList(Orthodont)
fm2 <- lme(fm1)
summary(fm1)
summary(fm2)
}
\keyword{models}
