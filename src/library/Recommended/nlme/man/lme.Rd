% File nlme/man/lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE
\name{lme}
\title{Linear Mixed-Effects Models}
\usage{
lme(fixed, data, random, correlation, weights, subset, method,
    na.action, control, contrasts = NULL, keep.data = TRUE)
\method{update}{lme}(object, fixed., \dots, evaluate = TRUE)
}
\alias{lme}
\alias{lme.formula}
\alias{update.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
  \item{fixed}{a two-sided linear formula object describing the
    fixed-effects part of the model, with the response on the left of a
    \code{~} operator and the terms, separated by \code{+} operators, on
    the right, an \code{lmList} object, or a \code{groupedData}
    object. The method functions \code{lme.lmList} and
    \code{lme.groupedData} are documented separately.}
 \item{fixed.}{Changes to the fixed-effects formula -- see
   \code{update.formula} for details.}
 \item{data}{an optional data frame containing the variables named in
   \code{fixed}, \code{random}, \code{correlation}, \code{weights}, and
   \code{subset}.  By default the variables are taken from the
   environment from which \code{lme} is called.}
 \item{random}{optionally, any of the following: (i) a one-sided formula
   of the form \code{~ x1 + ... + xn | g1/.../gm}, with \code{x1 + ... + xn}
   specifying the model for the random effects and \code{g1/.../gm} the
   grouping structure (\code{m} may be equal to 1, in which case no
   \code{/} is required). The random effects formula will be repeated
   for all levels of grouping, in the case of multiple levels of
   grouping; (ii) a list of one-sided formulas of the form
   \code{~ x1 + ... + xn | g}, with possibly different random effects models
   for each grouping level. The order of nesting will be assumed the
   same as the order of the elements in the list; (iii) a one-sided
   formula of the form \code{~ x1 + ... + xn}, or a \code{pdMat} object with
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
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
 \item{evaluate}{If \code{TRUE} evaluate the new call else return the call.}
}
\description{
  This generic function fits a linear mixed-effects model in the
  formulation described in Laird and Ware (1982) but allowing for nested
  random effects. The within-group errors are allowed to be correlated
  and/or have unequal variances. 
}
\value{
  an object of class \code{lme} representing the linear mixed-effects
  model fit. Generic functions such as \code{print}, \code{plot} and
  \code{summary} have methods to show the results of the fit. See
  \code{lmeObject} for the components of the fit. The functions
  \code{resid}, \code{coef}, \code{fitted}, \code{fixed.effects}, and
  \code{random.effects}  can be used to extract some of its components.
}
\note{
  The function does not do any scaling internally: the optimization will
  work best when the response is scaled so its variance is of the order
  of one.
}
\references{
 
  The computational methods follow the general framework of Lindstrom
  and Bates (1988). The model formulation is described in Laird and Ware
  (1982).  The variance-covariance parametrizations are described in
  Pinheiro and Bates (1996).  The different correlation structures
  available for the \code{correlation} argument are described in Box,
  Jenkins and Reinse (1994), Littel \emph{et al} (1996), and Venables and
  Ripley, (2002). The use of variance functions for linear and nonlinear
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

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  

  Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
  S", 4th Edition, Springer-Verlag.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{corClasses}},
  \code{\link{lme.lmList}},
  \code{\link{lme.groupedData}},
  \code{\link{lmeControl}},
  \code{\link{lmeObject}},
  \code{\link{lmeStruct}},
  \code{\link{lmList}},
  \code{\link{pdClasses}},
  \code{\link{plot.lme}},
  \code{\link{predict.lme}},
  \code{\link{qqnorm.lme}},
  \code{\link{residuals.lme}},
  \code{\link{reStruct}},
  \code{\link{simulate.lme}},
  \code{\link{summary.lme}},
  \code{\link{varClasses}}, 
  \code{\link{varFunc}}
}
\examples{
fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)
}
\keyword{models}
