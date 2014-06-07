% File nlme/man/nlme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlme.Rd,v 1.8.2.3 2003/06/12 14:58:30 bates Exp $
\name{nlme}
\title{Nonlinear Mixed-Effects Models}
\usage{
nlme(model, data, fixed, random, groups, start, correlation, weights,
     subset, method, na.action, naPattern, control, verbose)
}
\alias{nlme}
\alias{nlme.formula}
\arguments{
  \item{model}{a nonlinear model formula, with the response on the left
    of a \code{~} operator and an expression involving parameters and
    covariates on the right, or an \code{nlsList} object.  If
    \code{data} is given, all names used in the formula should be
    defined as parameters or variables in the data frame. The method
    function \code{nlme.nlsList} is documented separately.} 
 \item{data}{an optional data frame containing the variables named in
   \code{model}, \code{fixed}, \code{random}, \code{correlation},
   \code{weights}, \code{subset}, and \code{naPattern}.  By default the
   variables are taken from the environment from which \code{nlme} is
   called.} 
 \item{fixed}{a two-sided linear formula of the form
   \code{f1+...+fn~x1+...+xm}, or a list of two-sided formulas of the form
   \code{f1~x1+...+xm}, with possibly different models for different
   parameters. The \code{f1,...,fn} are the names of parameters included on
   the right hand side of \code{model} and the \code{x1+...+xm}
   expressions define linear models for these parameters (when the left
   hand side of the formula contains several parameters, they all are
   assumed to follow the same linear model, described by the right hand
   side expression).
   A \code{1} on the right hand side of the formula(s) indicates a single
   fixed effects for the corresponding parameter(s).}
 \item{random}{optionally, any of the following: (i) a two-sided formula
   of the form \code{r1+...+rn~x1+...+xm | g1/.../gQ}, with
   \code{r1,...,rn} naming parameters included on the right
   hand side of \code{model}, \code{x1+...+xm} specifying the
   random-effects model for
   these parameters and \code{g1/.../gQ} the grouping structure
   (\code{Q} may be equal to 1, in which case no \code{/} is
   required). The random effects formula will be repeated 
   for all levels of grouping, in the case of multiple levels of
   grouping; (ii) a two-sided formula of the form
   \code{r1+...+rn~x1+..+xm}, a list of two-sided formulas of the form
   \code{r1~x1+...+xm}, with possibly different random-effects models
   for different parameters, a \code{pdMat} object with a two-sided
   formula, or list of two-sided formulas (i.e. a non-\code{NULL} value for
   \code{formula(random)}), or a list of pdMat objects with two-sided
   formulas, or lists of two-sided formulas. In this case, the grouping
   structure formula will be given in \code{groups}, or derived from the
   data used to fit the nonlinear mixed-effects model, which should
   inherit from class  \code{groupedData},; (iii) a named list
   of formulas, lists of formulas, or \code{pdMat} objects as in (ii),
   with the grouping factors as names. The order of nesting will be
   assumed the same as the order of the order of the elements in the
   list; (iv) an \code{reStruct} object. See the documentation on
   \code{pdClasses} for a description of the available \code{pdMat}
   classes. Defaults to \code{fixed}, 
   resulting in all fixed effects having also random effects.}
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
  This generic function fits a nonlinear mixed-effects model in the
  formulation described in Lindstrom and Bates (1990) but allowing for nested
  random effects. The within-group errors are allowed to be correlated
  and/or have unequal variances. 
}
\value{
  an object of class \code{nlme} representing the nonlinear
  mixed-effects model fit. Generic functions such as \code{print},
  \code{plot} and \code{summary} have methods to show the results of the
  fit. See \code{nlmeObject} for the components of the fit. The functions
  \code{resid}, \code{coef}, \code{fitted}, \code{fixed.effects}, and
  \code{random.effects}  can be used to extract some of its components.
}
\note{
  The function does not do any scaling internally: the optimization will
  work best when the response is scaled so its variance is of the order
  of one.
}
\references{
 The model formulation and computational methods are described in
 Lindstrom, M.J. and Bates, D.M. (1990). The variance-covariance
 parametrizations are described in Pinheiro, J.C. and Bates., D.M.
 (1996).   The different correlation structures available for the
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

 Littel, R.C., Milliken, G.A., Stroup, W.W., and Wolfinger, R.D. (1996)
 "SAS Systems for Mixed Models", SAS Institute.

 Lindstrom, M.J. and Bates, D.M. (1990) "Nonlinear Mixed Effects Models
 for Repeated Measures Data", Biometrics, 46, 673-687.

 Pinheiro, J.C. and Bates., D.M.  (1996) "Unconstrained
 Parametrizations for Variance-Covariance Matrices", Statistics and
 Computing, 6, 289-296.

 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  

 Venables, W.N. and Ripley, B.D. (2002) "Modern Applied Statistics with
 S", 4th Edition, Springer-Verlag.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{nlmeControl}}, \code{\link{nlme.nlsList}},
  \code{\link{nlmeObject}}, \code{\link{nlsList}},
  \code{\link{nlmeStruct}},
  \code{\link{pdClasses}},
  \code{\link{reStruct}}, \code{\link{varFunc}},
  \code{\link{corClasses}}, \code{\link{varClasses}}
}
\examples{
fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(fm1)
fm2 <- update(fm1, random = pdDiag(Asym + lrc ~ 1))
summary(fm2)
}
\keyword{models}
