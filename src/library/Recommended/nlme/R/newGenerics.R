###    New generics used with corStruct, varFunc, groupedData, and reStruct
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

ACF <-
  ## autocorrelation function - needed not exist if acf were generic
  function(object, maxLag, ...) UseMethod("ACF")

asTable <-
  ## Return the object in a tabular form
  function(object) UseMethod("asTable")

augPred <-
  ## Return the data used to fit the model augmented with the predictions
  function(object, primary = NULL, minimum = min(primary),
           maximum = max(primary), length.out = 51, ...) UseMethod("augPred")

"coef<-" <- "coefficients<-" <-
  ## Assignment of the unconstrained parameter
  function(object, ..., value) UseMethod("coef<-")

collapse <-
  ## collapse a data frame according to a factor, or several nested factors
  function(object, ...) UseMethod("collapse")

comparePred <-
  ## compare predictions from different fitted objects
  function(object1, object2, primary = NULL,
	   minimum = min(primary), maximum = max(primary),
	   length.out = 51, level = NULL, ...) UseMethod("comparePred")

"covariate<-" <-
  ## Assignment of the primary covariate
  function(object, value) UseMethod("covariate<-")

Dim <-
  ## Extract dimensions of an object. Not needed if "dims" were generic
  function(object, ...) UseMethod("Dim")

fixed.effects <-
  ## Generic extractor for estimates of fixed effects
  function(object, ...) UseMethod("fixef")

fixef <-
  ## Short form for generic extractor for estimates of fixed effects
  function(object, ...) UseMethod("fixef")

getCovariate <-
  ## Return the primary covariate associated with object according to form
  function(object, form = formula(object), data)
    UseMethod("getCovariate")

getData <-
  ## Return the data.frame used to fit an object, if any was given in
  ## the call that produced it
  function(object) UseMethod("getData")

getGroups <-
  ## Return the groups associated with object according to form.
  function(object, form = formula(object), level, data, sep = "/")
    UseMethod("getGroups")

getGroupsFormula <-
  ## Return the formula(s) for the groups associated with object.
  ## The result is a one-sided formula unless asList is TRUE in which case
  ## it is a list of formulas, one for each level.
  function(object, asList = FALSE, sep = "/")
    UseMethod("getGroupsFormula")

getResponse <-
  ## Return the response associated with object according to form.
  function(object, form = formula(object))
    UseMethod("getResponse")

isBalanced <-
  ## Check for balance, especially in a groupedData object
  function(object, countOnly = FALSE, level) UseMethod("isBalanced")

isInitialized <-
  ## Determine if the object has been assigned a value
  function(object) UseMethod("isInitialized")

Initialize <-
  ## Initialize  objects
  function(object, data, ...) UseMethod("Initialize")

intervals <-
  ## generate confidence intervals for the parameters in object
  function(object, level = 0.95, ...) UseMethod("intervals")

logDet <-
  ## Returns the negative of the sum of the logarithm of the determinant
  function(object, ...) UseMethod("logDet")

"matrix<-" <-
  ## Assignment of the matrix in an object representing special types of matrices
  function(object, value) UseMethod("matrix<-")

Names <-
  ## Extract names of an object. Not needed if "names" were generic
  function(object, ...) UseMethod("Names")

"Names<-" <-
  ## Assignment of names. Not needed if "names<-" were generic
  function(object, ..., value) UseMethod("Names<-")

needUpdate <-
  ## Checks if model plug-in needs to be updated after an estimation cycle
  function(object) UseMethod("needUpdate")

#pruneLevels <-
#  ## Returns the factor with the levels attribute truncated to only those
#  ## levels occuring in the factor
#  function(object) UseMethod("pruneLevels")

random.effects <-
  ## Generic function for extracting the random effects
  ## If aug.frame is true, the returned data frame is augmented with
  ## values from the original data object, if available.  The variables
  ## in the original data are collapsed over the groups variable by the
  ## function fun.
  function(object, ...) UseMethod("ranef")

ranef <-
  ## Short form for generic function for extracting the random effects
  function(object, ...) UseMethod("ranef")

recalc <-
  ## Recalculate condensed linear object, according to model plug-in
  function(object, conLin, ...) UseMethod("recalc")

Variogram <-
  ## calculates variogram of a vector according to a distance matrix
  function(object, distance, ...)
  UseMethod("Variogram")

### Local variables:
### mode: S
### End:

