% File nlme/man/comparePred.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: comparePred.Rd,v 1.5.2.1 2003/04/17 22:28:45 bates Exp $
\name{comparePred}
\title{Compare Predictions}
\usage{
comparePred(object1, object2, primary, minimum, maximum,
    length.out, level, \dots)
}
\alias{comparePred}
\alias{comparePred.gls}
\alias{comparePred.lme}
\alias{comparePred.lmList}
\arguments{
 \item{object1,object2}{fitted model objects, from which predictions can
   be extracted using the \code{predict} method.}
 \item{primary}{an optional one-sided formula specifying the primary
   covariate to be used to generate the augmented predictions. By
   default, if a  covariate can be extracted from the data used to generate
   the objects (using \code{getCovariate}), it will be used as
   \code{primary}.}
 \item{minimum}{an optional lower limit for the primary
   covariate. Defaults to \code{min(primary)}, after \code{primary} is 
   evaluated in the \code{data} used in fitting \code{object1}.}
 \item{maximum}{an optional upper limit for the primary
   covariate. Defaults to \code{max(primary)}, after \code{primary} is
   evaluated in the \code{data} used in fitting \code{object1}.}
 \item{length.out}{an optional integer with the number of primary
   covariate values at which to evaluate the predictions. Defaults to
   51.}
 \item{level}{an optional integer specifying the desired
   prediction level. Levels increase from outermost to innermost
   grouping, with level 0 representing the population (fixed effects)
   predictions. Only one level can be specified. Defaults to the
   innermost level.}
 \item{\dots}{some methods for the generic may require additional
   arguments.}
}
\description{
  Predicted values are obtained at the specified values of
  \code{primary} for each object. If either \code{object1} or
  \code{object2} have a grouping structure
  (i.e. \code{getGroups(object)} is not \code{NULL}), predicted values
  are obtained for each group. When both objects determine groups, the
  group levels must be the same. If other covariates besides
  \code{primary} are used in the prediction model, their group-wise averages
  (numeric covariates) or most frequent values (categorical covariates)
  are used to obtain the predicted values. The original observations are
  also included in the returned object. 
}
\value{
  a data frame with four columns representing, respectively, the values
  of the primary covariate, the groups (if \code{object} does not have a
  grouping structure, all elements will be \code{1}), the predicted or
  observed values, and the type of value in the third column: the
  objects' names are used to classify the predicted values and
  \code{original} is used for the observed values. The returned object
  inherits from classes \code{comparePred} and \code{augPred}.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include: \code{gls}, \code{lme}, and \code{lmList}.
}
\seealso{\code{\link{augPred}}, \code{\link{getGroups}}}
\examples{
fm1 <- lme(distance ~ age * Sex, data = Orthodont, random = ~ age)
fm2 <- update(fm1, distance ~ age)
comparePred(fm1, fm2, length.out = 2)
}
\keyword{models}
