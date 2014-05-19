% File nlme/man/augPred.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{augPred}
\title{Augmented Predictions}
\usage{
augPred(object, primary, minimum, maximum, length.out, \dots)
%% extra argument 'level':
\S3method{augPred}{lme}(object, primary = NULL,
        minimum = min(primary), maximum = max(primary),
        length.out = 51, level = Q, \dots)
}
\alias{augPred}
\alias{augPred.gls}
\alias{augPred.lme}
\alias{augPred.lmList}
\arguments{
 \item{object}{a fitted model object from which predictions can be
   extracted, using a \code{predict} method.}
 \item{primary}{an optional one-sided formula specifying the primary
   covariate to be used to generate the augmented predictions. By
   default, if a  covariate can be extracted from the data used to generate
   \code{object} (using \code{getCovariate}), it will be used as
   \code{primary}.}
 \item{minimum}{an optional lower limit for the primary
   covariate. Defaults to \code{min(primary)}.}
 \item{maximum}{an optional upper limit for the primary
   covariate. Defaults to \code{max(primary)}.}
 \item{length.out}{an optional integer with the number of primary
   covariate values at which to evaluate the predictions.  Defaults to
   51.}
 \item{level}{an optional integer vector specifying the desired
   prediction levels.  Levels increase from outermost to innermost
   grouping, with level 0 representing the population (fixed effects)
   predictions.  Defaults to the innermost level.}
 \item{\dots}{some methods for the generic may require additional
   arguments.}
}
\description{
  Predicted values are obtained at the specified values of
  \code{primary}.  If \code{object} has a grouping structure
  (i.e. \code{getGroups(object)} is not \code{NULL}), predicted values
  are obtained for each group.  If \code{level} has more than one
  element, predictions are obtained for each level of the
  \code{max(level)} grouping factor.  If other covariates besides
  \code{primary} are used in the prediction model, their average
  (numeric covariates) or most frequent value (categorical covariates)
  are used to obtain the predicted values.  The original observations
  are also included in the returned object.
}
\value{
  a data frame with four columns representing, respectively, the values
  of the primary covariate, the groups (if \code{object} does not have a
  grouping structure, all elements will be \code{1}), the predicted or
  observed values, and the type of value in the third column:
  \code{original} for the observed values and \code{predicted} (single
  or no grouping factor) or \code{predict.groupVar} (multiple levels of
  grouping), with \code{groupVar} replaced by the actual grouping
  variable name (\code{fixed} is used for population predictions). The
  returned object inherits from class \code{augPred}.
}
\references{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include: \code{gls}, \code{lme}, and \code{lmList}.
}
\seealso{\code{\link{plot.augPred}}, \code{\link{getGroups}},
  \code{\link{predict}}}

\examples{
fm1 <- lme(Orthodont, random = ~1)
augPred(fm1, length.out = 2, level = c(0,1))
}
\keyword{models}
