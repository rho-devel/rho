% File nlme/man/predict.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: predict.lme.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{predict.lme}
\title{Predictions from an lme Object}
\usage{
\method{predict}{lme}(object, newdata, level, asList, na.action, \dots)
}
\alias{predict.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{newdata}{an optional data frame to be used for obtaining the
   predictions. All variables used in the fixed and random effects
   models, as well as the grouping factors, must be present in the data
   frame. If missing, the fitted values are returned.}
 \item{level}{an optional integer vector giving the level(s) of grouping
   to be used in obtaining the predictions. Level values increase from
   outermost to innermost grouping, with level zero corresponding to the
   population predictions. Defaults to the highest or innermost level of
   grouping.}  
 \item{asList}{an optional logical value. If \code{TRUE} and a single
   value is given in \code{level}, the returned object is a list with
   the predictions split by groups; else the returned value is
   either a vector or a data frame, according to the length of
   \code{level}. }
 \item{na.action}{a function that indicates what should happen when
   \code{newdata} contains \code{NA}s.  The default action
   (\code{na.fail}) causes the function to print an error message and
   terminate if there are any incomplete observations.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The predictions at level \eqn{i} are obtained by adding together the
  population predictions (based only on the fixed effects estimates)
  and the estimated contributions of the random effects to the
  predictions at grouping levels less or equal to \eqn{i}. The resulting
  values estimate the best linear unbiased predictions (BLUPs) at level
  \eqn{i}. If group values not included in the original grouping factors
  are present in \code{newdata}, the corresponding predictions will be
  set to \code{NA} for levels greater or equal to the level at which the
  unknown groups occur.
}
\value{
  if a single level of grouping is specified in \code{level}, the
  returned value is either a list with the predictions split by groups
  (\code{asList = TRUE}) or a vector with the predictions
  (\code{asList = FALSE}); else, when multiple grouping levels are
  specified in \code{level}, the returned object is a data frame with
  columns given by the predictions at different levels and the grouping
  factors. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}, \code{\link{fitted.lme}}}

\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
newOrth <- data.frame(Sex = c("Male","Male","Female","Female","Male","Male"),
                      age = c(15, 20, 10, 12, 2, 4),
                      Subject = c("M01","M01","F30","F30","M04","M04"))
predict(fm1, newOrth, level = 0:1)
}
\keyword{models}
