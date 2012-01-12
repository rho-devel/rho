% File nlme/man/predict.nlme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: predict.nlme.Rd,v 1.9 2002/03/05 14:59:39 bates Exp $
\name{predict.nlme}
\title{Predictions from an nlme Object}
\usage{
\method{predict}{nlme}(object, newdata, level, asList, na.action,
naPattern, \dots)
}
\alias{predict.nlme}
\arguments{
 \item{object}{an object inheriting from class \code{nlme}, representing
   a fitted nonlinear mixed-effects model.}
 \item{newdata}{an optional data frame to be used for obtaining the
   predictions. All variables used in the nonlinear model, the fixed and
   the random effects models, as well as the grouping factors, must be
   present in the data frame. If missing, the fitted values are returned.}
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
 \item{naPattern}{an expression or formula object, specifying which returned
   values are to be regarded as missing.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The predictions at level \eqn{i} are obtained by adding together the
  contributions from the estimated fixed effects and the estimated
  random effects at levels less or equal to \eqn{i} and evaluating the
  model function at the resulting estimated parameters. If group values
  not included in the original grouping factors  are present in
  \code{newdata}, the corresponding predictions will be  set to
  \code{NA} for levels greater or equal to the level at which the 
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
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{nlme}}, \code{\link{fitted.lme}}}
\examples{
fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
newLoblolly <- data.frame(age = c(5,10,15,20,25,30),
                    Seed = rep(301,6))
predict(fm1, newLoblolly, level = 0:1)
}
\keyword{models}
