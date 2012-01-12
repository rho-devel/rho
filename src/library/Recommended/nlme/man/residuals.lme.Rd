% File nlme/man/residuals.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: residuals.lme.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{residuals.lme}
\title{Extract lme Residuals}
\usage{
\method{residuals}{lme}(object, level, type, asList, \dots)
}
\alias{residuals.lme}
\arguments{
  \item{object}{an object inheriting from class \code{lme}, representing
    a fitted linear mixed-effects model.}
  \item{level}{an optional integer vector giving the level(s) of grouping
    to be used in extracting the residuals from \code{object}. Level
    values increase from outermost to innermost grouping, with
    level zero corresponding to the population residuals. Defaults to
    the highest or innermost level of grouping.}
  \item{type}{an optional character string specifying the type of
    residuals to be used. If \code{"response"}, the "raw" residuals
    (observed - fitted) are used; else, if \code{"pearson"}, the
    standardized residuals (raw residuals divided by the corresponding
    standard errors) are used; else, if \code{"normalized"}, the
    normalized residuals (standardized residuals pre-multiplied by the
    inverse square-root factor of the estimated error correlation
    matrix) are used. Partial matching of arguments is used, so only the
    first character needs to be provided. Defaults to \code{"pearson"}.} 
  \item{asList}{an optional logical value. If \code{TRUE} and a single
    value is given in \code{level}, the returned object is a list with
    the residuals split by groups; else the returned value is
    either a vector or a data frame, according to the length of
    \code{level}. Defaults to \code{FALSE}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The residuals at level \eqn{i} are obtained by subtracting the fitted
  levels at that level from the response vector (and dividing
  by the estimated within-group standard error, if
  \code{type="pearson"}). The fitted values at level \eqn{i} are obtained
  by adding together the population fitted values (based only on the
  fixed effects estimates) and the estimated contributions of the random
  effects to the fitted values at grouping levels less or equal to
  \eqn{i}.  
}
\value{
  if a single level of grouping is specified in \code{level}, the
  returned value is either a list with the residuals split by groups
  (\code{asList = TRUE}) or a vector with the residuals
  (\code{asList = FALSE}); else, when multiple grouping levels are
  specified in \code{level}, the returned object is a data frame with
  columns given by the residuals at different levels and the grouping
  factors.  For a vector or data frame result the \code{\link{naresid}}
  method is applied.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}, \code{\link{fitted.lme}} }

\examples{
fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
residuals(fm1, level = 0:1)
}
\keyword{models}
