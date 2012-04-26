\name{predict.glmmPQL}
\alias{predict.glmmPQL}
\title{Predict Method for glmmPQL Fits}
\description{
  Obtains predictions from a fitted generalized linear model
  with random effects.
}
\usage{
\method{predict}{glmmPQL}(object, newdata = NULL, type = c("link", "response"),
       level, na.action = na.pass, ...)
}
\arguments{
  \item{object}{a fitted object of class inheriting from \code{"glmmPQL"}.}
  \item{newdata}{optionally, a data frame in which to look for variables with
    which to predict.}
  \item{type}{the type of prediction required.  The default is on the
    scale of the linear predictors; the alternative \code{"response"}
    is on the scale of the response variable.  Thus for a default
    binomial model the default predictions are of log-odds (probabilities
    on logit scale) and \code{type = "response"} gives the predicted
    probabilities.}
  \item{level}{an optional integer vector giving the level(s) of grouping
   to be used in obtaining the predictions. Level values increase from
   outermost to innermost grouping, with level zero corresponding to the
   population predictions. Defaults to the highest or innermost level of
   grouping.}  
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  If \code{level} is a single integer, a vector otherwise a data frame.
}
\seealso{
  \code{\link{glmmPQL}}, \code{\link[nlme]{predict.lme}}.
}
\examples{
fit <- glmmPQL(y ~ trt + I(week > 2), random = ~1 |  ID,
               family = binomial, data = bacteria)
predict(fit, bacteria, level = 0, type="response")
predict(fit, bacteria, level = 1, type="response")
}
\keyword{models}
