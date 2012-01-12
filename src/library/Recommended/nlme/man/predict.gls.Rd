% File nlme/man/predict.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: predict.gls.Rd,v 1.8.2.1 2003/05/17 21:59:38 bates Exp $
\name{predict.gls}
\title{Predictions from a gls Object}
\usage{
\method{predict}{gls}(object, newdata, na.action, \dots)
}
\alias{predict.gls}
\arguments{
 \item{object}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted linear model.}
 \item{newdata}{an optional data frame to be used for obtaining the
   predictions. All variables used in the linear model must be present
   in the data frame. If missing, the fitted values are returned.}
 \item{na.action}{a function that indicates what should happen when
   \code{newdata} contains \code{NA}s.  The default action
   (\code{na.fail}) causes the function to print an error message and
   terminate if there are any incomplete observations.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The predictions for the linear model represented by \code{object} are
  obtained at the covariate values defined in \code{newdata}.
}
\value{
  a vector with the predicted values.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gls}}}

\examples{
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
newOvary <- data.frame(Time = c(-0.75, -0.5, 0, 0.5, 0.75))
predict(fm1, newOvary)
}
\keyword{models}
