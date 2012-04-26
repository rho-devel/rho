% file MASS/man/predict.qda.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{predict.qda}
\alias{predict.qda}
\title{
Classify from Quadratic Discriminant Analysis
}
\description{
Classify multivariate observations in conjunction with \code{qda}
}
\usage{
\method{predict}{qda}(object, newdata, prior = object$prior,
        method = c("plug-in", "predictive", "debiased", "looCV"), \dots)
}
\arguments{
\item{object}{
object of class \code{"qda"}
}
\item{newdata}{
  data frame of cases to be classified or, if \code{object}
  has a formula, a data frame with columns of the same names as the
  variables used.  A vector will be interpreted
  as a row vector.  If newdata is missing, an attempt will be
  made to retrieve the data used to fit the \code{qda} object.
}
\item{prior}{
The prior probabilities of the classes, by default the proportions in the
training set or what was set in the call to \code{qda}.
}
\item{method}{
This determines how the parameter estimation is handled. With \code{"plug-in"}
(the default) the usual unbiased parameter estimates are used and
assumed to be correct. With \code{"debiased"} an unbiased estimator of
the log posterior probabilities is used, and with \code{"predictive"} the
parameter estimates are integrated out using a vague prior.  With
\code{"looCV"} the leave-one-out cross-validation fits to the original
dataset are computed and returned.
}
\item{\dots}{
arguments based from or to other methods
}}
\value{
a list with components

\item{class}{
The MAP classification (a factor)
}
\item{posterior}{
posterior probabilities for the classes
}}
\details{
This function is a method for the generic function
\code{predict()} for class \code{"qda"}.
It can be invoked by calling \code{predict(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{predict.qda(x)} regardless of the
class of the object.

Missing values in \code{newdata} are handled by returning \code{NA} if the
quadratic discriminants cannot be evaluated. If \code{newdata} is omitted and
the \code{na.action} of the fit omitted cases, these will be omitted on the
prediction.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks}. Cambridge University Press.
}
\seealso{
\code{\link{qda}}, \code{\link{lda}}, \code{\link{predict.lda}}
}
\examples{
tr <- sample(1:50, 25)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
zq <- qda(train, cl)
predict(zq, test)$class
}
\keyword{multivariate}
