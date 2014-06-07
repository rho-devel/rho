% file MASS/man/predict.lda.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{predict.lda}
\alias{predict.lda}
\title{
Classify Multivariate Observations by Linear Discrimination
}
\description{
Classify multivariate observations in conjunction with \code{lda}, and also
project data onto the linear discriminants.
}
\usage{
\method{predict}{lda}(object, newdata, prior = object$prior, dimen,
        method = c("plug-in", "predictive", "debiased"), \dots)
}
\arguments{
\item{object}{
object  of class \code{"lda"}
}
\item{newdata}{
  data frame of cases to be classified or, if \code{object}
  has a formula, a data frame with columns of the same names as the
  variables used.  A vector will be interpreted
  as a row vector.  If newdata is missing, an attempt will be
  made to retrieve the data used to fit the \code{lda} object.
}
\item{prior}{
The prior probabilities of the classes, by default the proportions in the
training set or what was set in the call to \code{lda}.
}
\item{dimen}{
the dimension of the space to be used. If this is less than \code{min(p, ng-1)},
only the first \code{dimen} discriminant components are used (except for
\code{method="predictive"}), and only those dimensions are returned in \code{x}.
}
\item{method}{
This determines how the parameter estimation is handled. With \code{"plug-in"}
(the default) the usual unbiased parameter estimates are used and
assumed to be correct. With \code{"debiased"} an unbiased estimator of
the log posterior probabilities is used, and with \code{"predictive"} the
parameter estimates are integrated out using a vague prior.
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
}
\item{x}{
the scores of test cases on up to \code{dimen} discriminant variables
}}
\details{
This function is a method for the generic function \code{predict()} for
class \code{"lda"}.  It can be invoked by calling \code{predict(x)} for
an object \code{x} of the appropriate class, or directly by calling
\code{predict.lda(x)} regardless of the class of the object.

Missing values in \code{newdata} are handled by returning \code{NA} if the
linear discriminants cannot be evaluated. If \code{newdata} is omitted and
the \code{na.action} of the fit omitted cases, these will be omitted on the
prediction.

This version centres the linear discriminants so that the
weighted mean (weighted by \code{prior}) of the group centroids is at
the origin.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks}. Cambridge University Press.
}
\seealso{
\code{\link{lda}}, \code{\link{qda}}, \code{\link{predict.qda}}
}
\examples{
tr <- sample(1:50, 25)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
z <- lda(train, cl)
predict(z, test)$class
}
\keyword{multivariate}
