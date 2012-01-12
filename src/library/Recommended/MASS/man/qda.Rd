% file MASS/man/qda.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{qda}
\alias{qda}
\alias{qda.data.frame}
\alias{qda.default}
\alias{qda.formula}
\alias{qda.matrix}
\alias{model.frame.qda}
\alias{print.qda}
\title{
Quadratic Discriminant Analysis
}
\description{
Quadratic discriminant analysis.
}
\usage{
qda(x, \dots)

\method{qda}{formula}(formula, data, \dots, subset, na.action)

\method{qda}{default}(x, grouping, prior = proportions,
    method, CV = FALSE, nu, \dots)

\method{qda}{data.frame}(x, \dots)

\method{qda}{matrix}(x, grouping, \dots, subset, na.action)
}
\arguments{
\item{formula}{
A formula of the form \code{groups ~ x1 + x2 + \dots}  That is, the
response is the grouping factor and the right hand side specifies
the (non-factor) discriminators.
}
\item{data}{
Data frame from which variables specified in \code{formula} are
preferentially to be taken.
}
\item{x}{
(required if no formula is given as the principal argument.)
a matrix or data frame or Matrix containing the explanatory variables.
}
\item{grouping}{
(required if no formula principal argument is given.)
a factor specifying the class for each observation.
}
\item{prior}{
the prior probabilities of class membership.  If unspecified, the class
proportions for the training set are used.  If specified, the
probabilities should be specified in the order of the factor levels.
}
\item{subset}{
An index vector specifying the cases to be used in the training
sample.  (NOTE: If given, this argument must be named.)
}
\item{na.action}{
A function to specify the action to be taken if \code{NA}s are found.
The default action is for the procedure to fail.  An alternative is
na.omit, which leads to rejection of cases with missing values on
any required variable.  (NOTE: If given, this argument must be named.)
}
\item{method}{
\code{"moment"} for standard estimators of the mean and variance,
\code{"mle"} for MLEs, \code{"mve"} to use \code{cov.mve}, or \code{"t"} for robust
estimates based on a t distribution.
}
\item{CV}{
If true, returns results (classes and posterior probabilities) for
leave-out-out cross-validation. Note that if the prior is estimated,
the proportions in the whole dataset are used.
}
\item{nu}{
degrees of freedom for \code{method = "t"}.
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
an object of class \code{"qda"} containing the following components:

\item{prior}{
the prior probabilities used.
}
\item{means}{
the group means.
}
\item{scaling}{
for each group \code{i}, \code{scaling[,,i]} is an array which transforms observations
so that within-groups covariance matrix is spherical.
}
\item{ldet}{
a vector of half log determinants of the dispersion matrix.
}
\item{lev}{
the levels of the grouping factor.
}
\item{terms}{
(if formula is a formula)
an object of mode expression and class term summarizing
the  formula.
}
\item{call}{
the (matched) function call.
}

unless \code{CV=TRUE}, when the return value is a list with components:
\item{class}{
The MAP classification (a factor)
}
\item{posterior}{
posterior probabilities for the classes
}}
\details{
Uses a QR decomposition which will give an error message if the
within-group variance is singular for any group.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks}. Cambridge University Press.
}
\seealso{
\code{\link{predict.qda}}, \code{\link{lda}}
}
\examples{
tr <- sample(1:50, 25)
train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
z <- qda(train, cl)
predict(z,test)$class
}
\keyword{multivariate}
