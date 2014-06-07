% file MASS/man/lda.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lda}
\alias{lda}
\alias{lda.default}
\alias{lda.data.frame}
\alias{lda.formula}
\alias{lda.matrix}
\alias{model.frame.lda}
\alias{print.lda}
\alias{coef.lda}
\title{
Linear Discriminant Analysis
}
\description{
Linear discriminant analysis.
}
\usage{
lda(x, \dots)

\method{lda}{formula}(formula, data, \dots, subset, na.action)

\method{lda}{default}(x, grouping, prior = proportions, tol = 1.0e-4,
    method, CV = FALSE, nu, \dots)

\method{lda}{data.frame}(x, \dots)

\method{lda}{matrix}(x, grouping, \dots, subset, na.action)
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
    the prior probabilities of class membership.  If unspecified, the
    class proportions for the training set are used.  If present, the
    probabilities should be specified in the order of the factor
    levels.
  }
  \item{tol}{
    A tolerance to decide if a matrix is singular; it will reject variables
    and linear combinations of unit-variance variables whose variance is
    less than \code{tol^2}.
  }
  \item{subset}{
    An index vector specifying the cases to be used in the training
    sample.  (NOTE: If given, this argument must be named.)
  }
  \item{na.action}{
    A function to specify the action to be taken if \code{NA}s are found.
    The default action is for the procedure to fail.  An alternative is
    \code{na.omit}, which leads to rejection of cases with missing values on
    any required variable.  (NOTE: If given, this argument must be named.)
  }
  \item{method}{
    \code{"moment"} for standard estimators of the mean and variance,
    \code{"mle"} for MLEs, \code{"mve"} to use \code{\link{cov.mve}}, or
    \code{"t"} for robust estimates based on a \eqn{t} distribution.
  }
  \item{CV}{
    If true, returns results (classes and posterior probabilities) for
    leave-one-out cross-validation. Note that if the prior is estimated,
    the proportions in the whole dataset are used.
  }
  \item{nu}{
    degrees of freedom for \code{method = "t"}.
  }
  \item{\dots}{
    arguments passed to or from other methods.
}}
\value{
  If \code{CV = TRUE} the return value is a list with components
  \code{class}, the MAP classification (a factor), and \code{posterior},
  posterior probabilities for the classes.

  Otherwise it is an object of class \code{"lda"} containing the
  following components:
  \item{prior}{
    the prior probabilities used.
  }
  \item{means}{
    the group means.
  }
  \item{scaling}{
    a matrix which transforms observations to discriminant functions,
    normalized so that within groups covariance matrix is spherical.
  }
  \item{svd}{
    the singular values, which give the ratio of the between- and
    within-group standard deviations on the linear discriminant
    variables.  Their squares are the canonical F-statistics.
  }
  \item{N}{
    The number of observations used.
  }
  \item{call}{
    The (matched) function call.
  }
}
\details{
The function
tries hard to detect if the within-class covariance matrix is
singular. If any variable has within-group variance less than
\code{tol^2} it will stop and report the variable as constant.  This
could result from poor scaling of the problem, but is more
likely to result from constant variables.

Specifying the \code{prior} will affect the classification unless
over-ridden in \code{predict.lda}.  Unlike in most statistical packages, it
will also affect the rotation of the linear discriminants within their
space, as a weighted between-groups covariance matrix is used. Thus
the first few linear discriminants emphasize the differences between
groups with the weights given by the prior, which may differ from
their prevalence in the dataset.

If one or more groups is missing in the supplied data, they are dropped
with a warning, but the classifications produced are with respect to the
original set of levels.
}
\note{
This function may be called giving either a formula and
optional data frame, or a matrix and grouping factor as the first
two arguments.  All other arguments are optional, but \code{subset=} and
\code{na.action=}, if required, must be fully named.

If a formula is given as the principal argument the object may be
modified using \code{update()} in the usual way.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks}. Cambridge University Press.
}
\seealso{
\code{\link{predict.lda}}, \code{\link{qda}}, \code{\link{predict.qda}}
}
\examples{
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75)
table(Iris$Sp[train])
## your answer may differ
##  c  s  v
## 22 23 30
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
predict(z, Iris[-train, ])$class
##  [1] s s s s s s s s s s s s s s s s s s s s s s s s s s s c c c
## [31] c c c c c c c v c c c c v c c c c c c c c c c c c v v v v v
## [61] v v v v v v v v v v v v v v v
(z1 <- update(z, . ~ . - Petal.W.))
}
\keyword{multivariate}
