% file MASS/man/loglm.Rd
% copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
%
\name{loglm}
\alias{loglm}
% \alias{anova.loglm}
% \alias{print.anova.loglm}
% \alias{coef.loglm}
% \alias{extractAIC.loglm}
% \alias{fitted.loglm}
% \alias{print.loglm}
% \alias{residuals.loglm}
% \alias{update.loglm}
\title{
Fit Log-Linear Models by Iterative Proportional Scaling
}
\description{
This function provides a front-end to the standard function,
\code{loglin}, to allow log-linear models to be specified and fitted
in a manner similar to that of other fitting functions, such as
\code{glm}.
}
\usage{
loglm(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{formula}{
    A linear model formula specifying the log-linear model.

    If the left-hand side is empty, the \code{data} argument is required
    and must be a (complete) array of frequencies.  In this case the
    variables on the right-hand side may be the names of the
    \code{dimnames} attribute of the frequency array, or may be the
    positive integers: 1, 2, 3, \dots used as alternative names for the
    1st, 2nd, 3rd, \dots dimension (classifying factor).
    If the left-hand side is not empty it specifies a vector of
    frequencies.  In this case the data argument, if present, must be
    a data frame from which the left-hand side vector and the
    classifying factors on the right-hand side are (preferentially)
    obtained.  The usual abbreviation of a \code{.} to stand for \sQuote{all
    other variables in the data frame} is allowed.  Any non-factors
    on the right-hand side of the formula are coerced to factor.
  }
  \item{data}{
    Numeric array or data frame.  In the first case it specifies the
    array of frequencies; in then second it provides the data frame
    from which the variables occurring in the formula are
    preferentially obtained in the usual way.
    
    This argument may be the result of a call to \code{\link{xtabs}}.
  }
  \item{subset}{
    Specifies a subset of the rows in the data frame to be used.  The
    default is to take all rows.
  }
  \item{na.action}{
    Specifies a method for handling missing observations.  The
    default is to fail if missing values are present.
  }
  \item{\dots}{
    May supply other arguments to the function \code{\link{loglm1}}.
}}
\value{
  An object of class \code{"loglm"} conveying the results of the fitted
  log-linear model.  Methods exist for the generic functions \code{print},
  \code{summary}, \code{deviance}, \code{fitted}, \code{coef},
  \code{resid}, \code{anova} and \code{update}, which perform the expected
  tasks.  Only log-likelihood ratio tests are allowed using \code{anova}.

  The deviance is simply an alternative name for the log-likelihood
  ratio statistic for testing the current model within a saturated
  model, in accordance with standard usage in generalized linear
  models.
}
\details{
  If the left-hand side of the formula is empty the \code{data} argument
  supplies the frequency array and the right-hand side of the
  formula is used to construct the list of fixed faces as required
  by \code{loglin}.  Structural zeros may be specified by giving a
  \code{start} argument with those entries set to zero, as described in
  the help information for \code{loglin}.

  If the left-hand side is not empty, all variables on the
  right-hand side are regarded as classifying factors and an array
  of frequencies is constructed.  If some cells in the complete
  array are not specified they are treated as structural zeros.
  The right-hand side of the formula is again used to construct the
  list of faces on which the observed and fitted totals must agree,
  as required by \code{loglin}.  Hence terms such as
  \code{a:b}, \code{a*b} and \code{a/b} are all equivalent.
}
\section{Warning}{
  If structural zeros are present, the calculation of degrees of
  freedom may not be correct.  \code{loglin} itself takes no action to
  allow for structural zeros.  \code{loglm} deducts one degree of
  freedom for each structural zero, but cannot make allowance for
  gains in error degrees of freedom due to loss of dimension in the
  model space.  (This would require checking the rank of the
  model matrix, but since iterative proportional scaling methods
  are developed largely to avoid constructing the model matrix
  explicitly, the computation is at least difficult.)
  
  When structural zeros (or zero fitted values) are present the
  estimated coefficients will not be available due to infinite
  estimates.  The deviances will normally continue to be correct, though.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link{loglm1}}, \code{\link{loglin}}
}
\examples{
# The data frames  Cars93, minn38 and quine are available
# in the MASS package.

# Case 1: frequencies specified as an array.
sapply(minn38, function(x) length(levels(x)))
## hs phs fol sex f
##  3   4   7   2 0
##minn38a <- array(0, c(3,4,7,2), lapply(minn38[, -5], levels))
##minn38a[data.matrix(minn38[,-5])] <- minn38$f

## or more simply
minn38a <- xtabs(f ~ ., minn38)

fm <- loglm(~ 1 + 2 + 3 + 4, minn38a)  # numerals as names.
deviance(fm)
## [1] 3711.9
fm1 <- update(fm, .~.^2)
fm2 <- update(fm, .~.^3, print = TRUE)
## 5 iterations: deviation 0.075
anova(fm, fm1, fm2)

# Case 1. An array generated with xtabs.

loglm(~ Type + Origin, xtabs(~ Type + Origin, Cars93))

# Case 2.  Frequencies given as a vector in a data frame
names(quine)
## [1] "Eth"  "Sex"  "Age"  "Lrn"  "Days"
fm <- loglm(Days ~ .^2, quine)
gm <- glm(Days ~ .^2, poisson, quine)  # check glm.
c(deviance(fm), deviance(gm))          # deviances agree
## [1] 1368.7 1368.7
c(fm$df, gm$df)                        # resid df do not!
c(fm$df, gm$df.residual)               # resid df do not!
## [1] 127 128
# The loglm residual degrees of freedom is wrong because of
# a non-detectable redundancy in the model matrix.
}
\keyword{category}
\keyword{models}
