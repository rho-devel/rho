% COPY + Modification of src/library/stats/man/xtabs.Rd (2007-12-05)
% ^^^^^^^^^^^^^^^^^^^^^^
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{xtabs}
\alias{xtabs}
\title{Cross Tabulation, Optionally Sparse}
\description{
  Create a contingency table from cross-classifying factors, usually
  contained in a data frame, using a formula interface.

  This is a fully compatible extension of the standard \pkg{stats}
  package \code{\link[stats]{xtabs}()} function with the added option
  to produce a \emph{sparse} matrix result via \code{sparse = TRUE}.
}
\usage{
xtabs(formula = ~., data = parent.frame(), subset, sparse = FALSE, na.action,
      exclude = c(NA, NaN), drop.unused.levels = FALSE)
}
\arguments{
  \item{formula}{a \link{formula} object with the cross-classifying variables
    (separated by \code{+}) on the right hand side (or an object which
    can be coerced to a formula).  Interactions are not allowed.  On the
    left hand side, one may optionally give a vector or a matrix of
    counts; in the latter case, the columns are interpreted as
    corresponding to the levels of a variable.  This is useful if the
    data have already been tabulated, see the examples below.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{sparse}{logical specifying if the result should be a
    \emph{sparse} matrix, i.e., inheriting from
    \linkS4class{sparseMatrix}.  Only works for two factors (since there
    are no higher-order sparse array classes yet).
  }
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels of the classifying factors.}
  \item{drop.unused.levels}{a logical indicating whether to drop unused
    levels in the classifying factors.  If this is \code{FALSE} and
    there are unused levels, the table will contain zero marginals, and
    a subsequent chi-squared test for independence of the factors will
    not work.}
}
\details{
  For (non-sparse) \code{xtabs} results,
  there is a \code{summary} method for contingency table objects created
  by \code{table} or \code{xtabs}, which gives basic information and
  performs a chi-squared test for independence of factors (note that the
  function \code{\link{chisq.test}} currently only handles 2-d tables).

  If a left hand side is given in \code{formula}, its entries are simply
  summed over the cells corresponding to the right hand side; this also
  works if the lhs does not give counts.
}
\value{
  By default, when \code{sparse=FALSE},
  a contingency table in array representation of S3 class \code{c("xtabs",
    "table")}, with a \code{"call"} attribute storing the matched call.

  When \code{sparse=TRUE}, a sparse numeric matrix, specifically an
  object of S4 class \linkS4class{dgTMatrix}.
}
\seealso{
  The \pkg{stats} package version \code{\link[stats]{xtabs}} and its
  references.
}
\examples{
## See for non-sparse examples:
example(xtabs, package = "stats")

## similar to "nlme"s  'ergoStool' :
d.ergo <- data.frame(Type = paste("T", rep(1:4, 9*4), sep=""),
                     Subj = gl(9,4, 36*4))
xtabs(~ Type + Subj, data=d.ergo) # 4 replicates each
set.seed(15) # a subset of cases:
xtabs(~ Type + Subj, data=d.ergo[sample(36, 10),], sparse=TRUE)

## Hypothetical two level setup:
inner <- factor(sample(letters[1:25], 100, replace = TRUE))
inout <- factor(sample(LETTERS[1:5], 25, replace = TRUE))
fr <- data.frame(inner = inner, outer = inout[as.integer(inner)])
xtabs(~ inner + outer, fr, sparse = TRUE)
}
\keyword{category}
