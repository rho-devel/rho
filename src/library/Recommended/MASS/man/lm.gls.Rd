% file MASS/man/lm.gls.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{lm.gls}
\alias{lm.gls}
\title{
Fit Linear Models by Generalized Least Squares
}
\description{
Fit linear models by Generalized Least Squares
}
\usage{
lm.gls(formula, data, W, subset, na.action, inverse = FALSE,
       method = "qr", model = FALSE, x = FALSE, y = FALSE,
       contrasts = NULL, \dots)
}
\arguments{
\item{formula}{
a formula expression as for regression models, of the form
\code{response ~ predictors}.
See the documentation of \code{formula} for other details.
}
\item{data}{
an optional data frame in which to interpret the variables occurring
in \code{formula}.
}
\item{W}{
a weight matrix.
}
\item{subset}{
expression saying which subset of the rows of the data should  be used
in the fit. All observations are included by default.
}
\item{na.action}{
a function to filter missing data.
}
\item{inverse}{
logical: if true \code{W} specifies the inverse of the weight matrix: this
is appropriate if a variance matrix is used.
}
\item{method}{
method to be used by \code{lm.fit}.
}
\item{model}{
should the model frame be returned?
}
\item{x}{
should the design matrix be returned?
}
\item{y}{
should the response be returned?
}
\item{contrasts}{
a list of contrasts to be used for some or all of
}
\item{\dots}{
additional arguments to \code{\link{lm.fit}}.
}}
\value{
  An object of class \code{"lm.gls"}, which is similar to an \code{"lm"}
  object.  There is no \code{"weights"} component, and only a few \code{"lm"}
  methods will work correctly.  As from version 7.1-22 the residuals and
  fitted values refer to the untransformed problem.
}
\details{
  The problem is transformed to uncorrelated form and passed to
  \code{\link{lm.fit}}.
}
\seealso{
  \code{\link[nlme]{gls}}, \code{\link{lm}}, \code{\link{lm.ridge}}
}
\keyword{models}
