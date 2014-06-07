\name{loglm1}
\alias{loglm1}
\alias{loglm1.xtabs}
\alias{loglm1.data.frame}
\alias{loglm1.default}
\title{
Fit Log-Linear Models by Iterative Proportional Scaling -- Internal function
}
\description{
  \code{loglm1} is an internal function used by \code{\link{loglm}}.
  It is a generic function dispatching on the \code{data} argument.
}
\usage{
loglm1(formula, data, \dots)

\method{loglm1}{xtabs}(formula, data, \dots)

\method{loglm1}{data.frame}(formula, data, \dots)

\method{loglm1}{default}(formula, data, start = rep(1, length(data)), fitted = FALSE,
       keep.frequencies = fitted, param = TRUE, eps = 1/10,
       iter = 40, print = FALSE, \dots)
}
\arguments{
  \item{formula}{
    A linear model formula specifying the log-linear model.
    See \code{\link{loglm}} for its interpretation.}

  \item{data}{
    Numeric array or data frame.  In the first case it specifies the
    array of frequencies; in then second it provides the data frame
    from which the variables occurring in the formula are
    preferentially obtained in the usual way.

    This argument may also be the result of a call to \code{\link{xtabs}}.
  }
  \item{start, param, eps, iter, print}{Arguments passed to
    \code{\link{loglin}}.}
  \item{fitted}{logical: should the fitted values be returned?}
  \item{keep.frequencies}{
    If \code{TRUE} specifies that the (possibly constructed) array of
    frequencies is to be retained as part of the fitted model object.  The
    default action is to use the same value as that used for \code{fitted}.
  }
  \item{\dots}{arguments passed to the default method.}
}
\value{
  An object of class \code{"loglm"}. 
}
\seealso{
\code{\link{loglm}}, \code{\link{loglin}}
}
\keyword{internal}
