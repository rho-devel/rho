% file MASS/man/logtrans.Rd
% copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
%
\name{logtrans}
\alias{logtrans}
\alias{logtrans.formula}
\alias{logtrans.lm}
\alias{logtrans.default}
\title{
Estimate log Transformation Parameter
}
\description{
Find and optionally plot the marginal (profile) likelihood for alpha
for a transformation model of the form \code{log(y + alpha) ~ x1 + x2 + \dots}.
}
\usage{
logtrans(object, ...)

\method{logtrans}{default}(object, \dots, alpha = seq(0.5, 6, by = 0.25) - min(y),
         plotit = TRUE, interp =, xlab = "alpha",
         ylab = "log Likelihood")

\method{logtrans}{formula}(object, data, \dots)

\method{logtrans}{lm}(object, \dots)
}
\arguments{
\item{object}{
Fitted linear model object, or formula defining the untransformed
model that is \code{y ~ x1 + x2 + \dots}.  The function is generic.
}
\item{\dots}{
If \code{object} is a formula, this argument may specify a data frame
as for \code{lm}.
}
\item{alpha}{
Set of values for the transformation parameter, alpha.
}
\item{plotit}{
Should plotting be done?
}
\item{interp}{
Should the marginal log-likelihood be interpolated with a spline
approximation?   (Default is \code{TRUE} if plotting is to be done and
the number of real points is less than 100.)
}
\item{xlab}{
as for \code{plot}.
}
\item{ylab}{
as for \code{plot}.
}
\item{data}{
  optional \code{data} argument for \code{lm} fit.
}
}
\value{
List with components \code{x} (for alpha) and \code{y} (for the marginal
log-likelihood values).
}
\section{Side Effects}{
A plot of the marginal log-likelihood is produced, if requested,
together with an approximate mle and 95\% confidence interval.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{boxcox}}
}
\examples{
logtrans(Days ~ Age*Sex*Eth*Lrn, data = quine,
         alpha = seq(0.75, 6.5, len=20))
}
\keyword{regression}
\keyword{models}
\keyword{hplot}
