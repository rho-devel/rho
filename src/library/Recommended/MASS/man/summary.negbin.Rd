% file MASS/man/summary.negbin.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{summary.negbin}
\alias{summary.negbin}
\alias{print.summary.negbin}
\title{
Summary Method Function for Objects of Class 'negbin'
}
\description{
Identical to \code{summary.glm}, but with three lines of additional output: the
ML estimate of theta, its standard error, and twice the log-likelihood
function.
}
\usage{
\method{summary}{negbin}(object, dispersion = 1, correlation = FALSE, \dots)
}
\arguments{
\item{object}{
fitted model object of class \code{negbin} inheriting from \code{glm} and \code{lm}.
Typically the output of \code{glm.nb}.
}
\item{dispersion}{
as for \code{summary.glm}, with a default of 1.
}
\item{correlation}{
as for \code{summary.glm}.
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
As for \code{summary.glm}; the additional lines of output are not included in
the resultant object.
}
\section{Side Effects}{
A summary table is produced as for \code{summary.glm}, with the additional
information described above.
}
\details{
\code{summary.glm} is used to produce the majority of the output and supply the
result.
This function is a method for the generic function
\code{summary()} for class \code{"negbin"}.
It can be invoked by calling \code{summary(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{summary.negbin(x)} regardless of the
class of the object.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{summary}}, \code{\link{glm.nb}}, \code{\link{negative.binomial}}, \code{\link{anova.negbin}}
}
\keyword{models}
\examples{
summary(glm.nb(Days ~ Eth*Age*Lrn*Sex, quine, link = log))
}
