% file MASS/man/anova.negbin.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{anova.negbin}
\alias{anova.negbin}
\title{
Likelihood Ratio Tests for Negative Binomial GLMs
}
\description{
Method function to perform sequential likelihood ratio tests for Negative
Binomial generalized linear models.
}
\usage{
\method{anova}{negbin}(object, \dots, test = "Chisq")
}
\arguments{
\item{object}{
  Fitted model object of class \code{"negbin"}, inheriting from
  classes \code{"glm"} and \code{"lm"}, specifying a Negative Binomial
  fitted GLM.  Typically the output of \code{\link{glm.nb}()}.
}
\item{\dots}{
Zero or more additional fitted model objects of class \code{"negbin"}.  They
should form a nested sequence of models, but need not be specified in any
particular order.
}
\item{test}{
  Argument to match the \code{test} argument of \code{\link{anova.glm}}.
  Ignored (with a warning if changed) if a sequence of two or more
  Negative Binomial fitted model objects is specified, but possibly
  used if only one object is specified.
}}
\note{
If only one fitted model object is specified, a sequential analysis of
deviance table is given for the fitted model.  The \code{theta} parameter is kept
fixed.  If more than one fitted model object is specified they must all be
of class \code{"negbin"} and likelihood ratio tests are done of each model within
the next.  In this case \code{theta} is assumed to have been re-estimated for each
model.
}
\details{
This function is a method for the generic function
\code{anova()} for class \code{"negbin"}.
It can be invoked by calling \code{anova(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{anova.negbin(x)} regardless of the
class of the object.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{glm.nb}}, \code{\link{negative.binomial}}, \code{\link{summary.negbin}}
}
\examples{
m1 <- glm.nb(Days ~ Eth*Age*Lrn*Sex, quine, link = log)
m2 <- update(m1, . ~ . - Eth:Age:Lrn:Sex)
anova(m2, m1)
anova(m2)
}
\keyword{regression}
