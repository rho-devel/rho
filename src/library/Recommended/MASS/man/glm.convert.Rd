% file MASS/man/glm.convert.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{glm.convert}
\alias{glm.convert}
\title{
Change a Negative Binomial fit to a GLM fit
}
\description{
This function modifies an output object from \code{glm.nb()} to one
that looks like the output from \code{glm()} with a negative binomial
family.  This allows it to be updated keeping the theta parameter
fixed.
}
\usage{
glm.convert(object)
}
\arguments{
\item{object}{
  An object of class \code{"negbin"}, typically the output from
  \code{\link{glm.nb}()}.
}}
\value{
An object of class \code{"glm"} with negative binomial family.  The theta
parameter is then fixed at its present estimate.
}
\details{
Convenience function needed to effect some low level changes to the
structure of the fitted model object.
}
\seealso{
\code{\link{glm.nb}}, \code{\link{negative.binomial}}, \code{\link{glm}}
}
\examples{
quine.nb1 <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
quine.nbA <- glm.convert(quine.nb1)
quine.nbB <- update(quine.nb1, . ~ . + Sex:Age:Lrn)
anova(quine.nbA, quine.nbB)
}
\keyword{regression}
\keyword{models}
