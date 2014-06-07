% file MASS/man/gamma.dispersion.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{gamma.dispersion}
\alias{gamma.dispersion}
\title{
Calculate the MLE of the Gamma Dispersion Parameter in a GLM Fit
}
\description{
A front end to \code{gamma.shape} for convenience.  Finds the
reciprocal of the estimate of the shape parameter only.
}
\usage{
gamma.dispersion(object, \dots)
}
\arguments{
\item{object}{
Fitted model object giving the gamma fit.
}
\item{\dots}{
Additional arguments passed on to \code{gamma.shape}.
}}
\value{
The MLE of the dispersion parameter of the gamma distribution.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{gamma.shape.glm}}, including the example on its help page.
}
\keyword{models}
