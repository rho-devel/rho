% file MASS/man/negative.binomial.Rd
% copyright (C) 1994-2006 W. N. Venables and B. D. Ripley
%
\name{negative.binomial}
\alias{negative.binomial}
\title{
  Family function for Negative Binomial GLMs
}
\description{
  Specifies the information required to fit a Negative Binomial generalized
  linear model, with known \code{theta} parameter, using \code{glm()}.
}
\usage{
negative.binomial(theta = stop("'theta' must be specified"), link = "log")
}
\arguments{
  \item{theta}{
    The known value of the additional parameter, \code{theta}.
  }
  \item{link}{
    The link function, as a character string, name or one-element
    character vector specifying one of \code{log}, \code{sqrt}
    or \code{identity}, or an object of class
    \code{"\link[=family]{link-glm}"}.
  }
}
\value{
  An object of class \code{"family"}, a list of functions and
  expressions needed by \code{glm()} to fit a Negative Binomial
  generalized linear model.
}
\seealso{
  \code{\link{glm.nb}}, \code{\link{anova.negbin}},
  \code{\link{summary.negbin}}
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\examples{
# Fitting a Negative Binomial model to the quine data
#   with theta = 2 assumed known.
#
glm(Days ~ .^4, family = negative.binomial(2), data = quine)
}
\keyword{regression}
\keyword{models}
