\name{boot-practicals}
\alias{nested.corr}
\alias{lik.CI}
\title{ Functions for Bootstrap Practicals}
\description{
  Functions for use with the practicals in Davison and Hinkley (1997).
}
\usage{
nested.corr(data, w, t0, M)
lik.CI(like, lim)
}
\details{
  \code{nested.corr} is meant for use with the double bootstrap
  in practical 5.5 of Davison and Hinkley (1997).

  \code{lik.CI} is meant for use with practicals 10.1 and 10.2 of
  Davison and Hinkley (1997). 
}
\references{ 
  Davison, A. C. and Hinkley, D. V. (1997)
  \emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\author{
  Angelo J. Canty.  Faster version of \code{nested.corr} for
  \pkg{boot} 1.3-1 by Brian Ripley.
}
\keyword{internal}
