\name{null.space.dimension}
\alias{null.space.dimension}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{The basis of the space of un-penalized functions for a TPRS}
\description{ The thin plate spline penalties give zero penalty to some
  functions. The space of these functions is spanned by a set of
  polynomial terms. \code{null.space.dimension} finds the dimension of this space, \eqn{M}{M}, given
  the number of covariates that the smoother is a function of, \eqn{d}{d},
  and the order of the smoothing penalty, \eqn{m}{m}. If \eqn{m}{m} does not
  satisfy \eqn{2m>d}{2m>d} then the smallest possible dimension
  for the null space is found given \eqn{d}{d} and the requirement that
  the smooth should be visually smooth.  
}
\usage{
null.space.dimension(d,m)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{d}{ is a positive integer - the number of variables of which the
   t.p.s. is a function. }
 \item{m}{ a non-negative integer giving the order of the penalty
   functional, or signalling that the default order should be used.}
}
\details{ Thin plate splines are only visually smooth if the order of the
  wiggliness penalty, \eqn{m}{m}, satisfies \eqn{2m > d+1}{2m >
    d+1}. If \eqn{2m<d+1}{2m<d+1} then this routine finds the smallest
  \eqn{m}{m} giving visual smoothness
  for the given \eqn{d}{d}, otherwise the supplied \eqn{m}{m} is used. The null space dimension is given by:

  \eqn{M=(m+d-1)!/(d!(m-1)!)}{M=(m+d-1)!/(d!(m-1)!}

  which is the value returned.

  
}
\value{An integer (array), the null space dimension
\eqn{M}{M}.
 
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\references{
Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

\url{http://www.maths.bath.ac.uk/~sw283/}
}



\seealso{\code{\link{tprs}}}

\examples{
require(mgcv)
null.space.dimension(2,0)
}
\keyword{models} \keyword{regression}%-- one or more ..


