\name{mono.con}
\alias{mono.con}
\title{Monotonicity constraints for a cubic regression spline}
\description{
  Finds linear constraints sufficient for monotonicity (and
  optionally upper and/or lower boundedness) of a cubic regression
  spline. The basis representation assumed is that given by the
  \code{gam}, \code{"cr"} basis: that is the spline has a set of knots,
  which have fixed x values, but the y values of which constitute the
  parameters of the spline.
}
\usage{
mono.con(x,up=TRUE,lower=NA,upper=NA)
}
\arguments{
 \item{x}{The array of knot locations.}
 \item{up}{If \code{TRUE} then the constraints imply increase, if
   \code{FALSE} then decrease. }
 \item{lower}{This specifies the lower bound on the spline unless it is
   \code{NA} in which case no lower bound is imposed.}
 \item{upper}{This specifies the upper bound on the spline unless it is
   \code{NA} in which case no upper bound is imposed.}
}
\details{
  Consider the natural cubic spline passing through the points
  \eqn{ \{x_i,p_i:i=1 \ldots n \} }{ (x_i,p_i), i=1..n}.  Then it is possible
  to find a relatively small set of linear constraints on \eqn{\mathbf{p}}{p}
  sufficient to ensure monotonicity (and bounds if required):
  \eqn{\mathbf{Ap}\ge\mathbf{b}}{Ap >= b}.
  Details are given in Wood (1994).
}
\value{
  a list containing constraint matrix \code{A} and constraint vector \code{b}.
}
\references{
  Gill, P.E., Murray, W. and Wright, M.H. (1981)
  \emph{Practical Optimization}. Academic Press, London.

  Wood, S.N. (1994) Monotonic smoothing splines fitted by cross validation.
  \emph{SIAM Journal on Scientific Computing} \bold{15}(5), 1126--1133.

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org} }

\seealso{  \code{\link{magic}}, \code{\link{pcls}}
}
\examples{
## see ?pcls
}
\keyword{models} \keyword{smooth} \keyword{regression} %-- one or more ..






