% file spatial/man/ppgetregion.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{ppgetregion}
\alias{ppgetregion}
\title{
Get Domain for Spatial Point Pattern Analyses
}
\description{
Retrieves the rectangular domain \code{(xl, xu)}  \eqn{\times}{x}  \code{(yl, yu)} from the
underlying \code{C} code.
}
\usage{
ppgetregion()
}
\value{
A vector of length four with names \code{c("xl", "xu", "yl", "yu")}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{ppregion}}
}
\keyword{spatial}
