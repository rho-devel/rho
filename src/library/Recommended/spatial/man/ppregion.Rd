% file spatial/man/ppregion.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{ppregion}
\alias{ppregion}
\title{
Set Domain for Spatial Point Pattern Analyses
}
\description{
Sets the rectangular domain \code{(xl, xu)}  \eqn{\times}{x}  \code{(yl, yu)}.
}
\usage{
ppregion(xl = 0, xu = 1, yl = 0, yu = 1)
}
\arguments{
\item{xl}{
Either \code{xl} or a list containing components \code{xl}, \code{xu}, \code{yl}, \code{yu} (such as
a point-process object)
}
\item{xu}{
}
\item{yl}{
}
\item{yu}{
}}
\value{
none
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\section{Side Effects}{
initializes variables in the \code{C} subroutines.
}
\seealso{
\code{\link{ppinit}}, \code{\link{ppgetregion}}
}
\keyword{spatial}
