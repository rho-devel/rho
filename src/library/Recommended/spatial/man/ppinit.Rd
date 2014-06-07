% file spatial/man/ppinit.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{ppinit}
\alias{ppinit}
\title{
Read a Point Process Object from a File
}
\description{
Read a file in standard format and create a point process object.
}
\usage{
ppinit(file)
}
\arguments{
\item{file}{
string giving file name
}}
\value{
  class \code{"pp"} object with components \code{x}, \code{y},
  \code{xl}, \code{xu}, \code{yl}, \code{yu}
}
\details{
The file should contain

the number of points\cr
a header (ignored)\cr
xl xu yl yu scale\cr
x y (repeated n times)\cr
}
\section{Side Effects}{
Calls \code{ppregion} to set the domain.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{ppregion}}
}
\examples{
towns <- ppinit("towns.dat")
par(pty="s")
plot(Kfn(towns, 10), type="b", xlab="distance", ylab="L(t)")
}
\keyword{spatial}
