\name{rep2abI}
\alias{rep2abI}
\title{Replicate Vectors into 'abIndex' Result}
\description{
  \code{rep2abI(x, times)} conceptually computes
  \code{\link{rep.int}(x, times)} but with an
  \code{\linkS4class{abIndex}} class result.
}
\usage{
rep2abI(x, times)
}
\arguments{
  \item{x}{numeric vector}
  \item{times}{integer (valued) scalar: the number of repetitions}
}
% \details{
% }
\value{
  a vector of \code{\link{class}} \code{\linkS4class{abIndex}}
}
\seealso{
  \code{\link{rep.int}()}, the base function;
  \code{\link{abIseq}}, \code{\linkS4class{abIndex}}.
}
\examples{
(ab <- rep2abI(2:7, 4))
stopifnot(identical(as(ab, "numeric"),
	   rep(2:7, 4)))
}
\keyword{manip}
