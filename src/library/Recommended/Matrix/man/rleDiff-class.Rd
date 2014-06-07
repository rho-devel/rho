\name{rleDiff-class}
\docType{class}
\Rdversion{1.1}
\title{Class "rleDiff" of  rle(diff(.))  Stored Vectors}
\alias{rleDiff-class}
\alias{show,rleDiff-method}
\description{
  Class \code{"rleDiff"} is for compactly storing long vectors
  which mainly consist of \emph{linear} stretches.  For such a vector
  \code{x}, \code{\link{diff}(x)} consists of \emph{constant} stretches
  and is hence well compressable via \code{\link{rle}()}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("rleDiff", ...)}.

  Currently experimental, see below.
}
\section{Slots}{
  \describe{
    \item{\code{first}:}{A single number (of class \code{"numLike"},
      a class union of \code{"numeric"} and \code{"logical"}).}
    \item{\code{rle}:}{Object of class \code{"rle"}, basically a
      \code{\link{list}} with components \code{"lengths"} and
      \code{"values"}, see \code{\link{rle}()}.  As this is used to
      encode potentially huge index vectors, \code{lengths} may be of
      type \code{\link{double}} here.}
  }
}
\section{Methods}{
  There is a simple \code{\link{show}} method only.% currently
}
\note{
  This is currently an \emph{experimental} auxiliary class
  for the class \code{\linkS4class{abIndex}}, see there.
}
\seealso{
  \code{\link{rle}}, \code{\linkS4class{abIndex}}.
}
\examples{
showClass("rleDiff")

ab <- c(abIseq(2, 100), abIseq(20, -2))
ab@rleD  # is "rleDiff"
}
\keyword{classes}
