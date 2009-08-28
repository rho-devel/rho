\name{atomicVector-class}
\docType{class}
\alias{atomicVector-class}
\title{Virtual Class "atomicVector" of Atomic Vectors}
\description{
  The \code{\link[methods]{class}} \code{"atomicVector"} is a
  \emph{virtual} class containing all atomic vector classes of base \R,
  as also implicitly defined via \code{\link{is.atomic}}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  In the \pkg{Matrix} package, the "atomicVector" is used in signature
  where typically \dQuote{old-style} "matrix" objects can be used and
  can be substituted by simple vectors.
}
\section{Extends}{%% FIXME:  promptClass() should show the direct subclasses !
  The atomic classes
  \code{"logical"}, \code{"integer"}, \code{"double"}, \code{"numeric"},
  \code{"complex"}, \code{"raw"} and \code{"character"} are extended
  directly.  Note that \code{"numeric"} already contains \code{"integer"}
  and \code{"double"}, but we want all of them to be direct subclasses of
  \code{"atomicVector"}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{is.atomic}}, \code{\link{integer}}, \code{\link{numeric}},
  \code{\link{complex}}, etc.
}
\examples{
showClass("atomicVector")
}
\keyword{classes}
