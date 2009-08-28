\name{number-class}
\docType{class}
\alias{number-class}
\title{Class "number" of Possibly Complex Numbers}
\description{The class \code{"number"} is a virtual class,
  currently used for vectors of eigen values which can be
  \code{"numeric"} or \code{"complex"}.

  It is a simple class union (\code{\link{setClassUnion}}) of
  \code{"numeric"} and \code{"complex"}.
}
\section{Objects from the Class}{Since it is a virtual Class, no objects
  may be created from it.}
\examples{
showClass("number")
stopifnot( is(1i, "number"), is(pi, "number"), is(1:3, "number") )
}
\keyword{classes}
