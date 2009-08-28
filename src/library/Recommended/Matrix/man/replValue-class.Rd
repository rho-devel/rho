\name{replValue-class}
\docType{class}
\alias{replValue-class}
%% >>>>>>> The  "[<-" methods are in ./Subassign-methods.Rd
\title{Virtual Class "replValue" - Simple Class for subassignment Values}
\description{The class \code{"replValue"} is a virtual class used for
  values in signatures for sub-assignment of \pkg{"Matrix"} matrices.

  In fact, it is a simple class union  (\code{\link{setClassUnion}}) of
  \code{"numeric"} and \code{"logical"} (and maybe \code{"complex"} in
  the future).
}
\section{Objects from the Class}{Since it is a virtual Class, no objects
  may be created from it.}
\seealso{
  %% FIXME: bug in Rdconv needs '[Matrix]' below:
  \code{\link[Matrix]{Subassign-methods}}, also for examples.
}
\examples{
showClass("replValue")
}
\keyword{classes}
