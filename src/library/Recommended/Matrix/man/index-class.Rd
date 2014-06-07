\name{index-class}
\docType{class}
\alias{index-class}
%% The "[" and "[<-" methods are
%% in ./Xtrct-methods.Rd and ./Subassign-methods.Rd
\title{Virtual Class "index" - Simple Class for Matrix Indices}
\description{The class \code{"index"} is a virtual class used for
  indices (in signatures) for matrix indexing and sub-assignment of
  \pkg{"Matrix"} matrices.

  In fact, it is currently implemented as a simple class union
  (\code{\link{setClassUnion}}) of
  \code{"numeric"},  \code{"logical"} and \code{"character"}.
}
\section{Objects from the Class}{Since it is a virtual Class, no objects
  may be created from it.}
\seealso{
  \code{\link{[-methods}}, and
  %% FIXME: bug in Rdconv needs '[Matrix]' below:
  \code{\link[Matrix]{Subassign-methods}}, also for examples.
}
\examples{
showClass("index")
}
\keyword{classes}
