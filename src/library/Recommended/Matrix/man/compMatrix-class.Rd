\name{compMatrix-class}
\docType{class}
\alias{compMatrix-class}

\title{Class "compMatrix" of Composite (Factorizable) Matrices}
\description{
  Virtual class of \emph{composite} matrices; i.e., matrices
  that can be \emph{factorized}, typically as a product of simpler
  matrices.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{factors}:}{Object of class \code{"list"} - a list
      of factorizations of the matrix.  Note that this is typically
      empty, i.e. \code{list()}, initially and is updated automagically
      whenever a matrix factorization is computed.}
    \item{\code{Dim}}{,}
    \item{\code{Dimnames}:}{inherited from the
      \code{\linkS4class{Matrix}} class, see there.}
  }
}
\section{Extends}{
  Class \code{"Matrix"}, directly.
}
% \section{Methods}{
%   No methods defined with class "compMatrix" in the signature.
% }

%% FIXME:  add stuff here:

% \seealso{
%
% }
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% }
\keyword{classes}
