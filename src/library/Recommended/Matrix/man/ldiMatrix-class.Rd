\name{ldiMatrix-class}
\docType{class}
\alias{ldiMatrix-class}
\alias{!,ldiMatrix-method}
%%----> put almost all methods into ./diagonalMatrix-class.Rd
% Deprecated:
\alias{coerce,ldiMatrix,lgCMatrix-method}
\alias{coerce,ldiMatrix,lgTMatrix-method}

\title{Class "ldiMatrix" of Diagonal Logical Matrices}
\description{The class \code{"ldiMatrix"} of logical diagonal matrices.
%% FIXME add more
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("ldiMatrix", ...)}
  but typically rather via \code{\link{Diagonal}}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{\code{"logical"} vector.}
    \item{\code{diag}:}{\code{"character"} string, either "U" or "N",
      see \code{\linkS4class{ddiMatrix}}.}
    \item{\code{Dim},\code{Dimnames}:}{matrix dimension and
      \code{\link{dimnames}}, see the \code{\linkS4class{Matrix}} class
      description.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{diagonalMatrix}"} and
  class \code{"\linkS4class{lMatrix}"}, directly.

  Class \code{"\linkS4class{sparseMatrix}"}, by class \code{"diagonalMatrix"}.
}
% \section{Methods}{
% No methods defined with class "ldiMatrix" in the signature.
% }
% \references{ ~put references to the literature/web site here ~ }
\seealso{
  Classes \code{\linkS4class{ddiMatrix}} and
  \code{\linkS4class{diagonalMatrix}}; function \code{\link{Diagonal}}.
}
\examples{
(lM <- Diagonal(x = c(TRUE,FALSE,FALSE)))
str(lM)#> gory details (slots)

crossprod(lM) # numeric
(nM <- as(lM, "nMatrix"))# -> sparse (not formally ``diagonal'')
crossprod(nM) # logical sparse
}
\keyword{classes}
