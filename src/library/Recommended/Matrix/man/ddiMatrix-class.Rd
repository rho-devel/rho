\name{ddiMatrix-class}
\title{Class "ddiMatrix" of Diagonal Numeric Matrices}
\docType{class}
\alias{ddiMatrix-class}
%%----> put all methods into ./diagonalMatrix-class.Rd
%
\description{The class \code{"ddiMatrix"} of numerical diagonal matrices.
  %% FIXME add more
  Note that diagonal matrices now extend \emph{\code{sparseMatrix}}, whereas
  they did extend dense matrices earlier.% up to early summer 2008.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("ddiMatrix", ...)}
  but typically rather via \code{\link{Diagonal}}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{numeric vector.  For an \eqn{n \times n}{n * n}
      matrix, the \code{x} slot is of length \eqn{n} or \code{0},
      depending on the \code{diag} slot:}
    \item{\code{diag}:}{\code{"character"} string, either \code{"U"} or
      \code{"N"} where \code{"U"} denotes unit-diagonal, i.e., identity
      matrices.}
    \item{\code{Dim},\code{Dimnames}:}{matrix dimension and
      \code{\link{dimnames}}, see the \code{\linkS4class{Matrix}} class
      description.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{diagonalMatrix}"}, directly.
  Class \code{"\linkS4class{dMatrix}"}, directly.
  Class \code{"\linkS4class{sparseMatrix}"}, indirectly, see
  \code{\link{showClass}("ddiMatrix")}.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "ddiMatrix", y = "ddiMatrix")}: ... }
	 }
}
\seealso{
  Class \code{\linkS4class{diagonalMatrix}} and function \code{\link{Diagonal}}.
}
\examples{
(d2 <- Diagonal(x = c(10,1)))
str(d2)
## slightly larger in internal size:
str(as(d2, "sparseMatrix"))

M <- Matrix(cbind(1,2:4))
M \%*\% d2 #> `fast' multiplication

chol(d2) # trivial
stopifnot(is(cd2 <- chol(d2), "ddiMatrix"),
          all.equal(cd2@x, c(sqrt(10),1)))
}
\keyword{classes}
