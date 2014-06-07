\name{diagU2N}
\title{Transform Triangular Matrices from Unit Triangular to General and back}
\alias{diagN2U}
\alias{diagU2N}
\alias{.diagU2N}
\description{
  Transform a triangular matrix \code{x}, i.e., of \code{\link{class}}
  \code{"\linkS4class{triangularMatrix}"},
  from (internally!) unit triangular (\dQuote{unitriangular}) to
  \dQuote{general} (\code{diagU2N(x)}) or back (\code{diagN2U(x)}).
  Note that the latter, \code{diagN2U(x)}, also sets the diagonal to one
  in cases where \code{diag(x)} was not all one.

  \code{.diagU2N(x)} assumes but does \emph{not} check that \code{x} is
  a \code{\linkS4class{triangularMatrix}} with \code{diag} slot
  \code{"U"}, and should hence be used with care.
}
\usage{
 diagN2U(x, cl = getClassDef(class(x)), checkDense = FALSE)
 diagU2N(x, cl = getClassDef(class(x)), checkDense = FALSE)
.diagU2N(x, cl, checkDense = FALSE)
}
\arguments{
  \item{x}{a \code{\linkS4class{triangularMatrix}}, often sparse.}
  \item{cl}{(optional, for speedup only:) class (definition) of \code{x}.}
  \item{checkDense}{logical indicating if dense (see
    \code{\linkS4class{denseMatrix}}) matrices should be considered at
    all; i.e., when false, as per default, the result will be sparse even
    when \code{x} is dense.}
}
\details{
  The concept of unit triangular matrices with a \code{diag} slot of
  \code{"U"} stems from LAPACK.
}
\note{Such internal storage details should rarely be of relevance to the
  user.  Hence, these functions really are rather \emph{internal}
  utilities.
}
\value{
  a triangular matrix of the same \code{\link{class}} and (semantically)
  with identical entries as \code{x},
  but with a different \code{diag} slot.
}
\seealso{
  \code{"\linkS4class{triangularMatrix}"},
  \code{"\linkS4class{dtCMatrix}"}.
}
\examples{
(T <- Diagonal(7) + triu(Matrix(rpois(49, 1/4), 7,7), k = 1))
(uT <- diagN2U(T)) # "unitriangular"
(t.u <- diagN2U(10*T))# changes the diagonal!
stopifnot(all(T == uT), diag(t.u) == 1,
          identical(T, diagU2N(uT)))
T[upper.tri(T)] <- 5
T <- diagN2U(as(T,"triangularMatrix"))
stopifnot(T@diag == "U")
dT <- as(T, "denseMatrix")
dt. <- diagN2U(dT)
dtU <- diagN2U(dT, checkDense=TRUE)
stopifnot(is(dtU, "denseMatrix"), is(dt., "sparseMatrix"),
          all(dT == dt.), all(dT == dtU),
          dt.@diag == "U", dtU@diag == "U")
}
\keyword{utilities}
\keyword{classes}
