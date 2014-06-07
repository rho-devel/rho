% file MASS/man/Null.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Null}
\alias{Null}
\title{
  Null Spaces of Matrices
}
\description{
  Given a matrix, \code{M}, find a matrix \code{N} giving a basis for the
  null space.  That is \code{t(N) \%*\% M}
  is the zero and \code{N} has the maximum number of linearly
  independent columns.
}
\usage{
Null(M)
}
\arguments{
  \item{M}{
    Input matrix.  A vector is coerced to a 1-column matrix.
  }
}
\value{
  The matrix \code{N} with the basis for the null space, or an empty
  vector if the matrix \code{M} is square and of maximal rank.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  % avoid link to Matrix
  \code{\link[base]{qr}}, \code{\link{qr.Q}}.
}
\examples{
# The function is currently defined as
function(M)
{
    tmp <- qr(M)
    set <- if(tmp$rank == 0) 1:ncol(M) else  - (1:tmp$rank)
    qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}
}
\keyword{algebra}
