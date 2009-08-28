\name{nnzero}
\alias{nnzero}

\title{The Number of Non-Zero Values of a Matrix}
\description{
  Returns the number of non-zero values of a numeric-like \R object, and
  in particular an object \code{x} inheriting from class
  \code{\linkS4class{Matrix}}.
}
\usage{
nnzero(x, na.counted = NA)
}
\arguments{
  \item{x}{an \R object, typically inheriting from class
    \code{\linkS4class{Matrix}} or \code{\link{numeric}}.}
  \item{na.counted}{a \code{\link{logical}} describing how
    \code{\link{NA}}s should be counted.  There are three possible
    settings for \code{na.counted}:
    \describe{
      \item{TRUE}{\code{NA}s \emph{are} counted as non-zero (since
	\dQuote{they are not zero}).}
      \item{NA}{(default)the result will be \code{NA} if there are \code{NA}'s in
	\code{x} (since \dQuote{NA's are not known, i.e., \emph{may be} zero}).}
      \item{FALSE}{\code{NA}s are \emph{omitted} from \code{x} before
	the non-zero entries are counted.}
    }
    For sparse matrices, you may often want to use \code{na.counted = TRUE}.
  }
}
% \details{
% }
\value{
  the number of non zero entries in \code{x} (typically
  \code{\link{integer}}).

  Note that for a \emph{symmetric} sparse matrix \code{S} (i.e., inheriting from
  class \code{\linkS4class{symmetricMatrix}}), \code{nnzero(S)} is
  typically \emph{twice} the \code{length(S@x)}.
}
%\author{Martin}
\seealso{The \code{\linkS4class{Matrix}} class also has a
  \code{\link{length}} method; typically, \code{length(M)} is much
  larger than \code{nnzero(M)} for a sparse matrix M, and the latter is
  a better indication of the \emph{size} of \code{M}.
}
\examples{
m <- Matrix(0+1:28, nrow = 4)
m[-3,c(2,4:5,7)] <- m[ 3, 1:4] <- m[1:3, 6] <- 0
(mT <- as(m, "dgTMatrix"))
nnzero(mT)
(S <- crossprod(mT))
nnzero(S)
str(S) # slots are smaller than nnzero()
stopifnot(nnzero(S) == sum(as.matrix(S) != 0))# failed earlier

data(KNex)
M <- KNex$mm
class(M)
dim(M)
length(M); stopifnot(length(M) == prod(dim(M)))
nnzero(M) # more relevant than length
## the above are also visible from
str(M)
}
\keyword{attribute}

