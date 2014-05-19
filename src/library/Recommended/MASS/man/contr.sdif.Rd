\name{contr.sdif}
\alias{contr.sdif}
\title{
  Successive Differences Contrast Coding
}
\description{
  A coding for factors based on successive differences.
}
\usage{
contr.sdif(n, contrasts = TRUE, sparse = FALSE)
}
\arguments{
  \item{n}{
    The number of levels required.
  }
  \item{contrasts}{
    logical: Should there be \code{n - 1} columns orthogonal to the mean
    (the default) or \code{n} columns spanning the space?
  }
  \item{sparse}{
    logical.  If true and the result would be sparse (only
    true for \code{contrasts = FALSE}), return a sparse matrix.
  }
}
\details{
  The contrast coefficients are chosen so that the coded coefficients
  in a one-way layout are the differences between the means of the
  second and first levels, the third and second levels, and so on.  This
  makes most sense for ordered factors, but does not assume that the
  levels are equally spaced.
}
\value{
  If \code{contrasts} is \code{TRUE}, a matrix with \code{n} rows and
  \code{n - 1} columns, and the \code{n} by \code{n} identity matrix if
  \code{contrasts} is \code{FALSE}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  Fourth Edition, Springer.
}
\seealso{
  \code{\link{contr.treatment}}, \code{\link{contr.sum}},
  \code{\link{contr.helmert}}.
}
\examples{
(A <- contr.sdif(6))
zapsmall(ginv(A))
}
\keyword{models}
