\name{CAex}
\alias{CAex}
\docType{data}
\title{Albers' example Matrix with "Difficult" Eigen Factorization}
\description{
  An example of a sparse matrix for which \code{\link{eigen}()} seemed
  to be difficult, an unscaled version of this has been posted to the
  web, accompanying an E-mail to R-help
  (\url{https://stat.ethz.ch/mailman/listinfo/r-help}), by
  Casper J Albers, Open University, UK.
}
\usage{data(CAex)}
\format{
  This is a \eqn{72 \times 72}{72 * 72} symmetric matrix with 216
  non-zero entries in five bands, stored as sparse matrix of class
  \code{\linkS4class{dgCMatrix}}.
}
\details{
  In some versions of \R, \code{\link{eigen}(CAex)} fell into an
  infinite loop (whereas \code{\link{eigen}(CAex, EISPACK=TRUE)} has
  been okay).
}
% \source{ see above
% }
\examples{
data(CAex)
str(CAex) # of class "dgCMatrix"

image(CAex)# -> it's a simple band matrix with 5 bands
## and the eigen values are basically 1 (42 times) and 0 (30 x):
zapsmall(ev <- eigen(CAex, EISPACK=TRUE, only.values=TRUE)$values)
## i.e., the matrix is symmetric, hence
sCA <- as(CAex, "symmetricMatrix")
## and
stopifnot(class(sCA) == "dsCMatrix",
          as(sCA, "matrix") == as(CAex, "matrix"))
}
\keyword{datasets}
