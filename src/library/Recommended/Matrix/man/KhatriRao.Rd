\name{KhatriRao}
\title{Khatri-Rao Matrix Product}
\alias{KhatriRao}
\description{
  Computes Khatri-Rao products for any kind of matrices.

  The Khatri-Rao product is a column-wise Kronecker product.  Originally
  introduced by Khatri and Rao (1968), it has many different applications,
  see Liu and Trenkler (2008) for a survey.  Notably, it is used in
  higher-dimensional tensor decompositions, see Bader and Kolda (2008).
}
\usage{
KhatriRao(X, Y = X, FUN = "*", make.dimnames = FALSE)
}
\arguments{
  \item{X,Y}{matrices of with the same number of columns.}
  \item{FUN}{the (name of the) \code{\link{function}} to be used for the
    column-wise Kronecker products, see \code{\link{kronecker}},
    defaulting to the usual multiplication.}
  \item{make.dimnames}{logical indicating if the result should inherit
    \code{\link{dimnames}} from \code{X} and \code{Y} in a simple way.}
}
%\details{
%}
\value{
  a \code{"\linkS4class{CsparseMatrix}"}, say \code{R}, the Khatri-Rao
  product of \code{X} (\eqn{n \times k}{n x k}) and \code{Y} (\eqn{m
    \times k}{m x k}), is of dimension \eqn{(n\cdot m) \times k}{(n*m) x k},
  where the j-th column, \code{R[,j]} is the kronecker product
  \code{\link{kronecker}(X[,j], Y[,j])}.
}
\note{%% TODO? Could make it generic, and have dense and sparse methods
  The current implementation is efficient for large sparse matrices.
}
\references{
  Khatri, C. G., and Rao, C. Radhakrishna (1968)
  Solutions to Some Functional Equations and Their Applications to
  Characterization of Probability Distributions.
  \emph{Sankhya: Indian J. Statistics, Series A} \bold{30}, 167--180.

  Liu, Shuangzhe, and G\enc{õ}{oe}tz Trenkler (2008)
  Hadamard, Khatri-Rao, Kronecker and Other Matrix Products.
  \emph{International J. Information and Systems Sciences} \bold{4}, 160--177.

  Bader, Brett W, and Tamara G Kolda (2008)
  Efficient MATLAB Computations with Sparse and Factored Tensors.
  \emph{SIAM J. Scientific Computing} \bold{30}, 205--231.
}
\author{
  Michael Cysouw, Univ. Marburg;
  minor tweaks by Martin Maechler.
}
\seealso{
 \code{\link{kronecker}}.
}
\examples{
## Example with very small matrices:
m <- matrix(1:12,3,4)
d <- diag(1:4)
KhatriRao(m,d)
KhatriRao(d,m)
dimnames(m) <- list(LETTERS[1:3], letters[1:4])
KhatriRao(m,d, make.dimnames=TRUE)
KhatriRao(d,m, make.dimnames=TRUE)
dimnames(d) <- list(NULL, paste0("D", 1:4))
KhatriRao(m,d, make.dimnames=TRUE)
KhatriRao(d,m, make.dimnames=TRUE)
dimnames(d) <- list(paste0("d", 10*1:4), paste0("D", 1:4))
KhatriRao(m,d, make.dimnames=TRUE)
KhatriRao(d,m, make.dimnames=TRUE)

nm <- as(m,"nMatrix")
nd <- as(d,"nMatrix")
KhatriRao(nm,nd, make.dimnames=TRUE)
KhatriRao(nd,nm, make.dimnames=TRUE)

stopifnot(dim(KhatriRao(m,d)) == c(nrow(m)*nrow(d), ncol(d)))
}
\keyword{methods}
\keyword{array}

