\name{rankMatrix}
\Rdversion{1.1}
\alias{rankMatrix}
\title{Rank of a Matrix}
\description{
  Compute the rank of matrix, a well-defined functional in theory,
  somewhat ambigous in practice.  We provide several methods, the
  default corresponding to Matlab's definition.
}
\usage{
rankMatrix(x, tol = NULL,
           method = c("tolNorm2", "qrLINPACK", "useGrad", "maybeGrad"),
           sval = svd(x, 0, 0)$d)
}
\arguments{
  \item{x}{numeric matrix, of dimension \eqn{n \times m}{n x m}, say.}
  \item{tol}{nonnegative number specifying a tolerance for
    \dQuote{practically zero} with specific meaning depending on
    \code{method}; by default, \code{max(dim(x)) *
      \link{.Machine}$double.eps * abs(max(sval))} is according to
    Matlab's default (for its only \code{method} "tolNorm2").}
  \item{method}{a character string specifying the computational method,
    can be abbreviated:
    \describe{
      \item{tolNorm2}{the number of singular values \code{>= tol};}
      \item{qrLINPACK}{this is the rank of \code{\link{qr}(x, tol,
	  LAPACK=FALSE)}, which is \code{qr(...)$rank} for a dense
	matrix, and the rank of \eqn{R} for sparse \code{x} (where
	\code{qr} uses a "sparseQR" method, see
	\code{\link{qr-methods}}, and not LINPACK).

	This used to be \emph{the} recommended way to compute a matrix
	rank for a while in the past.  For this method, \code{sval} are
	not used (nor computed).}
      \item{useGrad}{considering the \dQuote{gradient} of the
	(decreasing) singular values, the index of the \emph{smallest} gap.}
      \item{maybeGrad}{choosing method \code{"useGrad"} only when that
	seems \emph{reasonable}; otherwise using \code{"tolNorm2"}.}
%% FIXME say more
    }
  }
  \item{sval}{numeric vector of non-increasing singular values of
    \code{x}; typically unspecified and computed from \code{x}.}
}
% \details{
%   FIXME
% }
\value{
  positive integer in \code{1:min(dim(x))}, with attributes detailing
  the method used.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Martin Maechler; for the "*Grad" methods, building on
  suggestions by Ravi Varadhan.
}
\seealso{
 \code{\link{qr}}, \code{\link{svd}}.
}
\examples{
rankMatrix(cbind(1, 0, 1:3)) # 2

(meths <- eval(formals(rankMatrix)$method))

## a "border" case:
H12 <- Hilbert(12)
rankMatrix(H12, tol = 1e-20) # 12;  but  11  with default method & tol.
sapply(meths, function(.m.) rankMatrix(H12, method = .m.))
## tolNorm2 qrLINPACK   useGrad maybeGrad
##       11        12        11        11

## "sparse" case:
M15 <- kronecker(diag(x=c(100,1,10)), Hilbert(5))
sapply(meths, function(.m.) rankMatrix(M15, method = .m.))
#--> all 15, but 'useGrad' has 14.
}
\keyword{algebra}
\keyword{array}
