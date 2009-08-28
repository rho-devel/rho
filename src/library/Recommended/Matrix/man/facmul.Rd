\name{facmul}
\title{Multiplication by Decomposition Factors}
\usage{
facmul(x, factor, y, transpose, left, \dots)
}
\alias{facmul}
\alias{facmul.default}
\description{
    Performs multiplication by factors for certain decompositions (and
    allows explicit formation of those factors).
}
\arguments{
    \item{x}{
	a matrix decomposition. 
	No missing values or IEEE special values are allowed.
    }
    \item{factor}{
	an indicator for selecting a particular factor for multiplication.
    }
    \item{y}{
	a matrix or vector to be multiplied by the factor or its transpose.
	No missing values or IEEE special values are allowed.
    }
    \item{transpose}{
	a logical value.  When \code{FALSE} (the default) the factor is
	applied.  When \code{TRUE} the transpose of the factor is applied.
    }
    \item{left}{
	a logical value.  When \code{TRUE} (the default) the factor is
	applied from the left.  When \code{FALSE} the factor is applied
	from the right.
    }
    \item{...}{
	the method for \code{"qr.Matrix"} has additional arguments.
    }
}
\value{
    the product of the selected factor (or its transpose) and \code{y}
}
\section{NOTE}{
    Factors for decompositions such as \code{lu} and \code{qr} can be
    stored in a compact form.
    The function \code{facmul} allows multiplication without explicit
    formation of the factors, saving both storage and operations.
}
\references{
Golub, G., and Van Loan, C. F. (1989).
\emph{Matrix Computations,}
2nd edition, Johns Hopkins, Baltimore.
}
%  \seealso{
%      \code{\link{facmul.lu.Matrix}}, \code{\link{facmul.lu.Hermitian}}, \code{\link{facmul.qr.Matrix}}, \code{\link{expand}}.
%  }
\examples{
library(Matrix)
x <- Matrix(rnorm(9), 3, 3)
\dontrun{
qrx <- qr(x)                      # QR factorization of x
y <- rnorm(3)
facmul( qr(x), factor = "Q", y)   # form Q y
}
}
\keyword{array}
\keyword{algebra}
