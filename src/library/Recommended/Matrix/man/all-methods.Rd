\name{all-methods}
\docType{methods}
\alias{all-methods}
\alias{all,Matrix-method}
\alias{any,Matrix-method}
% \alias{all,lMatrix-method}
\alias{all,ldenseMatrix-method}
\alias{all,lsparseMatrix-method}
\alias{all,lsyMatrix-method}
%
\alias{any,lMatrix-method}
% \alias{any,ldenseMatrix-method}
% \alias{any,lsparseMatrix-method}
%
\title{"Matrix" Methods for Functions all() and any()}
\description{
  The basic \R functions \code{\link{all}} and \code{\link{any}} now
  have methods for \code{\linkS4class{Matrix}} objects and should
  behave as for \code{\link{matrix}} ones.
}
\section{Methods}{
%% FIXME: write more
  \describe{
    \item{all}{\code{signature(x = "Matrix", ..., na.rm = FALSE)}: ...}
    \item{any}{\code{signature(x = "Matrix", ..., na.rm = FALSE)}: ...}

    \item{all}{\code{signature(x = "ldenseMatrix", ..., na.rm = FALSE)}: ...}
    \item{all}{\code{signature(x = "lsparseMatrix", ..., na.rm = FALSE)}: ...}
  }
}
\keyword{methods}
\examples{
M <- Matrix(1:12 +0, 3,4)
all(M >= 1) # TRUE
any(M < 0 ) # FALSE
MN <- M; MN[2,3] <- NA; MN
all(MN >= 0) # NA
any(MN <  0) # NA
any(MN <  0, na.rm = TRUE) # -> FALSE
\dontshow{
sM <- as(MN, "sparseMatrix")
stopifnot(all(M >= 1), !any(M < 0),
          all.equal((sM >= 1), as(MN >= 1, "sparseMatrix")),
          ## MN:
          any(MN < 2), !all(MN < 5),
          is.na(all(MN >= 0)), is.na(any(MN < 0)),
          all(MN >= 0, na.rm=TRUE), !any(MN < 0, na.rm=TRUE),
          ## same for sM :
          any(sM < 2), !all(sM < 5),
          is.na(all(sM >= 0)), is.na(any(sM < 0)),
          all(sM >= 0, na.rm=TRUE), !any(sM < 0, na.rm=TRUE)
         )
}
}
