\name{externalFormats}
\alias{readHB}
\alias{readMM}
% \alias{writeHB}
\alias{writeMM}
\alias{writeMM,CsparseMatrix-method}
\alias{writeMM,TsparseMatrix-method}
\title{Read and write external matrix formats}
\description{
  Read matrices stored in the Harwell-Boeing or MatrixMarket formats
  or write \code{\linkS4class{sparseMatrix}} objects to one of these
  formats.
}
\usage{
readHB(file)
readMM(file)
writeMM(obj, file, ...)
}
\arguments{
 \item{obj}{a real sparse matrix}
 \item{file}{for \code{writeMM} - the name of the file to be written.
   For \code{readHB} and \code{readMM} the name of the file to read, as
   a character scalar.  The names of files storing matrices in the
   Harwell-Boeing format usually end in \code{".rua"} or \code{".rsa"}.
   Those storing matrices in the MatrixMarket format usually end in
   \code{".mtx"}.

   Alternatively, \code{readHB} and \code{readMM} accept connection objects.}
 \item{\dots}{optional additional arguments. Currently none are used in
   any methods.}
}
\value{
  The \code{readHB} and \code{readMM} functions return an object that
  inherits from the \code{"\linkS4class{Matrix}"} class.  Methods for the
  \code{writeMM} generic functions usually return
  \code{\link{NULL}} and, as a side effect, the matrix \code{obj} is
  written to \code{file} in the MatrixMarket format (writeMM).
}
\note{
  The Harwell-Boeing format is older and less flexible than the
  MatrixMarket format.  The function \code{writeHB} was deprecated and
  has now been removed.  Please use \code{writeMM} instead.

  A very simple way to export small sparse matrices \code{S}, is to use
  \code{summary(S)} which returns a \code{\link{data.frame}} with
  columns \code{i}, \code{j}, and possibly \code{x}, see \code{summary} in
  \code{\link{sparseMatrix-class}}, and an example below.
}
\references{
  \url{http://math.nist.gov/MatrixMarket}

  \url{http://www.cise.ufl.edu/research/sparse/matrices}
}
\examples{
str(pores <- readMM(system.file("external/pores_1.mtx",
                                package = "Matrix")))
str(utm <- readHB(system.file("external/utm300.rua",
                               package = "Matrix")))
str(lundA <- readMM(system.file("external/lund_a.mtx",
                                package = "Matrix")))
str(lundA <- readHB(system.file("external/lund_a.rsa",
                                package = "Matrix")))
\dontrun{
## NOTE: The following examples take quite some time
## ----  even on a fast internet connection:
if(FALSE) # the URL has been corrected, but we need an un-tar step!
str(sm <-
 readHB(gzcon(url("http://www.cise.ufl.edu/research/sparse/RB/Boeing/msc00726.tar.gz"))))

str(jgl009 <-
 readMM(gzcon(url("ftp://math.nist.gov/pub/MatrixMarket2/Harwell-Boeing/counterx/jgl009.mtx.gz"))))
}
data(KNex)
writeMM(KNex$mm, "mmMM.mtx")

## very simple export - in triplet format - to text file:
data(CAex)
s.CA <- summary(CAex)
message("writing to ", outf <- tempfile())
write.table(s.CA, file = outf, row.names=FALSE)
## and read it back -- showing off  sparseMatrix():
dd <- read.table(outf, header=TRUE)
mm <- do.call(sparseMatrix, dd)
stopifnot(all.equal(mm, CAex, tol=1e-15))
}
\keyword{IO}
\keyword{array}
\keyword{algebra}

