% file MASS/man/write.matrix.Rd
% copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
%
\name{write.matrix}
\alias{write.matrix}
\title{
Write a Matrix or Data Frame
}
\description{
  Writes a matrix or data frame to a file or the console, using column
  labels and a layout respecting columns.
}
\usage{
write.matrix(x, file = "", sep = " ", blocksize)
}
\arguments{
\item{x}{
  matrix or data frame.
}
\item{file}{
  name of output file. The default (\code{""}) is the console.
}
\item{sep}{
  The separator between columns.
}
\item{blocksize}{
  If supplied and positive, the output is written in blocks of
  \code{blocksize} rows.  Choose as large as possible consistent with
  the amount of memory available.
}}
\details{
  If \code{x} is a matrix, supplying \code{blocksize} is more
  memory-efficient and enables larger matrices to be written, but each
  block of rows might be formatted slightly differently.

  If \code{x} is a data frame, the conversion to a matrix may negate the
  memory saving.
}
\section{Side Effects}{
  A formatted file is produced, with column headings (if \code{x} has them)
  and columns of data.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link{write.table}}
}
\keyword{file}
\keyword{print}
