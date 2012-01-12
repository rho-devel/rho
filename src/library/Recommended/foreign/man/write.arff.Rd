% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{write.arff}
\alias{write.arff}
\title{Write Data into ARFF Files}
\description{
  Writes data into Weka Attribute-Relation File Format (ARFF) files.
}
\usage{
write.arff(x, file, eol = "\n", relation = deparse(substitute(x)))
}
\arguments{
  \item{x}{the data to be written, preferably a matrix or data frame.
    If not, coercion to a data frame is attempted.}
  \item{file}{either a character string naming a file, or a connection.
    \code{""} indicates output to the standard output connection.}
  \item{eol}{the character(s) to print at the end of each line (row).}
  \item{relation}{The name of the relation to be written in the file.}
}
\details{
  \code{relation} will be passed through \code{\link{make.names}} before
  writing to the file, in an attempt to it them acceptable to Weka, and
  column names what do not start with an alphabetic character will have
  \code{X} prepended.
  
  However, the references say that ARFF files are ASCII files, and that
  encoding is not enforced.
}
\references{
  Attribute-Relation File Format
  \url{http://www.cs.waikato.ac.nz/~ml/weka/arff.html}\cr
  \url{http://weka.sourceforge.net/wekadoc/index.php/en:ARFF_(3.5.1)}
}  
\seealso{
  \code{\link{read.arff}}
}
\examples{
write.arff(iris, file = "")
}
\keyword{print}
\keyword{file}
