% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.arff}
\alias{read.arff}
\title{Read Data from ARFF Files}
\description{
  Reads data from Weka Attribute-Relation File Format (ARFF) files.
}
\usage{
read.arff(file)
}
\arguments{
  \item{file}{a character string with the name of the ARFF file to read
    from, or a \code{\link{connection}} which will be opened if
    necessary, and if so closed at the end of the function call.}
}
\value{
  A data frame containing the data from the ARFF file.
}
\references{
  Attribute-Relation File Format
  \url{http://www.cs.waikato.ac.nz/~ml/weka/arff.html}\cr
  \url{http://weka.sourceforge.net/wekadoc/index.php/en:ARFF_(3.5.1)}
}
\seealso{
  \code{\link{write.arff}}
}
\keyword{file}
\keyword{connection}
