% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.mtp}
\alias{read.mtp}
\title{Read a Minitab Portable Worksheet}
\description{
  Return a list with the data stored in a file as a Minitab Portable
  Worksheet.
}
\usage{
read.mtp(file)
}
\arguments{
  \item{file}{character variable with the name of the file to read.  The
    file must be in Minitab Portable Worksheet format.}
}
\value{
  A list with one component for each column, matrix, or constant stored
  in the Minitab worksheet.
}
\note{
  This function was written around 1990 for the format current
  then. Later versions of Minitab appear to have added to the format.
}
\references{
  \url{http://www.minitab.com/}
}
\author{Douglas M. Bates}
\examples{
\dontrun{
read.mtp("ex1-10.mtp")
}
}
\keyword{file}
