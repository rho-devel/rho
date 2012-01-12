% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.epiinfo}
\alias{read.epiinfo}
\title{Read Epi Info Data Files}
\description{
  Reads data files in the \code{.REC} format used by Epi Info versions 6
  and earlier and by EpiData.  Epi Info is a public domain database and
  statistics package produced by the US Centers for Disease Control
  and EpiData is a freely available data entry and validation system.
  }
\usage{
read.epiinfo(file, read.deleted = FALSE, guess.broken.dates = FALSE,
             thisyear = NULL, lower.case.names = FALSE)
}
\arguments{
  \item{file}{A filename, URL, or connection.}
  \item{read.deleted}{Deleted records are read if \code{TRUE}, omitted
    if \code{FALSE} or replaced with \code{NA} if \code{NA}.}
  \item{guess.broken.dates}{Attempt to convert dates with 0 or 2 digit
    year information (see \sQuote{Details}).}
  \item{thisyear}{A 4-digit year to use for dates with no year. Defaults
    to the current year.}
  \item{lower.case.names}{Convert variable names to lowercase?}
}
\details{
  Epi Info allows dates to be specified with no year or with a 2 or 4
  digits.  Dates with four-digit years are always converted to
  \code{Date} class.  With the \code{guess.broken.dates} option the function
  will attempt to convert two-digit years using the operating system's
  default method (see \link{Date}) and will use the current
  year or the \code{thisyear} argument for dates with no year
  information.

  If \code{read.deleted} is \code{TRUE} the \code{"deleted"} attribute
  of the data frame indicates the deleted records.
}
\note{  Epi Info 2000, the current version, uses the Microsoft Access file
  format to store data. This may be readable with the \code{RODBC} or
  \code{RDCOM} packages.
}
\value{
  A data frame.
}
\references{
  \url{http://www.cdc.gov/epiinfo/},
  \url{http://www.epidata.dk}
}

\seealso{
  \link{DateTimeClasses}
}

\examples{
\dontrun{
data<-read.epiinfo("oswego.rec",guess.broken.dates=TRUE,thisyear="1972")
}
}
\keyword{ file }
