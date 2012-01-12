% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{write.dta}
\alias{write.dta}
\title{Write Files in Stata Binary Format}
\usage{
write.dta(dataframe, file, version = 7L,
          convert.dates = TRUE, tz = "GMT",
          convert.factors = c("labels", "string", "numeric", "codes"))
}
\arguments{
  \item{dataframe}{a data frame.}
  \item{file}{character string giving filename.}
  \item{version}{integer: Stata version: 6, 7, 8 and 10 are supported,
    and 9 is mapped to 8, 11 to 10.}
  \item{convert.dates}{Convert \code{Date} and \code{POSIXt} objects
    to Stata dates?}
  \item{tz}{timezone for date conversion}
  \item{convert.factors}{how to handle factors}
  } 
\description{
  Writes the data frame to file in the Stata binary
  format.  Does not write array variables unless they can be
  \code{\link{drop}}-ed to a vector.
}
\details{
  The major differences between file formats in Stata versions is that
  version 7.0 and later allow 32-character variable names (5 and 6 were
  restricted to 8-character name).  The \code{abbreviate} function is
  used to trim long variables to the permitted length.  A warning is
  given if this is needed and it is an error for the abbreviated names
  not to be unique.

  The columns in the data frame become variables in the Stata data set.
  Missing values are handled correctly.

  Unless deselected by argument \code{convert.dates}, \R date and
  date-time objects (\code{POSIXt} classes) are converted into the Stata
  format.  For date-time objects this may lose information -- Stata
  dates are in days since 1960-1-1.  \code{POSIXct} objects can be
  written without conversion but will not be understood as dates by
  Stata; \code{POSIXlt} objects cannot be written without conversion.

  There are four options for handling factors.  The default is to use
  Stata \sQuote{value labels} for the factor levels.  With
  \code{convert.factors="string"}, the factor levels are written as
  strings.  With \code{convert.factors="numeric"} the numeric values of
  the levels are written, or \code{NA} if they cannot be coerced to
  numeric.  Finally, \code{convert.factors="codes"} writes the
  underlying integer codes of the factors.  This last used to be the
  only available method and is provided largely for backwards
  compatibility.

  For Stata 8 or later use the default \code{version=7} -- the only
  advantage of Stata 8 format is that it can represent multiple
  different missing value types, and \R doesn't have them.  Stata 10/11
  allows longer format lists, but \R does not make use of them.

  Note that the Stata formats are documented to use ASCII strings --
  \R does not enforce this, but use of non-ASCII character strings will
  not be portable as the encoding is not recorded.  Up to 244 bytes are
  allowed in character data, and longer strings will be truncated with a
  warning.

  Stata uses some large numerical values to represent missing
  values.  This function does not currently check, and hence integers
  greater than \code{2147483620} and doubles greater than
  \code{8.988e+307} may be misinterpreted by Stata.
}
\value{
  \code{NULL}
}
\references{
  Stata 6.0 Users Manual, Stata 7.0 Programming manual, Stata online
  help (version 8 and later, also \url{http://www.stata.com/help.cgi?dta}
  and \url{http://www.stata.com/help.cgi?dta_113}) describe the file formats.
} 
\author{Thomas Lumley and R-core members}
\seealso{
  \code{\link{read.dta}},
  \code{\link{attributes}},
  \code{\link{DateTimeClasses}},
  \code{\link{abbreviate}}
}
\examples{
write.dta(swiss, swissfile <- tempfile())
read.dta(swissfile)
}
\keyword{file}
