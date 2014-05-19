% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.spss}
\alias{read.spss}
\title{Read an SPSS Data File}
\description{
  \code{read.spss} reads a file stored by the SPSS \code{save} or
  \code{export} commands.
}
\usage{
read.spss(file, use.value.labels = TRUE, to.data.frame = FALSE,
          max.value.labels = Inf, trim.factor.names = FALSE,
          trim_values = TRUE, reencode = NA, use.missings = to.data.frame)
}
\arguments{
  \item{file}{Character string: the name of the file or URL to read.}
  \item{use.value.labels}{Convert variables with value labels into \R
    factors with those levels?}
  \item{to.data.frame}{return a data frame?}
  \item{max.value.labels}{Only variables with value labels and at most this
    many unique values will be converted to factors if
    \code{use.value.labels = TRUE}.}
  \item{trim.factor.names}{Logical: trim trailing spaces from factor levels?}
  \item{trim_values}{logical: should values and value labels have
    trailing spaces ignored when matching for \code{use.value.labels = TRUE}?}
  \item{reencode}{logical: should character strings be re-encoded to the
    current locale.  The default, \code{NA}, means to do so in a UTF-8
    locale, only.  Alternatively a character string specifying an encoding to
    assume for the file.}
  \item{use.missings}{logical: should information on user-defined
    missing values be used to set the corresponding values to
    \code{NA}?}
}
\value{
  A list (or data frame) with one component for each variable in
  the saved data set.

  If what looks like a Windows codepage was recorded in the SPSS file,
  it is attached (as a number) as attribute \code{"codepage"} to the
  result.

  There may be attributes \code{"label.table"} and
  \code{"variable.labels"}.  Attribute \code{"label.table"} is a named
  list of value labels with one element per variable, either \code{NULL}
  or a named character vector.
  Attribute \code{"variable.labels"} is a named character vector with
  names the short variable names and elements the long names.
  
  If there are user-defined missing values, there will be a attribute
  \code{"Missings"}.  This is a named list with one list element per
  variable.  Each element has an element \code{type}, a length-one
  character vector giving the type of missingness, and may also have an
  element \code{value} with the values corresponding to missingness.
  This is a complex subject (where the \R and C source code for
  \code{read.spss} is the main documentation), but the simplest cases
  are types \code{"one"}, \code{"two"} and \code{"three"} with a
  corresponding number of (real or string) values whose labels can be
  found from the \code{"label.table"} attribute.  Other possibilities are
  a finite or semi-infinite range, possibly plus a single value.
  See also \url{http://www.gnu.org/software/pspp/manual/html_node/Missing-Observations.html#Missing-Observations}.
}
\details{
  This uses modified code from the PSPP project
  (\url{http://www.gnu.org/software/pspp/} for reading the SPSS formats.

  If the filename appears to be a URL (of schemes \samp{http:},
  \samp{ftp:} or \samp{https:}) the URL is first downloaded to a
  temporary file and then read.  (\samp{https:} is only supported on
  some platforms.)
  
  Occasionally in SPSS, value labels will be added to some values of a
  continuous variable (e.g. to distinguish different types of missing
  data), and you will not want these variables converted to factors.  By
  setting \code{max.value.labels} you can specify that variables with a
  large number of distinct values are not converted to factors even if
  they have value labels.  In addition, variables will not be converted
  to factors if there are non-missing values that have no value label.
  The value labels are then returned in the \code{"value.labels"}
  attribute of the variable.
  
  If SPSS variable labels are present, they are returned as the
  \code{"variable.labels"} attribute of the answer.

  Fixed length strings (including value labels) are padded on the right
  with spaces by SPSS, and so are read that way by \R.  The default
  argument \code{trim_values=TRUE} causes trailing spaces to be ignored
  when matching to value labels, as examples have been seen where the
  strings and the value labels had different amounts of padding.  See
  the examples for \code{\link{sub}} for ways to remove trailing spaces
  in character data.
  
  URL \url{http://msdn.microsoft.com/en-us/library/ms776446(VS.85).aspx}
  provides a list of translations from Windows codepage numbers to
  encoding names that \code{\link{iconv}} is likely to know about and so
  suitable values for \code{reencode}.  Automatic re-encoding is
  attempted for apparent codepages of 200 or more in a UTF-8 locale:
  some other high-numbered codepages can be re-encoded on most systems,
  but the encoding names are platform-dependent (see
  \code{\link{iconvlist}}).
}
\note{
  If SPSS value labels are converted to factors the underlying numerical
  codes will not in general be the same as the SPSS numerical
  values, since the numerical codes in R are always \eqn{1,2,3,\dots}.

  You may see warnings about the file encoding for SPSS \code{save}
  files: it is possible such files contain non-ASCII character data
  which need re-encoding.  The most common occurrence is Windows codepage
  1252, a superset of Latin-1.  The encoding is recorded (as an integer)
  in attribute \code{"codepage"} of the result if it looks like a
  Windows codepage.  Automatic re-encoding is done only in UTF-8
  locales: see argument \code{reencode}.
}
\author{Saikat DebRoy and the R-core team}
\seealso{
  A different interface also based on the PSPP codebase is available in
  package \pkg{memisc}: see its help for \code{spss.system.file}.
}
\examples{
\dontrun{
read.spss("datafile")
## don't convert value labels to factor levels
read.spss("datafile", use.value.labels = FALSE)
## convert value labels to factors for variables with at most
## ten distinct values.
read.spss("datafile", max.value.labels = 10)
}
}
\keyword{file}
