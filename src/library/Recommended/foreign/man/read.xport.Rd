% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.xport}
\alias{read.xport}
\title{Read a SAS XPORT Format Library}
\description{
  Reads a file as a SAS XPORT format library and returns a list of
  data.frames.
}
\usage{
read.xport(file)
}
\arguments{
 \item{file}{character variable with the name of the file to read.  The
   file must be in SAS XPORT format.}
}
\value{
  If there is a more than one dataset in the XPORT format library, a named
  list of data frames, otherwise a data frame.  The columns of the data
  frames will be either numeric (corresponding to numeric in SAS) or
  factor (corresponding to character in SAS).  All SAS numeric missing
  values (including special missing values represented by \code{._},
  \code{.A} to \code{.Z} by SAS) are mapped to \R \code{NA}.

  Trailing blanks are removed from character columns before conversion to
  a factor.  Some sources claim that character missing values in SAS are
  represented by \code{' '} or \code{''}: these are not treated as \R
  missing values.
}
\references{
  SAS Technical Support document TS-140:
  ``The Record Layout of a Data Set in SAS Transport (XPORT) Format''
  available at
  \url{http://ftp.sas.com/techsup/download/technote/ts140.html}.
}
\author{Saikat DebRoy \email{saikat@stat.wisc.edu}}
\seealso{
  \code{\link{lookup.xport}}
}
\examples{
\dontrun{
read.xport("transport")
}
}
\keyword{file}
