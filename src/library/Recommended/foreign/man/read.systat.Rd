% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

% Original copyright 2004 by Roger Bivand
% Changes for the foreign package (C) 2004 R Development Core Team

\name{read.systat}
\alias{read.systat}
\title{Obtain a Data Frame from a Systat File}
\description{
  \code{read.systat} reads a rectangular data file stored by the Systat
  \code{SAVE} command as (legacy) \code{*.sys} or more recently
  \code{*.syd} files.
}
\usage{
read.systat(file, to.data.frame = TRUE)
}
\arguments{
  \item{file}{character variable with the name of the file to read}
  \item{to.data.frame}{return a data frame (otherwise a list)}
}
\details{
  The function only reads those Systat files that are rectangular data
  files (\code{mtype = 1}), and warns when files have non-standard
  variable name codings.  The files tested were produced on MS-DOS and
  Windows: files for the Mac version of Systat have a completely
  different format.

  The C code was originally written for an add-on module for Systat
  described in Bivand (1992 paper).  Variable names retain the trailing
  dollar in the list returned when \code{to.data.frame} is \code{FALSE},
  and in that case character variables are returned as is and filled up
  to 12 characters with blanks on the right.  The original function was
  limited to reading Systat files with up to 256 variables (a Systat
  limitation); it will now read up to 8192 variables.

  If there is a user comment in the header this is returned as attribute
  \code{"comment"}.  Such comments are always a multiple of 72
  characters (with a maximum of 720 chars returned), normally padded with
  trailing spaces.
}

\value{
  A data frame (or list) with one component for each variable in the
  saved data set.
}

\references{
  Systat Manual, 1987, 1989\cr
 
  Bivand, R. S. (1992)
  SYSTAT-compatible software for modelling spatial dependence among
  observations. \emph{Computers and Geosciences} \bold{18}, 951--963.
}
\author{Roger Bivand}

\examples{
summary(iris)
iris.s <- read.systat(system.file("files/Iris.syd", package="foreign")[1])
str(iris.s)
summary(iris.s)
}
\keyword{file}
