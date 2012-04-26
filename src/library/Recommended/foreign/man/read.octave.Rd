% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

% Original copyright (c) 2004 Stephen Eglen
% Changes copyright (c) 2004-10 R Development Core Team

\name{read.octave}
\alias{read.octave}
\title{Read Octave Text Data Files}
\description{
  Read a file in Octave text data format into a list.
}
\usage{
read.octave(file)
}
\arguments{
  \item{file}{a character string with the name of the file to read.}
}
\details{
  This function is used to read in files in Octave text data format, as
  created by \code{save -text} in Octave.  It knows about most of the
  common types of variables, including the standard atomic (real and
  complex scalars, matrices, and \eqn{N}-d arrays, strings, ranges, and
  boolean scalars and matrices) and recursive (structs, cells, and
  lists) ones, but has no guarantee to read all types.  If a type is not
  recognized, a warning indicating the unknown type is issued, it is
  attempted to skip the unknown entry, and \code{NULL} is used as its
  value.  Note that this will give incorrect results, and maybe even
  errors, in the case of unknown recursive data types.

  As Octave can read MATLAB binary files, one can make the contents of
  such files available to R by using Octave's load and save (as text)
  facilities as an intermediary step.
}
\value{
  A list with one named component for each variable in the file.
}
\references{\url{http://www.octave.org/}}
\author{Stephen Eglen \email{stephen@gnu.org} and Kurt Hornik}
\keyword{file}
