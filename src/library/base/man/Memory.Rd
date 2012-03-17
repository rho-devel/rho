% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{Memory}
\alias{Memory}
\alias{mem.limits}
\title{Memory Available for Data Storage (adapted for CXXR)}
\description{
  How \R manages its workspace.
  
  \code{mem;limits} is deprecated as of \R 2.14.0
}
\usage{
mem.limits(nsize = NA, vsize = NA)
}
\arguments{
  \item{vsize}{Ignored in CXXR.}
  \item{nsize}{Ignored in CXXR.}
}
\details{
  \R has a variable-sized workspace which is sized automatically.  These
  limits are mainly historical, and now used only exceptionally, and not
  at all in CXXR.
#ifdef windows
  
  On Windows the \option{--max-mem-size} option sets the maximum memory
  allocation: it has a minimum allowed value of 32M.  This is intended
  to catch attempts to allocate excessive amounts of memory which may
  cause other processes to run out of resources.  See also
  \code{\link{memory.limit}}.
#endif



  You can find out the current memory consumption (the MemoryBank
  allocation and number of nodes) by typing \code{\link{gc}()} at the
  \R prompt.  Note that following \code{\link{gcinfo}(TRUE)}, automatic
  garbage collection always prints memory use statistics.
  The function \code{mem.limits} is deprecated: use \code{\link{gc}}
  instead, which reports the limits in the exceptional case that they are
  set.
  
}

\value{
  \code{mem.limits()} returns a numeric vector giving the current
  settings of the maxima; in CXXR these are always \code{NA}.
}

\seealso{
  \emph{An Introduction to R} for more command-line options.

  \code{\link{Memory-limits}} for the design limitations.

  \code{\link{gc}} for information on the garbage collector and total
  memory usage,  \code{\link{object.size}(a)} for the (approximate)
  size of \R object \code{a}.  \code{\link{memory.profile}} for
  profiling the usage of nodes.
#ifdef windows

  \code{\link{memory.size}} to monitor total memory usage,
  \code{\link{memory.limit}} for the current limit.
#endif
}

\keyword{environment}
