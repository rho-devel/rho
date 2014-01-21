% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Memory}
\alias{Memory}
\title{Memory Available for Data Storage}
\description{
  How \R manages its workspace.
}
\details{
  \R has a variable-sized workspace.  There are (rarely-used)
  command-line options to control its minimum size, but no longer any to
  control the maximum size.
#ifdef windows

  On Windows the \option{--max-mem-size} option (or environment variable
  \env{R_MAX_MEM_SIZE}) sets the maximum (virtual) memory allocation: it
  has a minimum allowed value of 32M.  This is intended to catch
  attempts to allocate excessive amounts of memory which may cause other
  processes to run out of resources.  See also \code{\link{memory.limit}}.
#endif



  You can find out the current memory consumption (the MemoryBank
  allocation and number of nodes) by typing \code{\link{gc}()} at the
  \R prompt.  Note that following \code{\link{gcinfo}(TRUE)}, automatic
  garbage collection always prints memory use statistics.
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
