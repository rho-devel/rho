% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{Memory}
\alias{Memory}
\alias{mem.limits}
\title{Memory Available for Data Storage (adapted for CXXR)}
\description{
  Use command line options to control the memory available for \R.
}
\usage{
#ifdef unix
\special{R --min-vsize=vl --max-vsize=vu --min-nsize=nl --max-nsize=nu \\
  --max-ppsize=N}
#endif
#ifdef windows
\special{Rgui --min-vsize=vl --max-vsize=vu --min-nsize=nl --max-nsize=nu \\
  --max-ppsize=N}
\special{Rterm --min-vsize=vl --max-vsize=vu --min-nsize=nl --max-nsize=nu \\
  --max-ppsize=N}
#endif

mem.limits(nsize = NA, vsize = NA)
}
\arguments{
  \item{vl, vu, vsize}{Heap memory in bytes.}
  \item{nl, nu, nsize}{Number of nodes.}
  \item{N}{Number of nested \code{PROTECT} calls.}.
}
\details{
  \R has a variable-sized workspace.  There is
  much less need to set memory options than prior to \R 1.2.0, and most
  users will never need to set these.  They are provided both as a way
  to control the overall memory usage (which can also be done
  by operating-system facilities such as \code{limit} on Unix and by
  using the command-line option \option{--max-mem-size} on Windows),
  and since setting larger values of the minima will make \R slightly
  more efficient on large tasks.

#ifdef windows
  (On Windows the \option{--max-mem-size} option sets the maximum memory
  allocation: it has a minimum allowed value of 32M.  This is intended
  to catch attempts to allocate excessive amounts of memory which may
  cause other processes to run out of resources.  The default is the
  smaller of the amount of physical RAM in the machine and 1.5Gb (or
  2.5Gb if the version of Windows in use allows 3Gb per user process).
  See also \code{\link{memory.limit}}.)
#endif

  To understand the options, one needs to know that CXXR allocates
  most of its memory via an internal mechanism implemented by class
  CXXR::MemoryBank (which in turn gets its memory using C++'s
  \code{::operator new}).  The amount of memory allocated via
  MemoryBank is used to decide when to carry out garbage collections.

  The \option{--*-nsize} and \option{--max-*} options are ignored in
  CXXR.  The \option{--min-vsize} options is described below.  All
  these options must be integers or integers followed by 
  \code{G}, \code{M}, \code{K}, or \code{k} meaning \code{Giga}
  (\eqn{2^{30} = 1073741824}) \emph{Mega} (\eqn{2^{20} = 1048576}),
  (computer) \emph{Kilo} (\eqn{2^{10} = 1024}), or regular \emph{kilo}
  (1000). 

  The \option{--min-vsize} option sets the minimal value for the heap
  size at which garbage collection will be triggered.  The specified
  value is also the initial trigger value, but thereafter \R will
  raise or lower the trigger value depending on usage, but never below
  this initial value.  The default value is currently a minimum
  trigger level of 6MB of heap allocation.

  In CR, maximum trigger levels can be changed during an \R session by
  calling \code{mem.limits}, but this command has no effect in
  CXXR. (If this is called with the default values, it reports the
  current settings.)  

  You can find out the current memory consumption (the MemoryBank
  allocation and number of nodes) by typing \code{\link{gc}()} at the
  \R prompt.

  In CR, the command-line option \option{--max-ppsize} controls the maximum
  size of the pointer protection stack, but it has no effect in CXXR.
}

\value{
  \code{mem.limits()} returns an integer vector giving the current
  settings of the maxima; in CXXR these are always \code{NA}.
}

\seealso{
  \emph{An Introduction to R} for more command-line options

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

\examples{
# Start R with 10MB of heap memory and 500k nodes, limit to
# 100Mb and 1M nodes.  (In CXXR only the first option has any effect)
\dontrun{
## Unix
R --min-vsize=10M --max-vsize=100M --min-nsize=500k --max-nsize=1M
}
}
\keyword{environment}
