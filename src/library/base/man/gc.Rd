\name{gc}
\title{Garbage Collection (adapted for CXXR)}
\usage{
gc(verbose = getOption("verbose"), reset=FALSE)
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
  \item{verbose}{logical; if \code{TRUE}, the garbage collection prints
    statistics about nodes and the total memory allocated via CXXR's
    internal heap.}
  \item{reset}{logical; if \code{TRUE} the values for maximum space used
    are reset to the current values.}
}
\description{
  A call of \code{gc} causes a garbage collection to take place.
  \code{gcinfo} sets a flag so that
  automatic collection is either silent (\code{verbose=FALSE}) or
  prints memory usage statistics (\code{verbose=TRUE}).
}
\details{
  A call of \code{gc} causes a garbage collection to take place.
  This will also take place automatically without user intervention, and the
  primary purpose of calling \code{gc} is for the report on memory usage.

  However, it can be useful to call \code{gc} after a large object
  has been removed, as this may prompt \R to return memory to the
  operating system.

  When \code{gcinfo(TRUE)} is in force, messages are sent to the message
  connection at each garbage collection of the form
  \preformatted{
    Garbage collection 12 = 10+0+2 (level 0) ...
    6.4 Mbytes of cons cells used (58%)
    2.0 Mbytes of vectors used (32%)
  }
  (Subject to change for CXXR: FIXME.)  Here the last two lines give
  the current memory usage rounded up to the next 0.1Mb and as a
  percentage of the current trigger value.  The first line gives a
  breakdown of the number of garbage collections at various levels
  (for an explanation see the \sQuote{R Internals} manual).
}

\value{
  \code{gc} returns a matrix with columns \code{"Nodes"} and
  \code{"Mbytes"} (total memory allocated via CXXR's internal heap,
  rounded up to next 0.1 MB), and rows \code{"used"} and \code{"gc trigger"}. 

  If maxima have been set for either \code{"Nodes"} or total memory,
  a third row is printed giving the current limits (with \code{NA}
  denoting no limit). 

  The final row shows the maximum node count and space used since the
  last call to \code{gc(reset=TRUE)} (or since \R started).
  
  \code{gcinfo} returns the previous value of the flag.
}
\seealso{
  The \sQuote{R Internals} manual.

  \code{\link{Memory}} on \R's memory management,
  and \code{\link{gctorture}} if you are an \R developer.
 
  \code{\link{reg.finalizer}} for actions to happen at garbage
  collection.
}
\examples{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x,i)
gcinfo(verbose = FALSE)#-- don't show it anymore

gc(TRUE)

gc(reset=TRUE)
}
\keyword{environment}
