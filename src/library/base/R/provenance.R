# rho $Id$
# rho
# rho This file is part of Rho, a project to refactor the R interpreter
# rho into C++.  It may consist in whole or in part of program code and
# rho documentation taken from the R project itself, incorporated into
# rho Rho (and possibly MODIFIED) under the terms of the GNU General Public
# rho Licence.
# rho 
# rho Rho is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
# rho copyrights and copyright restrictions as may be stated below.
# rho 
# rho Rho is not part of the R project, and bugs and other issues should
# rho not be reported via r-bugs or other R project channels; instead refer
# rho to the Rho website.

provenance.graph <- function(names) {
  ans <- .Internal(provenance.graph(names))
  names(ans) <- c("symbols", "commands", "timestamps", "xenogenous", "values",
                  "parents", "children")
  ans$timestamps <- as.POSIXct(ans$timestamps, origin = "1970-01-01",
        tz = "GMT")
  ans
}

pedigree <- function(names) {
  pg <- provenance.graph(names)
  pg$commands[!duplicated(pg$timestamps)]
}
