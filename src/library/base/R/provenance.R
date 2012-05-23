#  File src/library/utils/R/withVisible.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

withVisible <- function(x) .Internal(withVisible(x))

# CXXR $Id$
# CXXR
# CXXR This file is part of CXXR, a project to refactor the R interpreter
# CXXR into C++.  It may consist in whole or in part of program code and
# CXXR documentation taken from the R project itself, incorporated into
# CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
# CXXR Licence.
# CXXR 
# CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
# CXXR copyrights and copyright restrictions as may be stated below.
# CXXR 
# CXXR CXXR is not part of the R project, and bugs and other issues should
# CXXR not be reported via r-bugs or other R project channels; instead refer
# CXXR to the CXXR website.

pedigree <- function(names) {
  ans <- .Internal(pedigree(names))
  names(ans) <- c("commands", "timestamps", "symbols")
  ans$timestamps <- as.POSIXct(ans$timestamps, origin = "1970-01-01",
        tz = "GMT")
  ans
}

