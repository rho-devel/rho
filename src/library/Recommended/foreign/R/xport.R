### This file is part of the 'foreign' package for R.

###
###             Read SAS xport format libraries
###
### Copyright 1999-1999 Douglas M. Bates <bates$stat.wisc.edu>,
###                     Saikat DebRoy <saikat$stat.wisc.edu>
###
### This file is part of the `foreign' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, a copy is available at
### http://www.r-project.org/Licenses/

lookup.xport <- function(file) .Call(xport_info, file)


read.xport <- function(file) {
    data.info <- lookup.xport(file)
    ans <- .Call(xport_read, file, data.info)
    if (length(ans) == 1L) as.data.frame(ans[[1L]])
    else lapply(ans, as.data.frame)
}
