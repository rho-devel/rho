


### Copyright (C) 2005-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org> 
###
### This file is part of the lattice package for R.
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
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



make.groups <- function(...)
{
    tmp <- list(...)
    nms <- as.character(substitute(list(...)))[-1]
    if (is.null(names(tmp)))
        names(tmp) <- nms
    else
    {
        unnamed <- names(tmp) == ""
        names(tmp)[unnamed] <- nms[unnamed]
    }
    if (all(sapply(tmp, is.data.frame)))
    {
        cbind(do.call(rbind, tmp),
              which =
              rep(gl(length(tmp), 1, labels = names(tmp)),
                  sapply(tmp, nrow)))
    }
    else 
        data.frame(data = unlist(tmp),
                   which =
                   rep(gl(length(tmp), 1, labels = names(tmp)),
                       sapply(tmp, length)))
}


