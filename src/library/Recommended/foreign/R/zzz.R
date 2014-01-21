### This file is part of the `foreign' package for R.

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

.onLoad <- function(lib, pkg) .C(spss_init)

.onUnload <- function(libpath) library.dynam.unload("foreign", libpath)

.noGenerics <- TRUE
