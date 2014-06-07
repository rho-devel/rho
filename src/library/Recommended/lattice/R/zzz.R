
### Copyright (C) 2000-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>,
###
### This file is part of the lattice package for R.  It is made
### available under the terms of the GNU General Public License,
### version 2, or at your option, any later version, incorporated
### herein by reference.
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



.LatticeEnv <- new.env()
assign("lattice.status",  list(), envir = .LatticeEnv)
assign("lattice.theme",   list(), envir = .LatticeEnv)
assign("lattice.options", list(), envir = .LatticeEnv)
## assign("last.object",     NULL,   envir = .LatticeEnv)



experimentalOptions <- function()
    list(layout.heights =
         list(top.padding = list(x = 0.5, units = "char", data = NULL),
              main.key.padding = list(x = 0.5, units = "char", data = NULL),
              key.axis.padding = list(x = 0.5, units = "char", data = NULL),
              axis.xlab.padding = list(x = 0.5, units = "char", data = NULL),
              xlab.key.padding = list(x = 0.5, units = "char", data = NULL),
              key.sub.padding = list(x = 0.5, units = "char", data = NULL),
              bottom.padding = list(x = 0.5, units = "char", data = NULL)),
         layout.widths =
         list(left.padding = list(x = 0.5, units = "char", data = NULL),
              key.ylab.padding = list(x = 0.5, units = "char", data = NULL),
              ylab.axis.padding = list(x = 0.5, units = "char", data = NULL),
              axis.key.padding = list(x = 0.5, units = "char", data = NULL),
              right.padding = list(x = 0.5, units = "char", data = NULL))
         )



.onLoad <- function(libname, pkgname) 
{
    ## library.dynam("lattice", pkgname, libname )
    lattice.options(.defaultLatticeOptions())
    lattice.options(experimentalOptions())
    lattice.setStatus(.defaultLatticeStatus(), clean.first = TRUE)
}

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice", libpath)



