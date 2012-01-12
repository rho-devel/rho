
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
    lattice.setStatus(.defaultLatticeStatus())
}

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice", libpath)




## If present, .First.lib will be used if the NAMESPACE file is
## missing.  This is useful during development, thanks to C-c C-l in
## Emacs/ESS. It won't be used if a NAMESPACE file is present.  Note:
## Due to registration of C functions done in the NAMESPACE file,
## wireframe (and possibly cloud) won't work in this scenario.


.First.lib <- function(lib, pkg) 
{
    cat(gettext("Note: you shouldn't be seeing this message unless\nyou are using a non-standard version of lattice"),
        fill = TRUE)
    library.dynam("lattice", pkg, lib )
    ## having the next line causes a warning from R CMD check
    ## if (!require("grid")) stop("The grid package couldn't be loaded.\nPlease check your installation of R")
    haveGrid <- eval(parse(text = "require(grid)"))
    if (!haveGrid) stop("The grid package couldn't be loaded.\nPlease check your installation of R")
    lattice.options(.defaultLatticeOptions())
    lattice.options(experimentalOptions())
    lattice.setStatus(.defaultLatticeStatus())
}





