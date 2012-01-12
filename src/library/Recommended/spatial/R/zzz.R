.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("spatial", libpath)

if(getRversion() < "2.6.0") nzchar <- function(x) nchar(x, "b") > 0
