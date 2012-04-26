.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("MASS", libpath)
