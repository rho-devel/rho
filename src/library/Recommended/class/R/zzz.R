.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("class", libpath)
