.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("nnet", libpath)
