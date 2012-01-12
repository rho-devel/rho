.onUnload <- function(libpath)
{
    library.dynam.unload("cluster", libpath)
}

## no S4 methodology here; speedup :
.noGenerics <- TRUE
