### Note that "in theory" even base::as.vector() should be overloaded.
### In practice that could be too much of a performance penalty in some cases.

.MatrixEnv <- new.env(parent=emptyenv())
## as long as it's small, no 'hash = TRUE'

.chm_common <- new.env(parent = emptyenv())
## environment in which to store some settings from cholmod_common

.onLoad <- function(libname, pkgname)
{
    .Call(CHM_set_common_env, .chm_common)
}

## Instead, simply re-assign the [cr]bind()s which are recursively
## based on [cr]bind2 :
##
## save to cBind / rBind  ("rename")
cBind <- methods:::cbind
rBind <- methods:::rbind

.onUnload <- function(libpath)
{
    library.dynam.unload("Matrix", libpath)
}
