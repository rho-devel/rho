### Note that "in theory" even base::as.vector() should be overloaded.
### In practice that could be too much of a performance penalty in some cases.

.MatrixEnv <- new.env(parent=emptyenv())
## as long as it's small, no 'hash = TRUE'

.chm_common <- new.env(parent = emptyenv())
## environment in which to store some settings from cholmod_common

## A wrapper for now [as long as  'methods' has no *exported* version]:
.M.classEnv <- function(Class) methods:::.classEnv(Class)

.onLoad <- function(libname, pkgname)
{

    ## GOAL: all the functions in 'base' that start with something like
    ##	"x <- as.matrix(x)" or	"X <- as.array(X)"
    ## will work for 'Matrix'-matrices :

    if(getRversion() < "2.15.0") {
        assignInNamespace("..Old.._x_",       base::`%x%`,    ns = "base")

        ## kronecker() / %x% -- in principle should re-assign base::kronecker
        ## -----------> ?? performance hit ?? in mantelhaen.test() ??
        ##
        ## This is formally identical to the base definition, but should use the
        ## generic kronecker (and hence our kronecker() methods for our matrices):
        assignInNamespace("%x%", function (X, Y) kronecker(X, Y), ns = "base")
    }

    ## Hack needed, as C-level  eval / findFun seems not to work with
    ## loaded & non-attached Matrix:
    assignInNamespace(".M.classEnv", .M.classEnv, ns = "base")

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
    if(getRversion() < "2.15.0")
        assignInNamespace("%x%",       base::..Old.._x_,       ns = "base")

    library.dynam.unload("Matrix", libpath)
}
