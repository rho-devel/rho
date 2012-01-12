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
    require(methods)
    require(utils) # -> assignInNamespace {but "anyway"}

    ## GOAL: all the functions in 'base' that start with something like
    ##	"x <- as.matrix(x)" or	"X <- as.array(X)"
    ## will work for 'Matrix'-matrices :

    ## works around namespace-protection on purpose:
    assignInNamespace("..Old..as.matrix", base::as.matrix, ns = "base")
    assignInNamespace("..Old..as.array",  base::as.array, ns = "base")
    assignInNamespace("..Old.._x_",       base::`%x%`,    ns = "base")

    ##  hack because base::as.matrix() is an S3 generic :
    tmp <- function(x, ...) if(isS4(x)) Matrix::as.matrix(x) else UseMethod("as.matrix")
    environment(tmp) <- baseenv()
    assignInNamespace("as.matrix", tmp,      ns = "base")
    assignInNamespace("as.array",  as.array, ns = "base")


    ## kronecker() / %x% -- in principle should re-assign base::kronecker
    ## -----------> ?? performance hit ?? in mantelhaen.test() ??
    ##
    ## This is formally identical to the base definition, but should use the
    ## generic kronecker
    assignInNamespace("%x%", function (X, Y) kronecker(X, Y), ns = "base")

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
    assignInNamespace("as.matrix", base::..Old..as.matrix, ns = "base")
    assignInNamespace("as.array",  base::..Old..as.array,  ns = "base")
    assignInNamespace("%x%",       base::..Old.._x_,       ns = "base")

    library.dynam.unload("Matrix", libpath)
}

if(getRversion() < "2.12.0") { ## ...../R/src/library/base/R/which.R :
arrayInd <- function(ind, .dim, .dimnames = NULL, useNames = FALSE) {
    ##-- return a matrix  length(ind) x rank == length(ind) x length(.dim)
    m <- length(ind)
    rank <- length(.dim)
    wh1 <- ind - 1L
    ind <- 1L + wh1 %% .dim[1L]
    ind <- matrix(ind, nrow = m, ncol = rank,
		  dimnames = if(useNames)
		  list(.dimnames[[1L]][ind],
		       if(rank == 2L) c("row", "col") # for matrices
		       else paste("dim", seq_len(rank), sep="")))
    if(rank >= 2L) {
	denom <- 1L
	for (i in 2L:rank) {
	    denom <- denom * .dim[i-1L]
	    nextd1 <- wh1 %/% denom	# (next dim of elements) - 1
	    ind[,i] <- 1L + nextd1 %% .dim[i]
	}
    }
    storage.mode(ind) <- "integer"
    ind
}

}
