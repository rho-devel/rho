#### symmetricMatrix : virtual class

setAs("denseMatrix", "symmetricMatrix",
      function(from) ##		           vvvv  do *check* symmetry
      .Call(dense_to_symmetric, from, "U", TRUE))
setAs("matrix", "symmetricMatrix",
      function(from) .Call(dense_to_symmetric, from, "U", TRUE))

### ----------- forceSymmetric() ----- *all* methods ------------------------

## forceSymmetric() coerces to "symmetricMatrix"  withOUT  testing
## ---------------- contrary to  as(M, <symmetric>)  which should only
## work when 'M' is a symmetric matrix __ in the sense of isSymmetric() __
## i.e., with allowing a little bit of asymmetric numeric fuzz:

setMethod("forceSymmetric", signature(x = "matrix", uplo="ANY"),
	  function(x, uplo)
	      .Call(dense_to_symmetric, x,
		    if(missing(uplo)) "U" else uplo, FALSE))


symCls <- names(getClass("symmetricMatrix")@subclasses)
for(cls in symCls) {
    ## When x is symmetric and uplo is missing, we keep 'uplo' from 'x':
    setMethod("forceSymmetric", signature(x = cls, uplo="missing"),
	      function(x, uplo) x)

    setMethod("forceSymmetric", signature(x = cls, uplo="character"),
	      function(x, uplo) {
		  if(uplo == x@uplo)
		      x
		  else ## uplo is "wrong" for x
		      t(x)
	      })
}

setMethod("forceSymmetric", signature(x = "denseMatrix", uplo="character"),
	  function(x, uplo) .Call(dense_to_symmetric, x, uplo, FALSE))
setMethod("forceSymmetric", signature(x = "denseMatrix", uplo="missing"),
	  function(x, uplo) {
	      uplo <- if(is(x, "triangularMatrix")) x@uplo else "U"
	      ## FIXME?	 diagU2N() ??
	      .Call(dense_to_symmetric, x, uplo, FALSE)
	  })

setMethod("forceSymmetric", signature(x="sparseMatrix"),
	  function(x, uplo) {
	      x <- as(x, "CsparseMatrix")
	      callGeneric()
	  })

forceCspSymmetric <- function(x, uplo, isTri = is(x, "triangularMatrix"))
{
    ## isTri ==> effectively *diagonal*
    if(isTri && x@diag == "U")
	x <- .Call(Csparse_diagU2N, x)
    if(missing(uplo))
	uplo <- if(isTri) x@uplo else "U"
    .Call(Csparse_general_to_symmetric, x, uplo)
}
setMethod("forceSymmetric", signature(x="CsparseMatrix"),
	  function(x, uplo) forceCspSymmetric(x, uplo))


setMethod("symmpart", signature(x = "symmetricMatrix"), function(x) x)
setMethod("skewpart", signature(x = "symmetricMatrix"), setZero)

###------- pack() and unpack() --- for *dense*  symmetric & triangular matrices:
packM <-  function(x, Mtype, kind, unpack) {
    cd <- getClassDef(cx <- class(x))
    if(extends(cd, "sparseMatrix"))
	stop(sprintf("(un)packing only applies to dense matrices, class(x)='%s'",
		     cx))
    if(!missing(kind) && kind == "symmetric") { ## use 'unpack' but not 'Mtype'
	## special treatment for positive definite ones:
	as(x, if(unpack) {
	    if(extends(cd, "dppMatrix")) "dpoMatrix"
	    else paste0(.M.kindC(cd), "syMatrix")
	} else { ## !unpack : "pack" :
	    if(extends(cd, "dpoMatrix")) "dppMatrix"
	    else paste0(.M.kindC(cd), "spMatrix")
	})
    } else as(x, paste0(.M.kindC(cd), Mtype))
}
setMethod("unpack", "symmetricMatrix", function(x, ...)
	  packM(x, kind="symmetric", unpack=TRUE))
setMethod("pack",   "symmetricMatrix", function(x, ...)
	  packM(x, kind="symmetric", unpack=FALSE))
setMethod("unpack", "triangularMatrix", function(x, ...) packM(x,"trMatrix"))
setMethod("pack",   "triangularMatrix", function(x, ...) packM(x,"tpMatrix"))
## to produce a nicer error message:
pckErr <- function(x, ...)
    stop(sprintf("(un)packing only applies to dense matrices, class(x)='%s'",
		 class(x)))
setMethod("unpack", "sparseMatrix", pckErr)
setMethod("pack",   "sparseMatrix", pckErr)
rm(pckErr)

##' pack (<matrix>)  -- smartly:
setMethod("pack", signature(x = "matrix"),
	  function(x, symmetric=NA, upperTri = NA, ...) {
	      if(is.na(symmetric)) ## must guess symmetric / triangular
		  symmetric <- isSymmetric.matrix(x)
	      if(symmetric) {
		  pack(.Call(dense_to_symmetric, x, "U", TRUE), ...)
	      } else { # triangular
		  ## isTriMat(..) : should still check fully (speed up ?) ..
		  if(isTr <- isTriMat(x, upper=upperTri)) {
		      uplo <- attr(isTr, "kind")
		      pack(new(paste0(.M.kind(x),"tpMatrix"),
			       x = x[indTri(nrow(x), upper=(uplo == "U"), diag=TRUE)],
			       Dim = dim(x), Dimnames = .M.DN(x), uplo = uplo), ...)
		  } else
		      stop("'x' is not symmetric nor triangular")
	      }
	  })

## {"traditional"} specific methods
setMethod("unpack", signature(x = "dspMatrix"),
          function(x, ...) as(x, "dsyMatrix"), valueClass = "dsyMatrix")
setMethod("unpack", signature(x = "dtpMatrix"),
          function(x, ...) as(x, "dtrMatrix"), valueClass = "dtrMatrix")
###


## autogenerate coercions
##  as(*,  "symmetricMatrix")
##  ~~~~~~~~~~~~~~~~~~~~~~~~~
## Basic problem:
## This should work at package install time when package:Matrix does not exist!
if(FALSE)
local({
    allCl <- getClasses("package:Matrix") ## << fails at install time!!!!
    clss <- allCl[sapply(allCl, extends, class2 = "Matrix")]
    virt <- sapply(clss, isVirtualClass)
    ## Now ensure coercions for all  non-virtual "Matrix" inheriting classes:
    for(cl in clss[!virt]) {
        cld <- getClassDef(cl)
        if(extends(cld, "symmetricMatrix"))
            cat("\tsymmetric:\t", cl,"\n")
        else if(extends(cld, "triangularMatrix"))
            cat("\ttriangular:\t", cl,"\n")
        else if(extends(cld, "diagonalMatrix"))
            cat("\tdiagonal:\t", cl,"\n")
        else {
            cat("do ",cl,"\n")
##             setAs(cl, "symmetricMatrix",
##                   function(from) as(from, ".s.Matrix"))
        }
    }## for
})
