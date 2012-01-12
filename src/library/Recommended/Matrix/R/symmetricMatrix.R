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

setMethod("forceSymmetric", signature(x="CsparseMatrix"),
	  function(x, uplo) {
	      isTri <- is(x, "triangularMatrix")
	      if (isTri && x@diag == "U")
		  x <- .Call(Csparse_diagU2N, x)
	      if(missing(uplo))
		  uplo <- if(isTri) x@uplo else "U"
	      .Call(Csparse_general_to_symmetric, x, uplo)
          })


setMethod("symmpart", signature(x = "symmetricMatrix"), function(x) x)
setMethod("skewpart", signature(x = "symmetricMatrix"), setZero)


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
