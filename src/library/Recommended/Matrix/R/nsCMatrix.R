#### Logical Symmetric Sparse Matrices in Compressed column-oriented format

### contains = "nsparseMatrix"

setAs("nsCMatrix", "matrix",
      function(from) as(as(from, "ngCMatrix"), "matrix"))

setAs("nsCMatrix", "ngCMatrix",
      function(from) .Call(Csparse_symmetric_to_general, from))

## Specific conversions, should they be necessary.  Better to convert as
## as(x, "TsparseMatrix") or as(x, "denseMatrix")
setAs("nsCMatrix", "nsTMatrix",
      function(from) .Call(Csparse_to_Tsparse, from, FALSE))

## --- these now happen using  "nCsparseMatrix" -- in ./ngCMatrix.R
##
## .nsC2d <- function(from)
##     new("dsCMatrix", i = from@i, p = from@p,
## 	x = rep.int(1, length(from@i)), uplo = from@uplo,
## 	Dim = from@Dim, Dimnames = from@Dimnames)

## .nsC2l <- function(from)
##     new("lsCMatrix", i = from@i, p = from@p,
## 	x = rep.int(TRUE, length(from@i)), uplo = from@uplo,
## 	Dim = from@Dim, Dimnames = from@Dimnames)

## Not needed, once we use "nCsparseMatrix" (-> ./ngCMatrix.R ):
setAs("nsCMatrix", "dMatrix", nC2d)
setAs("nsCMatrix", "dsparseMatrix", nC2d)
setAs("nsCMatrix", "dsCMatrix", nC2d)
##
setAs("nsCMatrix", "lMatrix", nC2l)
setAs("nsCMatrix", "lsparseMatrix", nC2l)
setAs("nsCMatrix", "lsCMatrix", nC2l)

## rm(.nsC2d,.nsC2l) # don't even keep "hidden"

## have rather tril() and triu() methods than
## setAs("nsCMatrix", "ntCMatrix", ....)
setMethod("tril", "nsCMatrix",
	  function(x, k = 0, ...) {
	      if(x@uplo == "L" && k == 0)
		  ## same internal structure + diag
		  new("ntCMatrix", uplo = x@uplo, i = x@i, p = x@p,
		      Dim = x@Dim, Dimnames = x@Dimnames)
	      else tril(as(x, "ngCMatrix"), k = k, ...)
	  })
setMethod("triu", "nsCMatrix",
	  function(x, k = 0, ...) {
	      if(x@uplo == "U" && k == 0)
		  new("ntCMatrix", uplo = x@uplo, i = x@i, p = x@p,
		      Dim = x@Dim, Dimnames = x@Dimnames)
	      else triu(as(x, "ngCMatrix"), k = k, ...)
	  })

setMethod("chol", signature(x = "nsCMatrix"),
	  function(x, pivot=FALSE, ...) stop("temporarily disabled"))## FIXME

## Use more general method from CsparseMatrix class
## setMethod("t", signature(x = "nsCMatrix"),
##           function(x)
##           .Call(nsCMatrix_trans, x),
##           valueClass = "nsCMatrix")
