### Define Methods that can be inherited for all subclasses

## This replaces many "d..Matrix" -> "dgeMatrix" ones
## >> but << needs all sub(sub(sub)) classes of "ddenseMatrix" listed
##   -----  in  ../src/Mutils.c

setAs("ddenseMatrix", "dgeMatrix",
      function(from) .Call(dup_mMatrix_as_dgeMatrix, from))

setAs("ddenseMatrix", "matrix",
      function(from) as(as(from, "dgeMatrix"), "matrix"))

## d(ouble) to l(ogical):
setAs("dgeMatrix", "lgeMatrix", function(from) d2l_Matrix(from, "dgeMatrix"))
setAs("dsyMatrix", "lsyMatrix", function(from) d2l_Matrix(from, "dsyMatrix"))
setAs("dspMatrix", "lspMatrix", function(from) d2l_Matrix(from, "dspMatrix"))
setAs("dtrMatrix", "ltrMatrix", function(from) d2l_Matrix(from, "dtrMatrix"))
setAs("dtpMatrix", "ltpMatrix", function(from) d2l_Matrix(from, "dtpMatrix"))

setAs("ddenseMatrix", "CsparseMatrix",
      function(from) {
	  if (class(from) != "dgeMatrix") # don't lose symmetry/triangularity/...
	      as_Csparse(from)
	  else .Call(dense_to_Csparse, from)
      })

## special case
setAs("dgeMatrix", "dgCMatrix",
      function(from) .Call(dense_to_Csparse, from))

setAs("matrix", "CsparseMatrix",
      function(from) .Call(dense_to_Csparse, from))
##       function(from) {
## 	    if(is.numeric(from))
## 		.Call(dense_to_Csparse, .Call(dup_mMatrix_as_dgeMatrix, from))
## 	    else if(is.logical(from)) ## FIXME: this works, but maybe wastefully
##                 as(Matrix(from, sparse=TRUE), "CsparseMatrix")
## 	    else stop('not-yet-implemented coercion to "CsparseMatrix"')
##       })


## special case needed in the Matrix function
setAs("matrix", "dgCMatrix",
      function(from) {
          storage.mode(from) <- "double"
          .Call(dense_to_Csparse, from)
      })

setAs("numeric", "CsparseMatrix",
      function(from)
      .Call(dense_to_Csparse, .Call(dup_mMatrix_as_dgeMatrix, from)))

setMethod("as.numeric", signature(x = "ddenseMatrix"),
	  function(x, ...) as(x, "dgeMatrix")@x)

## -- see also ./Matrix.R  e.g., for a show() method

## These methods are the 'fallback' methods for all dense numeric
## matrices in that they simply coerce the ddenseMatrix to a
## dgeMatrix. Methods for special forms override these.

setMethod("norm", signature(x = "ddenseMatrix", type = "missing"),
	  function(x, type, ...) norm(as(x, "dgeMatrix")))

setMethod("norm", signature(x = "ddenseMatrix", type = "character"),
	  function(x, type, ...) norm(as(x, "dgeMatrix"), type))

setMethod("rcond", signature(x = "ddenseMatrix", norm = "missing"),
	  function(x, norm, ...) rcond(as(x, "dgeMatrix"), ...))

setMethod("rcond", signature(x = "ddenseMatrix", norm = "character"),
	  function(x, norm, ...) rcond(as(x, "dgeMatrix"), norm, ...))

## Not really useful; now require *identical* class for result:
## setMethod("t", signature(x = "ddenseMatrix"),
## 	  function(x) callGeneric(as(x, "dgeMatrix")))

setMethod("diag", signature(x = "ddenseMatrix"),
          function(x, nrow, ncol) diag(as(x, "dgeMatrix")))

setMethod("solve", signature(a = "ddenseMatrix", b = "missing"),
          function(a, b, ...) solve(as(a, "dgeMatrix")))

setMethod("solve", signature(a = "ddenseMatrix", b = "ANY"),
          function(a, b, ...) solve(as(a, "dgeMatrix"), b))

setMethod("lu", signature(x = "ddenseMatrix"),
          function(x, ...) lu(as(x, "dgeMatrix")))

setMethod("chol", signature(x = "ddenseMatrix"), cholMat)

setMethod("determinant", signature(x = "ddenseMatrix", logarithm = "missing"),
	  function(x, logarithm, ...) determinant(as(x, "dgeMatrix")))

setMethod("determinant", signature(x = "ddenseMatrix", logarithm = "logical"),
	  function(x, logarithm, ...)
	  determinant(as(x, "dgeMatrix"), logarithm))

## now done for "dMatrix":
## setMethod("expm", signature(x = "ddenseMatrix"),
##           function(x) callGeneric(as(x, "dgeMatrix")))

setMethod("Math",
          signature(x = "ddenseMatrix"),
          function(x) callGeneric(as(x, "dgeMatrix")))



.trilDense <- function(x, k = 0, ...) {
    k <- as.integer(k[1])
    d <- dim(x)
    stopifnot(-d[1] <= k, k <= d[1]) # had k <= 0
    ## returns "lower triangular" if k <= 0 && sqr
    .Call(dense_band, x, -d[1], k)
}
## NB: have extra tril(), triu() methods for symmetric ["dsy" and "dsp"] and
##     for triangular ["dtr" and "dtp"]
setMethod("tril", "denseMatrix", .trilDense)
setMethod("tril",      "matrix", .trilDense)

.triuDense <- function(x, k = 0, ...) {
    k <- as.integer(k[1])
    d <- dim(x)
    stopifnot(-d[1] <= k, k <= d[1]) # had k >= 0
    ## returns "upper triangular" if k >= 0
    .Call(dense_band, x, k, d[2])
}
setMethod("triu", "denseMatrix", .triuDense)
setMethod("triu",      "matrix", .triuDense)

.bandDense <- function(x, k1, k2, ...) {
    k1 <- as.integer(k1[1])
    k2 <- as.integer(k2[1])
    dd <- dim(x)
    sqr <- dd[1] == dd[2]
    stopifnot(-dd[1] <= k1, k1 <= k2, k2 <= dd[2])
    r <- .Call(dense_band, x, k1, k2)
    if (sqr &&  k1 < 0 &&  k1 == -k2  && isSymmetric(x)) ## symmetric
	forceSymmetric(r)
    else
	r
}
setMethod("band", "denseMatrix", .bandDense)
setMethod("band",      "matrix", .bandDense)


setMethod("symmpart", signature(x = "ddenseMatrix"),
	  function(x) .Call(ddense_symmpart, x))
setMethod("skewpart", signature(x = "ddenseMatrix"),
	  function(x) .Call(ddense_skewpart, x))

