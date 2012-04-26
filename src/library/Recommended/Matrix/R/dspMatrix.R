### Coercion and Methods for Symmetric Packed Matrices

setAs("dspMatrix", "dsyMatrix",
      function(from) .Call(dspMatrix_as_dsyMatrix, from))
dsp2sC <- function(from) as(.Call(dspMatrix_as_dsyMatrix, from), "dsCMatrix")
## setAs("dspMatrix", "dsCMatrix", dsp2sC)
setAs("dspMatrix", "CsparseMatrix", dsp2sC)
setAs("dspMatrix", "sparseMatrix", dsp2sC)

## dge <--> dsp   via  dsy
setAs("dgeMatrix", "dspMatrix",
      function(from) as(as(from, "dsyMatrix"), "dspMatrix"))

## S3-matrix <--> dsp   via  dsy
setAs("dspMatrix", "matrix",
      function(from) as(as(from, "dsyMatrix"), "matrix"))
setAs("matrix", "dspMatrix",
      function(from) as(as(from, "dsyMatrix"), "dspMatrix"))



setMethod("rcond", signature(x = "dspMatrix", norm = "character"),
          function(x, norm, ...)
          .Call(dspMatrix_rcond, x, norm),
          valueClass = "numeric")

setMethod("rcond", signature(x = "dspMatrix", norm = "missing"),
          function(x, norm, ...)
          .Call(dspMatrix_rcond, x, "O"),
          valueClass = "numeric")

setMethod("BunchKaufman", signature(x = "dspMatrix"),
	  function(x) .Call(dspMatrix_trf, x))

## Should define multiplication from the right

setMethod("solve", signature(a = "dspMatrix", b = "missing"),
	  function(a, b, ...) .Call(dspMatrix_solve, a),
	  valueClass = "dspMatrix")

setMethod("solve", signature(a = "dspMatrix", b = "matrix"),
	  function(a, b, ...) .Call(dspMatrix_matrix_solve, a, b),
	  valueClass = "dgeMatrix")

setMethod("solve", signature(a = "dspMatrix", b = "ddenseMatrix"),
	  function(a, b, ...) .Call(dspMatrix_matrix_solve, a, b),
	  valueClass = "dgeMatrix")

##setMethod("solve", signature(a = "dspMatrix", b = "numeric"),
##	  function(a, b, ...)
##	  .Call(dspMatrix_matrix_solve, a, as.matrix(b)),
##	  valueClass = "dgeMatrix")

## No longer needed
## setMethod("solve", signature(a = "dspMatrix", b = "integer"),
## 	  function(a, b, ...) {
## 	      storage.mode(b) <- "double"
## 	      .Call(dspMatrix_matrix_solve, a, as.matrix(b))
## 	  }, valueClass = "dgeMatrix")

setMethod("norm", signature(x = "dspMatrix", type = "character"),
          function(x, type, ...) .Call(dspMatrix_norm, x, type),
          valueClass = "numeric")

setMethod("norm", signature(x = "dspMatrix", type = "missing"),
          function(x, type, ...) .Call(dspMatrix_norm, x, "O"),
          valueClass = "numeric")

setMethod("t", signature(x = "dspMatrix"),
          function(x) as(t(as(x, "dsyMatrix")), "dspMatrix"),
          valueClass = "dspMatrix")

setMethod("unpack", signature(x = "dspMatrix"),
          function(x, ...) as(x, "dsyMatrix"),
          valueClass = "dsyMatrix")

## The following allows  as(*, "dppMatrix").
## However it *requires* that dppMatrix_chol() gives an error
## for non-positive-semi-definite matrices -- which it does since 2005-10-03
if(FALSE)## FIXME: This gives an error for singular pos.SEMI-def. matrices:
setIs("dspMatrix", "dppMatrix",
      test = function(obj)
          "try-error" != class(try(.Call(dppMatrix_chol, obj), TRUE)),
      replace = function(obj, value) {
          ## copy all slots
          for(n in slotNames(obj)) slot(obj, n) <- slot(value, n)
          obj
      })

