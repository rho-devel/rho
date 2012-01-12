#### Sparse Matrices in Compressed column-oriented format

### contains = "dsparseMatrix", "CsparseMatrix"

## Specific conversions, should they be necessary.  Better to convert as
## as(x, "TsparseMatrix") or as(x, "denseMatrix")

## Moved to ./Csparse.R :
## setAs("dgCMatrix", "dgTMatrix", ....
## setAs("dgCMatrix", "dgeMatrix", ....
## setAs("dgeMatrix", "dgCMatrix", ....

setAs("dgCMatrix", "ngCMatrix",
      function(from) .Call(Csparse_to_nz_pattern, from, FALSE))

## rather use Csparse* to lsparse* in ./lsparseMatrix.R ,
## but this is for "back-compatibility" (have had tests for it..):
setAs("dgCMatrix", "lgCMatrix",
      function(from) { ## FIXME use .Call() too!
	  r <- new("lgCMatrix")
	  r@x <- as.logical(from@x)
	  ## and copy the other slots
	  for(nm in c("i", "p", "Dim", "Dimnames"))
	      slot(r, nm) <- slot(from, nm)
	  r
      })

setMethod("image", "dgCMatrix",
	  function(x, ...) {
	      x <- as(x, "dgTMatrix")
	      callGeneric()
	  })

## Group Methods, see ?Arith (e.g.)
## -----
##
## "Arith" is now in ./Ops.R
##
## "Math" is up in ./Csparse.R
##
## "Math2" is up in ./dMatrix.R


###---- end {Group Methods} -----------------


## "[<-" methods { setReplaceMethod()s }  are now in ./Csparse.R

## setMethod("writeHB", signature(obj = "dgCMatrix"),
## 	  function(obj, file, ...) {
## 	      .Deprecated("writeMM")
## 	      .Call(Matrix_writeHarwellBoeing, obj,
## 		    as.character(file), "DGC")
## 	  })

##-> ./colSums.R  for colSums,... rowMeans

setMethod("determinant", signature(x = "dgCMatrix", logarithm = "logical"),
          detSparseLU) # using mkDet() --> ./Auxiliaries.R

setMethod("qr", signature(x = "dgCMatrix"),
	  function(x, tol = 1e-07, LAPACK = FALSE)
	  .Call(dgCMatrix_QR, x, TRUE))

setMethod("qr", signature(x = "sparseMatrix"),
	  function(x, ...)
	  qr(as(as(as(x, "CsparseMatrix"), "dsparseMatrix"), "dgCMatrix"), ...))

setMethod("lu", signature(x = "dgCMatrix"),
          ## by default do give an error on singularity
	  function(x, errSing = TRUE, ...) {
	      .Call(dgCMatrix_LU, x,
		    TRUE, ## <- orderp
		    1,	  ## <- tol
		    errSing)
	      })
setMethod("lu", signature(x = "sparseMatrix"),
	  function(x, ...) # "FIXME": do in C, so can cache 'x@factors$LU'
	  lu(as(as(as(x, "CsparseMatrix"), "dsparseMatrix"), "dgCMatrix"), ...))


setMethod("solve", signature(a = "dgCMatrix", b = "matrix"),
	  function(a, b, ...) .Call(dgCMatrix_matrix_solve, a, b),
	  valueClass = "dgeMatrix")

setMethod("solve", signature(a = "dgCMatrix", b = "ddenseMatrix"),
	  function(a, b, ...) .Call(dgCMatrix_matrix_solve, a, b),
	  valueClass = "dgeMatrix")
setMethod("solve", signature(a = "dgCMatrix", b = "dsparseMatrix"),
	  function(a, b, ...)
	  .Call(dgCMatrix_matrix_solve, a, as(b, "denseMatrix")),
	  valueClass = "dgeMatrix")

## This is a really dumb method but some people apparently want it
setMethod("solve", signature(a = "dgCMatrix", b = "missing"),
	  function(a, b, ...)
          .Call(dgCMatrix_matrix_solve, a, b = diag(nrow(a))),
	  valueClass = "dgeMatrix")
