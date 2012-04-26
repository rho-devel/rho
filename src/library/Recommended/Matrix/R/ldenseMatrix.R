#### "ldenseMatrix" - virtual class of logical dense matrices
####  ------------
#### Contains  lge*;  ltr*, ltp*;  lsy*, lsp*;	 ldi*

## Logical -> Double {of same structure}:

setAs("lgeMatrix", "dgeMatrix", function(from) l2d_Matrix(from, "lgeMatrix"))
setAs("lsyMatrix", "dsyMatrix", function(from) l2d_Matrix(from, "lsyMatrix"))
setAs("lspMatrix", "dspMatrix", function(from) l2d_Matrix(from, "lspMatrix"))
setAs("ltrMatrix", "dtrMatrix", function(from) l2d_Matrix(from, "ltrMatrix"))
setAs("ltpMatrix", "dtpMatrix", function(from) l2d_Matrix(from, "ltpMatrix"))

### NOTA BENE: Much of this is *very* parallel to ./ndenseMatrix.R
###						  ~~~~~~~~~~~~~~~~

## all need be coercable to "lgeMatrix":

setAs("lsyMatrix", "lgeMatrix",	 function(from)
      .Call(lsyMatrix_as_lgeMatrix, from, 0:0))
setAs("ltrMatrix", "lgeMatrix",	 function(from)
      .Call(ltrMatrix_as_lgeMatrix, from, 0:0))
setAs("ltpMatrix", "lgeMatrix",
      function(from) as(as(from, "ltrMatrix"), "lgeMatrix"))
setAs("lspMatrix", "lgeMatrix",
      function(from) as(as(from, "lsyMatrix"), "lgeMatrix"))
## and the reverse
setAs("lgeMatrix", "ltpMatrix",
      function(from) as(as(from, "ltrMatrix"), "ltpMatrix"))
setAs("lgeMatrix", "lspMatrix",
      function(from) as(as(from, "lsyMatrix"), "lspMatrix"))


## packed <->  non-packed :

setAs("lspMatrix", "lsyMatrix",
      function(from)
      .Call(lspMatrix_as_lsyMatrix, from, 0:0))

setAs("lsyMatrix", "lspMatrix",
      function(from)
      .Call(lsyMatrix_as_lspMatrix, from, 0:0))

setAs("ltpMatrix", "ltrMatrix",
      function(from)
      .Call(ltpMatrix_as_ltrMatrix, from, 0:0))

setAs("ltrMatrix", "ltpMatrix",
      function(from)
      .Call(ltrMatrix_as_ltpMatrix, from, 0:0))



### -> symmetric :

if(FALSE) ## not sure if this is a good idea ... -- FIXME?
setIs("lgeMatrix", "lsyMatrix",
      test = function(obj) isSymmetric(obj),
      replace = function(obj, value) { ## copy all slots
	  for(n in slotNames(obj)) slot(obj, n) <- slot(value, n)
      })

### Alternative (at least works):
setAs("lgeMatrix", "lsyMatrix",
      function(from) {
	  if(isSymmetric(from))
	      new("lsyMatrix", x = from@x, Dim = from@Dim,
		  Dimnames = from@Dimnames, factors = from@factors)
	  else
	      stop("not a symmetric matrix; consider forceSymmetric() or symmpart()")
      })

setAs("lgeMatrix", "ltrMatrix",
      function(from) {
	  if(isT <- isTriangular(from))
	      new("ltrMatrix", x = from@x, Dim = from@Dim,
		  Dimnames = from@Dimnames, uplo = .if.NULL(attr(isT, "kind"), "U"))
	  ## TODO: also check 'diag'
	  else stop("not a triangular matrix")
      })


###  ldense* <-> "matrix" :

## 1) "lge* :
setAs("lgeMatrix", "matrix",
      function(from) array(from@x, dim = from@Dim, dimnames = from@Dimnames))

setAs("matrix", "lgeMatrix",
      function(from) {
	  new("lgeMatrix",
	      x = as.logical(from),
	      Dim = as.integer(dim(from)),
	      Dimnames = .M.DN(from))
      })

## 2) base others on "lge*":

setAs("matrix", "lsyMatrix",
      function(from) as(as(from, "lgeMatrix"), "lsyMatrix"))
setAs("matrix", "lspMatrix",
      function(from) as(as(from, "lsyMatrix"), "lspMatrix"))
setAs("matrix", "ltrMatrix",
      function(from) as(as(from, "lgeMatrix"), "ltrMatrix"))
setAs("matrix", "ltpMatrix",
      function(from) as(as(from, "ltrMatrix"), "ltpMatrix"))

## Useful if this was called e.g. for as(*, "lsyMatrix"), but it isn't
setAs("matrix", "ldenseMatrix", function(from) as(from, "lgeMatrix"))

setAs("ldenseMatrix", "matrix", ## uses the above l*M. -> lgeM.
      function(from) as(as(from, "lgeMatrix"), "matrix"))

## dense |-> compressed :

setAs("lgeMatrix", "lgTMatrix",
      function(from) as(.dense2C(from), "lgTMatrix"))

setAs("lgeMatrix", "lgCMatrix",
      function(from) as(as(from, "lgTMatrix"), "lgCMatrix"))

setMethod("as.logical", signature(x = "ldenseMatrix"),
	  function(x, ...) as(x, "lgeMatrix")@x)

###----------------------------------------------------------------------

setMethod("diag", signature(x = "ltrMatrix"),
          function(x, nrow, ncol) .Call(ltrMatrix_getDiag, x))

setMethod("diag", signature(x = "ltpMatrix"),
          function(x, nrow, ncol) .Call(ltpMatrix_getDiag, x))


setMethod("diag", signature(x = "ldenseMatrix"),
	  function(x, nrow, ncol) diag(as(x, "lgeMatrix")))
setMethod("diag", signature(x = "ndenseMatrix"),# << the "same"
	  function(x, nrow, ncol) diag(as(x, "ldenseMatrix")))

setMethod("diag", signature(x = "lgeMatrix"),
	  function(x, nrow, ncol) .Call(lgeMatrix_getDiag, x))

setMethod("t", signature(x = "lgeMatrix"), t_geMatrix)
setMethod("t", signature(x = "ltrMatrix"), t_trMatrix)
setMethod("t", signature(x = "lsyMatrix"), t_trMatrix)
setMethod("t", signature(x = "ltpMatrix"),
	  function(x) as(t(as(x, "ltrMatrix")), "ltpMatrix"))
setMethod("t", signature(x = "lspMatrix"),
	  function(x) as(t(as(x, "lsyMatrix")), "lspMatrix"))

## NOTE:  "&" and "|"  are now in group "Logic" c "Ops" --> ./Ops.R
##        "!" is in ./not.R

setMethod("as.vector", signature(x = "ldenseMatrix", mode = "missing"),
	  function(x, mode) as(x, "lgeMatrix")@x)

setMethod("all", signature(x = "lsyMatrix"),
          function(x, ..., na.rm = FALSE)
          all(x@x, ..., na.rm = na.rm))
## Note: the above "lsy*" method is needed [case below can be wrong]
setMethod("all", signature(x = "ldenseMatrix"),
	  function(x, ..., na.rm = FALSE) {
	      if(prod(dim(x)) >= 1)
		  (!is(x, "triangularMatrix") && !is(x, "diagonalMatrix") &&
		   all(x@x, ..., na.rm = na.rm))
	      else all(x@x, ..., na.rm = na.rm)
	  })

## setMethod("any", ) ---> ./lMatrix.R

## setMethod("any", signature(x = "ldenseMatrix"),
## 	  function(x, ..., na.rm = FALSE)
## 	  (prod(dim(x)) >= 1 && is(x, "triangularMatrix") && x@diag == "U") ||
## 	  any(x@x, ..., na.rm = na.rm))

setMethod("norm", signature(x = "ldenseMatrix", type = "character"),
	  function(x, type, ...)
          .Call(dgeMatrix_norm, as(as(x,"dMatrix"),"dgeMatrix"), type),
	  valueClass = "numeric")

.rcond_via_d <- function(x, norm, ...)
    rcond(as(as(x, "dMatrix"), "dgeMatrix"), norm=norm, ...)


setMethod("rcond", signature(x = "ldenseMatrix", norm = "character"),
	  .rcond_via_d, valueClass = "numeric")

