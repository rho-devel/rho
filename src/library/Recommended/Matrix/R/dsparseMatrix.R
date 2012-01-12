### d(ouble)sparseMatrix methods :

setMethod("image", "dsparseMatrix",
	  function(x, ...) image(as(x, "dgTMatrix"), ...))

setMethod("chol", signature(x = "dsparseMatrix"),
	   function(x, pivot=FALSE, ...) {
	       px <- as(x, "symmetricMatrix")
	       if (isTRUE(validObject(px, test=TRUE))) chol(px, pivot, ...)
	       else stop("'x' is not positive definite -- chol() undefined.")
	   })

setMethod("determinant", signature(x = "dsparseMatrix", logarithm = "logical"),
          function(x, logarithm = TRUE, ...)
          determinant(as(x,"CsparseMatrix"), logarithm, ...))
##-> now dgC or dsC or dtC .. which *have* their methods

setMethod("lu", signature(x = "dsparseMatrix"), # "FIXME": do in C, so can cache 'x@factors$LU'
	  function(x, ...) lu(as(x, "dgCMatrix"), ...))


## Group Methods, see ?Arith (e.g.): "Ops" --> ./Ops.R
## -----
## others moved to ./Csparse.R (and 'up' to ./sparseMatrix.R):
##  "Math2" is in ./dMatrix.R


