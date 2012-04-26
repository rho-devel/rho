#### Positive-definite Symmetric Matrices -- Coercion and Methods

setAs("dpoMatrix", "dppMatrix",
      function(from)
      copyClass(.Call(dsyMatrix_as_dspMatrix, from),
		"dppMatrix",
		sNames = c("x", "Dim", "Dimnames", "uplo", "factors")))

setAs("dpoMatrix", "corMatrix",
      function(from) {
	  sd <- sqrt(diag(from))
	  if(is.null(names(sd)) && !is.null(nms <- from@Dimnames[[1]]))
	      names(sd) <- nms
	  Is <- Diagonal(x = 1/sd)
	  new("corMatrix", as(forceSymmetric(Is %*% from %*% Is),
			      "dpoMatrix"),
	      sd = unname(sd))
      })

setAs("dpoMatrix", "lMatrix",
      function(from) as(as(from, "dsyMatrix"), "lMatrix"))
setAs("dpoMatrix", "nMatrix",
      function(from) as(as(from, "dsyMatrix"), "nMatrix"))

if(FALSE) # should no longer be needed
setAs("corMatrix", "lMatrix",
      function(from) as(as(from, "dpoMatrix"), "lMatrix"))

## Needed *in addition* to the general to_dpo() method below:
setAs("dspMatrix", "dpoMatrix",
      function(from) as(as(from,"dsyMatrix"), "dpoMatrix"))

to_dpo <- function(from) # not coercing to "dsy*" explicitly:
    as(as(as(as(from, "symmetricMatrix"), "dMatrix"),
	  "denseMatrix"), "dpoMatrix")
setAs("Matrix", "dpoMatrix", to_dpo)
setAs("matrix", "dpoMatrix", to_dpo)



setMethod("chol", signature(x = "dpoMatrix"),
	  function(x, pivot, ...) .Call(dpoMatrix_chol, x))

setMethod("rcond", signature(x = "dpoMatrix", norm = "character"),
          function(x, norm, ...) .Call(dpoMatrix_rcond, x, norm))

setMethod("rcond", signature(x = "dpoMatrix", norm = "missing"),
          function(x, norm, ...) .Call(dpoMatrix_rcond, x, "O"))

setMethod("solve", signature(a = "dpoMatrix", b = "missing"),
          function(a, b, ...) .Call(dpoMatrix_solve, a),
          valueClass = "dpoMatrix")

setMethod("solve", signature(a = "dpoMatrix", b = "dgeMatrix"),
          function(a, b, ...) .Call(dpoMatrix_dgeMatrix_solve, a, b),
          valueClass = "dgeMatrix")

setMethod("solve", signature(a = "dpoMatrix", b = "matrix"),
          function(a, b, ...) .Call(dpoMatrix_matrix_solve, a, b),
          valueClass = "matrix")

## Is this usable / necessary?  -- FIXME!
## setMethod("solve", signature(a = "dpoMatrix", b = "numeric"),
##          function(a, b, ...)
##          as.numeric(.Call(dpoMatrix_matrix_solve,
##                           a, as.matrix(b))),
##          valueClass = "numeric")
