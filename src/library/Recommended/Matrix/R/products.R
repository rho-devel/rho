#### All  %*%, crossprod() and tcrossprod() methods of the Matrix package
#### ^^^  ----------------------------------------------------------
###  Exceptions: ./diagMatrix.R
###		 ./pMatrix.R

###-- I --- %*% ------------------------------------------------------

## General method for dense matrix multiplication in case specific methods
## have not been defined.
setMethod("%*%", signature(x = "ddenseMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dgeMatrix_matrix_mm,
			       .Call(dup_mMatrix_as_dgeMatrix, x), y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "dgeMatrix", y = "dgeMatrix"),
	  function(x, y) .Call(dgeMatrix_matrix_mm, x, y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "dgeMatrix", y = "matrix"),
	  function(x, y) .Call(dgeMatrix_matrix_mm, x, y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "matrix", y = "dgeMatrix"),
	  function(x, y) .Call(dgeMatrix_matrix_mm, y, x, TRUE),
	  valueClass = "dgeMatrix")

## Not needed because of c("numeric", "Matrix") method
##setMethod("%*%", signature(x = "numeric", y = "CsparseMatrix"),
##	    function(x, y) t(.Call(Csparse_dense_crossprod, y, x)),
##	    valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "dtrMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "dtrMatrix", y = "matrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "ddenseMatrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "matrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE),
	  valueClass = "dgeMatrix")

## no longer needed
## setMethod("%*%", signature(x = "dtrMatrix", y = "dtrMatrix"),
##	  function(x, y) callGeneric(x = x, y = as(y, "dgeMatrix")),
##	     valueClass = "dgeMatrix")


## For multiplication operations, sparseMatrix overrides other method
## selections.	Coerce a ddensematrix argument to a lsparseMatrix.
setMethod("%*%", signature(x = "lsparseMatrix", y = "ldenseMatrix"),
	  function(x, y) x %*% as(y, "lsparseMatrix"))

setMethod("%*%", signature(x = "ldenseMatrix", y = "lsparseMatrix"),
	  function(x, y) as(x, "lsparseMatrix") %*% y)

## and coerce lsparse* to lgC*
setMethod("%*%", signature(x = "lsparseMatrix", y = "lsparseMatrix"),
	  function(x, y) as(x, "lgCMatrix") %*% as(y, "lgCMatrix"))


setMethod("%*%", signature(x = "CsparseMatrix", y = "CsparseMatrix"),
	  function(x, y) .Call(Csparse_Csparse_prod, x, y))

setMethod("%*%", signature(x = "CsparseMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(Csparse_dense_prod, x, y))

setMethod("%*%", signature(x = "CsparseMatrix", y = "matrix"),
	  function(x, y) .Call(Csparse_dense_prod, x, y))

## Not needed because of c("Matrix", "numeric") method
##setMethod("%*%", signature(x = "CsparseMatrix", y = "numeric"),
##	    function(x, y) .Call(Csparse_dense_prod, x, y))

## Not yet.  Don't have methods for y = "CsparseMatrix" and general x
#setMethod("%*%", signature(x = "ANY", y = "TsparseMatrix"),
#	   function(x, y) callGeneric(x, as(y, "CsparseMatrix")))

setMethod("%*%", signature(x = "TsparseMatrix", y = "ANY"),
	  function(x, y) .T.2.C(x) %*% y)
setMethod("%*%", signature(x = "ANY", y = "TsparseMatrix"),
	  function(x, y) x %*% .T.2.C(y))
setMethod("%*%", signature(x = "TsparseMatrix", y = "Matrix"),
	  function(x, y) .T.2.C(x) %*% y)
setMethod("%*%", signature(x = "Matrix", y = "TsparseMatrix"),
	  function(x, y) x %*% .T.2.C(y))



##-------- Work via  as(*, lgC) : ------------

## For multiplication operations, sparseMatrix overrides other method
## selections.	Coerce a ddensematrix argument to a nsparseMatrix.
setMethod("%*%", signature(x = "nsparseMatrix", y = "ndenseMatrix"),
	  function(x, y) x %*% as(y, "nsparseMatrix"))

setMethod("%*%", signature(x = "ndenseMatrix", y = "nsparseMatrix"),
	  function(x, y) as(x, "nsparseMatrix") %*% y)
## and coerce nsparse* to lgC*
setMethod("%*%", signature(x = "nsparseMatrix", y = "nsparseMatrix"),
	  function(x, y) as(x, "ngCMatrix") %*% as(y, "ngCMatrix"))


## FIXME(2): These two are sub-optimal : has  2 x  t(<dense>)  :
setMethod("%*%", signature(x = "ddenseMatrix", y = "CsparseMatrix"),
	  function(x, y) t(.Call(Csparse_dense_crossprod, y, t(x))),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "matrix", y = "CsparseMatrix"),
	  function(x, y) t(.Call(Csparse_dense_crossprod, y, t(x))),
	  valueClass = "dgeMatrix")

## "Matrix"
## Methods for operations where one argument is numeric
setMethod("%*%", signature(x = "Matrix", y = "numeric"), .M.v)

setMethod("%*%", signature(x = "numeric", y = "Matrix"), .v.M)

setMethod("%*%", signature(x = "Matrix", y = "matrix"),
	  function(x, y) callGeneric(x, Matrix(y)))
setMethod("%*%", signature(x = "matrix", y = "Matrix"),
	  function(x, y) callGeneric(Matrix(x), y))

## bail-out methods in order to get better error messages
.local.bail.out <- function (x, y)
    stop(gettextf('not-yet-implemented method for <%s> %%*%% <%s>',
		  class(x), class(y)))
setMethod("%*%", signature(x = "ANY", y = "Matrix"), .local.bail.out)
setMethod("%*%", signature(x = "Matrix", y = "ANY"), .local.bail.out)


### sparseVector
setMethod("%*%", signature(x = "Matrix", y = "sparseVector"), .M.v)
setMethod("%*%", signature(x = "sparseVector", y = "Matrix"), .v.M)

###--- II --- crossprod -----------------------------------------------------

setMethod("crossprod", signature(x = "dgeMatrix", y = "missing"),
	  function(x, y = NULL) .Call(dgeMatrix_crossprod, x, FALSE),
	  valueClass = "dpoMatrix")

## crossprod (x,y)
setMethod("crossprod", signature(x = "dgeMatrix", y = "dgeMatrix"),
	  function(x, y = NULL) .Call(dgeMatrix_dgeMatrix_crossprod, x, y, FALSE),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "dgeMatrix", y = "matrix"),
	  function(x, y = NULL) .Call(dgeMatrix_matrix_crossprod, x, y, FALSE),
	  valueClass = "dgeMatrix")
setMethod("crossprod", signature(x = "dgeMatrix", y = "numeric"),
	  function(x, y = NULL)
	  .Call(dgeMatrix_matrix_crossprod, x, as.matrix(as.double(y)), FALSE),
	  valueClass = "dgeMatrix")
setMethod("crossprod", signature(x = "matrix", y = "dgeMatrix"),
	  function(x, y = NULL) crossprod(as(x, "dgeMatrix"), y),
	  valueClass = "dgeMatrix")
setMethod("crossprod", signature(x = "numeric", y = "dgeMatrix"),
	  function(x, y = NULL) crossprod(as.matrix(as.double(x)), y),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "ddenseMatrix", y = "missing"),
	  function(x, y = NULL) crossprod(as(x, "dgeMatrix")))

setMethod("crossprod", signature(x = "dtrMatrix", y = "missing"),
	  function(x, y = NULL) callGeneric(x = as(x, "dgeMatrix")),
	  valueClass = "dpoMatrix")


## "crossprod" methods too ...
## setMethod("crossprod", signature(x = "dgTMatrix", y = "missing"),
##	     function(x, y = NULL)
##	     .Call(csc_crossprod, as(x, "dgCMatrix")))

## setMethod("crossprod", signature(x = "dgTMatrix", y = "matrix"),
##	     function(x, y = NULL)
##	     .Call(csc_matrix_crossprod, as(x, "dgCMatrix"), y))

##setMethod("crossprod", signature(x = "dgTMatrix", y = "numeric"),
##	    function(x, y = NULL)
##	    .Call(csc_matrix_crossprod, as(x, "dgCMatrix"), as.matrix(y)))

## setMethod("tcrossprod", signature(x = "dgTMatrix", y = "missing"),
##	     function(x, y = NULL)
##	     .Call(csc_tcrossprod, as(x, "dgCMatrix")))

setMethod("crossprod", signature(x = "CsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix")) {
		  warning("crossprod(x) calculated as x %*% x for sparse, symmetric x")
		  return(x %*% x)
	      }
	      .Call(Csparse_crossprod, x, trans = FALSE, triplet = FALSE)
	  })

setMethod("crossprod", signature(x = "CsparseMatrix", y = "CsparseMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_Csparse_crossprod, x, y, trans = FALSE))

## FIXME: Generalize the class of y.  This specific method is to replace one
##	  in dgCMatrix.R
setMethod("crossprod", signature(x = "CsparseMatrix", y = "ddenseMatrix"),
	  function(x, y = NULL) .Call(Csparse_dense_crossprod, x, y))

setMethod("crossprod", signature(x = "CsparseMatrix", y = "matrix"),
	  function(x, y = NULL) .Call(Csparse_dense_crossprod, x, y))

setMethod("crossprod", signature(x = "CsparseMatrix", y = "numeric"),
	  function(x, y = NULL) .Call(Csparse_dense_crossprod, x, y))

setMethod("crossprod", signature(x = "TsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix")) {
		  x <- .T.2.C(x)
		  warning("crossprod(x) calculated as x %*% x for sparse, symmetric x")
		  return(x %*% x)
	      }
	      .Call(Csparse_crossprod, x, trans = FALSE, triplet = TRUE)
	  })

setMethod("crossprod", signature(x = "TsparseMatrix", y = "ANY"),
	  function(x, y = NULL) crossprod(.T.2.C(x), y))
setMethod("crossprod", signature(x = "ANY", y = "TsparseMatrix"),
	  function(x, y = NULL) crossprod(x, .T.2.C(y)))
setMethod("crossprod", signature(x = "TsparseMatrix", y = "Matrix"),
	  function(x, y = NULL) crossprod(.T.2.C(x), y))
setMethod("crossprod", signature(x = "Matrix", y = "TsparseMatrix"),
	  function(x, y = NULL) crossprod(x, .T.2.C(y)))


setMethod("crossprod", signature(x = "dsparseMatrix", y = "ddenseMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_dense_crossprod, as(x, "dgCMatrix"), y))

setMethod("crossprod", signature(x = "ddenseMatrix", y = "dsparseMatrix"),
	  function(x, y = NULL)
	  t(.Call(Csparse_dense_crossprod, as(y, "dgCMatrix"), x)))

setMethod("crossprod", signature(x = "dsparseMatrix", y = "dgeMatrix"),
## NB: using   callGeneric(.) here, leads to infinite recursion :
	  function(x, y = NULL) .Call(Csparse_dense_crossprod, as(x, "dgCMatrix"), y))

## NB: there's already
##     ("CsparseMatrix", "missing") and ("TsparseMatrix", "missing") methods

## infinite recursion:
## setMethod("crossprod", signature(x = "dgeMatrix", y = "dsparseMatrix"),
##	  function(x, y = NULL) crossprod(x, as(y, "dgCMatrix")))


setMethod("crossprod", signature(x = "lsparseMatrix", y = "ldenseMatrix"),
	  function(x, y = NULL) crossprod(x, as(y, "lsparseMatrix")))

setMethod("crossprod", signature(x = "ldenseMatrix", y = "lsparseMatrix"),
	  function(x, y = NULL) crossprod(as(x, "lsparseMatrix"), y))

setMethod("crossprod", signature(x = "lsparseMatrix", y = "lsparseMatrix"),
	  function(x, y = NULL)
	  crossprod(as(x, "lgCMatrix"), as(y, "lgCMatrix")))

setMethod("crossprod", signature(x = "nsparseMatrix", y = "ndenseMatrix"),
	  function(x, y = NULL) crossprod(x, as(y, "nsparseMatrix")))

setMethod("crossprod", signature(x = "ndenseMatrix", y = "nsparseMatrix"),
	  function(x, y = NULL) crossprod(as(x, "nsparseMatrix"), y))

setMethod("crossprod", signature(x = "nsparseMatrix", y = "nsparseMatrix"),
	  function(x, y = NULL)
	  crossprod(as(x, "ngCMatrix"), as(y, "ngCMatrix")))


## FIXME(3): slightly sub-optimal : t(<dense>)	:
setMethod("crossprod", signature(x = "ddenseMatrix", y = "CsparseMatrix"),
	  function(x, y) t(.Call(Csparse_dense_crossprod, y, x)),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "matrix", y = "CsparseMatrix"),
	  function(x, y) t(.Call(Csparse_dense_crossprod, y, x)),
	  valueClass = "dgeMatrix")

## "Matrix"
setMethod("crossprod", signature(x = "Matrix", y = "numeric"), .M.v)

setMethod("crossprod", signature(x = "numeric", y = "Matrix"), .v.M)

setMethod("crossprod", signature(x = "Matrix", y = "matrix"),
	  function(x, y = NULL) callGeneric(x, Matrix(y)))
setMethod("crossprod", signature(x = "matrix", y = "Matrix"),
	  function(x, y = NULL) callGeneric(Matrix(x), y))

## sparseVector
setMethod("crossprod", signature(x = "Matrix", y = "sparseVector"), .M.v)
setMethod("crossprod", signature(x = "sparseVector", y = "Matrix"), .v.M)

## cheap fallbacks
setMethod("crossprod", signature(x = "Matrix", y = "Matrix"),
	  function(x, y = NULL) t(x) %*% y)
setMethod("crossprod", signature(x = "Matrix", y = "ANY"),
	  function(x, y = NULL) t(x) %*% y)
setMethod("crossprod", signature(x = "ANY", y = "Matrix"),
	  function(x, y = NULL) t(x) %*% y)

###--- III --- tcrossprod ---------------------------------------------------

setMethod("tcrossprod", signature(x = "dgeMatrix", y = "dgeMatrix"),
	  function(x, y = NULL) .Call(dgeMatrix_dgeMatrix_crossprod, x, y, TRUE),
	  valueClass = "dgeMatrix")

setMethod("tcrossprod", signature(x = "dgeMatrix", y = "matrix"),
	  function(x, y = NULL) .Call(dgeMatrix_matrix_crossprod, x, y, TRUE),
	  valueClass = "dgeMatrix")
setMethod("tcrossprod", signature(x = "dgeMatrix", y = "numeric"),
	  function(x, y = NULL)
	  .Call(dgeMatrix_matrix_crossprod, x, rbind(as.double(y)), TRUE),
	  valueClass = "dgeMatrix")
setMethod("tcrossprod", signature(x = "matrix", y = "dgeMatrix"),
	  function(x, y = NULL) tcrossprod(as(x, "dgeMatrix"), y),
	  valueClass = "dgeMatrix")
setMethod("tcrossprod", signature(x = "numeric", y = "dgeMatrix"),
	  function(x, y = NULL) tcrossprod(rbind(as.double(x)), y),
	  valueClass = "dgeMatrix")


setMethod("tcrossprod", signature(x = "dgeMatrix", y = "missing"),
	  function(x, y = NULL) .Call(dgeMatrix_crossprod, x, TRUE),
	  valueClass = "dpoMatrix")

if(FALSE) { ## this would mask 'base::tcrossprod'
setMethod("tcrossprod", signature(x = "matrix", y = "missing"),
	  function(x, y = NULL)
	  .Call(dgeMatrix_crossprod, as(x, "dgeMatrix"), TRUE),
	  valueClass = "dpoMatrix")

setMethod("tcrossprod", signature(x = "numeric", y = "missing"),
	  function(x, y = NULL) tcrossprod(as.matrix(as.double(x))))
}

setMethod("tcrossprod", signature(x = "ddenseMatrix", y = "missing"),
	  function(x, y = NULL) tcrossprod(as(x, "dgeMatrix")))

setMethod("tcrossprod", signature(x = "CsparseMatrix", y = "CsparseMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_Csparse_crossprod, x, y, trans = TRUE))

setMethod("tcrossprod", signature(x = "CsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix")) {
		  warning("tcrossprod(x) calculated as x %*% x for sparse, symmetric x")
		  return(x %*% x)
	      }
	      .Call(Csparse_crossprod, x, trans = TRUE, triplet = FALSE)
	  })

setMethod("tcrossprod", signature(x = "TsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      .Call(Csparse_crossprod, x, trans = TRUE, triplet = TRUE)
	  })

setMethod("tcrossprod", signature(x = "ANY", y = "TsparseMatrix"),
	  function(x, y = NULL) tcrossprod(x, .T.2.C(y)))
setMethod("tcrossprod", signature(x = "TsparseMatrix", y = "ANY"),
	  function(x, y = NULL) tcrossprod(.T.2.C(x), y))
setMethod("tcrossprod", signature(x = "Matrix", y = "TsparseMatrix"),
	  function(x, y = NULL) tcrossprod(x, .T.2.C(y)))
setMethod("tcrossprod", signature(x = "TsparseMatrix", y = "Matrix"),
	  function(x, y = NULL) tcrossprod(.T.2.C(x), y))


## "Matrix"
setMethod("tcrossprod", signature(x = "Matrix", y = "numeric"), .M.v)
setMethod("tcrossprod", signature(x = "numeric", y = "Matrix"), .v.M)
setMethod("tcrossprod", signature(x = "Matrix", y = "matrix"),
	  function(x, y = NULL) callGeneric(x, Matrix(y)))
setMethod("tcrossprod", signature(x = "matrix", y = "Matrix"),
	  function(x, y = NULL) callGeneric(Matrix(x), y))

## sparseVector
setMethod("tcrossprod", signature(x = "Matrix", y = "sparseVector"), .M.v)
setMethod("tcrossprod", signature(x = "sparseVector", y = "Matrix"), .v.M)

## cheap fallbacks
setMethod("tcrossprod", signature(x = "Matrix", y = "Matrix"),
	  function(x, y = NULL) x %*% t(y))
setMethod("tcrossprod", signature(x = "Matrix", y = "ANY"),
	  function(x, y = NULL) x %*% t(y))
setMethod("tcrossprod", signature(x = "ANY", y = "Matrix"),
	  function(x, y = NULL) x %*% t(y))

## Local variables:
## mode: R
## page-delimiter: "^###---"
## End:
