#### All  %*%, crossprod() and tcrossprod() methods of the Matrix package
#### ^^^  ----------------------------------------------------------
###  Exceptions: ./diagMatrix.R
###		 ./indMatrix.R ./pMatrix.R

## For %*% (M = Matrix; v = vector (double or integer {complex maybe?}):
.M.v <- function(x, y) { dim(y) <- c(length(y), 1L); callGeneric(x, y) }
.v.M <- function(x, y) { dim(x) <- c(1L, length(x)); callGeneric(x, y) }

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

.dsy_m_mm <- function(x, y) .Call(dsyMatrix_matrix_mm, x, y, FALSE)
setMethod("%*%", signature(x = "dsyMatrix", y = "matrix"),  .dsy_m_mm)
setMethod("%*%", signature(x = "dsyMatrix", y = "ddenseMatrix"),  .dsy_m_mm)
## for disambiguity :
setMethod("%*%", signature(x = "dsyMatrix", y = "dsyMatrix"),  .dsy_m_mm)
## or even
## for(yCl in .directSubClasses(getClass("ddenseMatrix")))
##     setMethod("%*%", signature(x = "dsyMatrix", y = yCl), .dsy_m_mm)

setMethod("%*%", signature(x = "ddenseMatrix", y = "dsyMatrix"),
          function(x, y) .Call(dsyMatrix_matrix_mm, y, x, TRUE))
setMethod("%*%", signature(x = "matrix", y = "dsyMatrix"),
          function(x, y) .Call(dsyMatrix_matrix_mm, y, x, TRUE))

setMethod("%*%", signature(x = "dspMatrix", y = "ddenseMatrix"),
          function(x, y) .Call(dspMatrix_matrix_mm, x, y),
          valueClass = "dgeMatrix")
setMethod("%*%", signature(x = "dspMatrix", y = "matrix"),
          function(x, y) .Call(dspMatrix_matrix_mm, x, y),
          valueClass = "dgeMatrix")


## Not needed because of c("numeric", "Matrix") method
##setMethod("%*%", signature(x = "numeric", y = "CsparseMatrix"),
##	    function(x, y) t(.Call(Csparse_dense_crossprod, y, t(x))),
##	    valueClass = "dgeMatrix")

## FIXME -- do the "same" for "dtpMatrix" {also, with [t]crossprod()}
## all just like these "%*%" :
setMethod("%*%", signature(x = "dtrMatrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_dtrMatrix_mm, x, y, FALSE, FALSE))

setMethod("%*%", signature(x = "dtrMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "dtrMatrix", y = "matrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "ddenseMatrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, FALSE),
	  valueClass = "dgeMatrix")

setMethod("%*%", signature(x = "matrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, FALSE),
	  valueClass = "dgeMatrix")



setMethod("%*%", signature(x = "dtpMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dtpMatrix_matrix_mm, x, y, FALSE, FALSE))
setMethod("%*%", signature(x = "dgeMatrix", y = "dtpMatrix"),
	  function(x, y) .Call(dgeMatrix_dtpMatrix_mm, x, y))

## dtpMatrix <-> matrix : will be used by the "numeric" one
setMethod("%*%", signature(x = "dtpMatrix", y = "matrix"),
          function(x, y) .Call(dtpMatrix_matrix_mm, x, y, FALSE, FALSE))
setMethod("%*%", signature(x = "matrix", y = "dtpMatrix"),
          function(x, y) as(x, "dgeMatrix") %*% y)

## dtpMatrix <-> numeric : the auxiliary functions are R version specific!
##setMethod("%*%", signature(x = "dtpMatrix", y = "numeric"), .M.v)
##setMethod("%*%", signature(x = "numeric", y = "dtpMatrix"), .v.M)


## For multiplication operations, sparseMatrix overrides other method
## selections.	Coerce a ddensematrix argument to a lsparseMatrix.
setMethod("%*%", signature(x = "lsparseMatrix", y = "ldenseMatrix"),
	  function(x, y) x %*% as(y, "sparseMatrix"))

setMethod("%*%", signature(x = "ldenseMatrix", y = "lsparseMatrix"),
	  function(x, y) as(x, "sparseMatrix") %*% y)

## and coerce lsparse* to lgC*
setMethod("%*%", signature(x = "lsparseMatrix", y = "lsparseMatrix"),
	  function(x, y) as(x, "lgCMatrix") %*% as(y, "lgCMatrix"))


for(c.x in c("lMatrix", "nMatrix")) {
    setMethod("%*%", signature(x = c.x, y = "dMatrix"),
	      function(x, y) as(x, "dMatrix") %*% y)
    setMethod("%*%", signature(x = "dMatrix", y = c.x),
	      function(x, y) x %*% as(y, "dMatrix"))
    for(c.y in c("lMatrix", "nMatrix"))
    setMethod("%*%", signature(x = c.x, y = c.y),
	      function(x, y) as(x, "dMatrix") %*% as(y, "dMatrix"))
}; rm(c.x, c.y)

setMethod("%*%", signature(x = "CsparseMatrix", y = "CsparseMatrix"),
	  function(x, y) .Call(Csparse_Csparse_prod, x, y))

setMethod("%*%", signature(x = "CsparseMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(Csparse_dense_prod, x, y))

setMethod("%*%", signature(x = "CsparseMatrix", y = "matrix"),
	  function(x, y) x %*% Matrix(y))

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
setMethod("%*%", signature(x = "TsparseMatrix", y = "TsparseMatrix"),
	  function(x, y) .T.2.C(x) %*% .T.2.C(y))



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
	  function(x, y) x %*% Matrix(y))
setMethod("%*%", signature(x = "matrix", y = "Matrix"),
	  function(x, y) Matrix(x) %*% y)

## bail-out methods in order to get better error messages
.local.bail.out <- function (x, y)
    stop(gettextf('not-yet-implemented method for <%s> %%*%% <%s>',
		  class(x), class(y)), domain=NA)
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
	  function(x, y = NULL) .Call(dgeMatrix_matrix_crossprod, x, y, FALSE),
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
	  function(x, y = NULL) crossprod(as(x, "dgeMatrix")),
	  valueClass = "dpoMatrix")

## "dtrMatrix" - remaining (uni)triangular if possible
setMethod("crossprod", signature(x = "dtrMatrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_dtrMatrix_mm, x, y, FALSE, TRUE))

setMethod("crossprod", signature(x = "dtrMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE, TRUE),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "dtrMatrix", y = "matrix"),
	  function(x, y) .Call(dtrMatrix_matrix_mm, x, y, FALSE, TRUE),
	  valueClass = "dgeMatrix")
## "dtpMatrix"
if(FALSE) ## not yet in C
setMethod("crossprod", signature(x = "dtpMatrix", y = "dtpMatrix"),
	  function(x, y) .Call(dtpMatrix_dtpMatrix_mm, x, y, FALSE, TRUE))

setMethod("crossprod", signature(x = "dtpMatrix", y = "ddenseMatrix"),
	  function(x, y) .Call(dtpMatrix_matrix_mm, x, y, FALSE, TRUE),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "dtpMatrix", y = "matrix"),
	  function(x, y) .Call(dtpMatrix_matrix_mm, x, y, FALSE, TRUE),
	  valueClass = "dgeMatrix")



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
	      if (is(x, "symmetricMatrix"))
		  ## crossprod() should give "symmetric*":
		  forceSymmetric(x %*% x, uplo = x@uplo)
	      else
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
	  function(x, y = NULL) crossprod(x, Matrix(y)))

setMethod("crossprod", signature(x = "CsparseMatrix", y = "numeric"),
	  function(x, y = NULL) crossprod(x, Matrix(y)))

setMethod("crossprod", signature(x = "TsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix"))
		  ## crossprod() should give "symmetric*":
		  forceSymmetric(x %*% x, uplo = x@uplo)
	      else
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
setMethod("crossprod", signature(x = "TsparseMatrix", y = "TsparseMatrix"),
	  function(x, y = NULL) crossprod(.T.2.C(x), .T.2.C(y)))


setMethod("crossprod", signature(x = "dsparseMatrix", y = "ddenseMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_dense_crossprod, as(x, "CsparseMatrix"), y))

setMethod("crossprod", signature(x = "ddenseMatrix", y = "dgCMatrix"),
	  function(x, y = NULL) t(.Call(Csparse_dense_crossprod, y, x)))
setMethod("crossprod", signature(x = "ddenseMatrix", y = "dsparseMatrix"),
	  function(x, y = NULL)
	  t(.Call(Csparse_dense_crossprod, as(y, "CsparseMatrix"), x)))

setMethod("crossprod", signature(x = "dgCMatrix", y = "dgeMatrix"),
	  function(x, y = NULL) .Call(Csparse_dense_crossprod, x, y))
setMethod("crossprod", signature(x = "dsparseMatrix", y = "dgeMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_dense_crossprod, as(x, "CsparseMatrix"), y))

## NB: there's already
##     ("CsparseMatrix", "missing") and ("TsparseMatrix", "missing") methods

## infinite recursion:
## setMethod("crossprod", signature(x = "dgeMatrix", y = "dsparseMatrix"),
##	  function(x, y = NULL) crossprod(x, as(y, "dgCMatrix")))


setMethod("crossprod", signature(x = "lsparseMatrix", y = "ldenseMatrix"),
	  function(x, y = NULL) crossprod(x, as(y, "sparseMatrix")))

setMethod("crossprod", signature(x = "ldenseMatrix", y = "lsparseMatrix"),
	  function(x, y = NULL) crossprod(as(x, "sparseMatrix"), y))

setMethod("crossprod", signature(x = "lsparseMatrix", y = "lsparseMatrix"),
	  function(x, y = NULL)
	  crossprod(as(x, "lgCMatrix"), as(y, "lgCMatrix")))

setMethod("crossprod", signature(x = "nsparseMatrix", y = "ndenseMatrix"),
	  function(x, y = NULL) crossprod(x, as(y, "sparseMatrix")))

setMethod("crossprod", signature(x = "ndenseMatrix", y = "nsparseMatrix"),
	  function(x, y = NULL) crossprod(as(x, "sparseMatrix"), y))

setMethod("crossprod", signature(x = "nsparseMatrix", y = "nsparseMatrix"),
	  function(x, y = NULL)
	  crossprod(as(x, "ngCMatrix"), as(y, "ngCMatrix")))


## FIXME(3): slightly sub-optimal : t(<dense>)	:
setMethod("crossprod", signature(x = "ddenseMatrix", y = "CsparseMatrix"),
	  function(x, y) t(.Call(Csparse_dense_crossprod, y, x)),
	  valueClass = "dgeMatrix")

setMethod("crossprod", signature(x = "matrix", y = "CsparseMatrix"),
	  function(x, y) crossprod(Matrix(x), y))


## "Matrix"
cross.v.M <- function(x, y) { dim(x) <- c(length(x), 1L); crossprod(x, y) }
setMethod("crossprod", signature(x = "Matrix", y = "numeric"), .M.v)

setMethod("crossprod", signature(x = "numeric", y = "Matrix"), cross.v.M)

setMethod("crossprod", signature(x = "Matrix", y = "matrix"),
	  function(x, y = NULL) crossprod(x, Matrix(y)))
setMethod("crossprod", signature(x = "matrix", y = "Matrix"),
	  function(x, y = NULL) crossprod(Matrix(x), y))

## sparseVector
setMethod("crossprod", signature(x = "Matrix", y = "sparseVector"), .M.v)
setMethod("crossprod", signature(x = "sparseVector", y = "Matrix"), cross.v.M)

## cheap fallbacks
setMethod("crossprod", signature(x = "Matrix", y = "Matrix"),
	  function(x, y = NULL) t(x) %*% y)
setMethod("crossprod", signature(x = "Matrix", y = "missing"),
	  function(x, y = NULL) t(x) %*% x)
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
	  function(x, y = NULL) .Call(dgeMatrix_matrix_crossprod, x, y, TRUE),
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


setMethod("tcrossprod", signature(x = "dtrMatrix", y = "dtrMatrix"),
	  function(x, y) .Call(dtrMatrix_dtrMatrix_mm, y, x, TRUE, TRUE))


## Must  have 1st arg. = "dtrMatrix" in  dtrMatrix_matrix_mm ():
## would need another way, to define  tcrossprod()  --- TODO? ---
##
## setMethod("tcrossprod", signature(x = "dtrMatrix", y = "ddenseMatrix"),
## 	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, TRUE))

## setMethod("tcrossprod", signature(x = "dtrMatrix", y = "matrix"),
## 	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, TRUE))

setMethod("tcrossprod", signature(x = "ddenseMatrix", y = "dtrMatrix"),
 	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, TRUE))

setMethod("tcrossprod", signature(x = "matrix", y = "dtrMatrix"),
 	  function(x, y) .Call(dtrMatrix_matrix_mm, y, x, TRUE, TRUE))

if(FALSE) { ## TODO in C
setMethod("tcrossprod", signature(x = "ddenseMatrix", y = "dtpMatrix"),
 	  function(x, y) .Call(dtpMatrix_matrix_mm, y, x, TRUE, TRUE))

setMethod("tcrossprod", signature(x = "matrix", y = "dtpMatrix"),
 	  function(x, y) .Call(dtpMatrix_matrix_mm, y, x, TRUE, TRUE))
}




setMethod("tcrossprod", signature(x = "CsparseMatrix", y = "CsparseMatrix"),
	  function(x, y = NULL)
	  .Call(Csparse_Csparse_crossprod, x, y, trans = TRUE))

setMethod("tcrossprod", signature(x = "CsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix"))
		  ## tcrossprod() should give "symmetric*":
		  forceSymmetric(x %*% x, uplo = x@uplo)
	      else
		  .Call(Csparse_crossprod, x, trans = TRUE, triplet = FALSE)
	  })

setMethod("tcrossprod", signature(x = "TsparseMatrix", y = "missing"),
	  function(x, y = NULL) {
	      if (is(x, "symmetricMatrix"))
		  ## tcrossprod() should give "symmetric*":
		  forceSymmetric(x %*% x, uplo = x@uplo)
	      else
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
setMethod("tcrossprod", signature(x = "TsparseMatrix", y = "TsparseMatrix"),
	  function(x, y = NULL) tcrossprod(.T.2.C(x), .T.2.C(y)))


## "Matrix"
tcr.M.v <- function(x, y) { dim(y) <- c(1L, length(y)); tcrossprod(x, y) }

setMethod("tcrossprod", signature(x = "Matrix", y = "numeric"), tcr.M.v)
setMethod("tcrossprod", signature(x = "numeric", y = "Matrix"), .v.M)
setMethod("tcrossprod", signature(x = "Matrix", y = "matrix"),
	  function(x, y = NULL) tcrossprod(x, Matrix(y)))
setMethod("tcrossprod", signature(x = "matrix", y = "Matrix"),
	  function(x, y = NULL) tcrossprod(Matrix(x), y))

## sparseVector
setMethod("tcrossprod", signature(x = "Matrix", y = "sparseVector"), tcr.M.v)
setMethod("tcrossprod", signature(x = "sparseVector", y = "Matrix"), .v.M)

## cheap fallbacks
setMethod("tcrossprod", signature(x = "Matrix", y = "Matrix"),
	  function(x, y = NULL) x %*% t(y))
setMethod("tcrossprod", signature(x = "Matrix", y = "missing"),
	  function(x, y = NULL) x %*% t(x))
setMethod("tcrossprod", signature(x = "Matrix", y = "ANY"),
	  function(x, y = NULL) x %*% t(y))
setMethod("tcrossprod", signature(x = "ANY", y = "Matrix"),
	  function(x, y = NULL) x %*% t(y))

## Local variables:
## mode: R
## page-delimiter: "^###---"
## End:
