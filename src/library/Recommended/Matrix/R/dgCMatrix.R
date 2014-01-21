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

setMethod("t", signature(x = "dgCMatrix"),
	  function(x) .Call(Csparse_transpose, x, FALSE),
	  valueClass = "dgCMatrix")

setMethod("determinant", signature(x = "dgCMatrix", logarithm = "logical"),
          detSparseLU) # using mkDet() --> ./Auxiliaries.R

setMethod("qr", signature(x = "dgCMatrix"),
	  function(x, tol = 1e-07, LAPACK = FALSE)
	  .Call(dgCMatrix_QR, # -> cs_sqr() and cs_qr() >> ../src/dgCMatrix.c
		x, ## order =
                if(!is.null(v <- getOption("Matrix.verbose")) && v >= 1) -1L
                else TRUE))

setMethod("qr", signature(x = "sparseMatrix"),
	  function(x, ...)
	  qr(as(as(as(x, "CsparseMatrix"), "dsparseMatrix"), "dgCMatrix"), ...))

LU.dgC <- function(x, errSing = TRUE, order = TRUE, tol = 1.0, ...)
    .Call(dgCMatrix_LU, x, order, tol, errSing)

setMethod("lu", signature(x = "dgCMatrix"), LU.dgC)

setMethod("lu", signature(x = "sparseMatrix"),
	  function(x, ...)
	  .set.factors(x, "lu",
		       lu(as(as(as(x, "CsparseMatrix"), "dsparseMatrix"), "dgCMatrix"),
			  ...)))


## MM: see solveSparse() in  ~/R/MM/Pkg-ex/Matrix/Doran-A.R
.solve.sparse.dgC <- function(a, b, tol = .Machine$double.eps) {
    lu.a <- lu(a)
    if(tol > 0) {
	rU <- range(abs(diag(lu.a@U)))
	if(rU[1] / rU[2] < tol)
	    stop(gettextf("LU computationally singular: ratio of extreme entries in |diag(U)| = %9.4g",
			  rU[1] / rU[2]),
		 domain=NA)
    }
    ## per default:  b = Identity = Diagonal(nrow(a)), however more efficiently
    b.miss <- missing(b)
    b.isMat <- b.miss || !is.null(dim(b))
    if(b.miss) b <- .sparseDiagonal(dim(a)[1L])
    ## bp := P %*% b
    bp <- if(b.isMat) b[lu.a@p+1L, ] else b[lu.a@p+1L]
    ## R:= U^{-1} L^{-1} P b
    R <- solve(lu.a@U, solve(lu.a@L, bp))
    ## result = Q'R = Q' U^{-1} L^{-1} P  b  = A^{-1} b,  as  A = P'LUQ
    R[invPerm(lu.a@q, zero.p=TRUE), ]
}

## FIXME: workaround, till  .Call(dgCMatrix_matrix_solve, a, b, sparse=TRUE)  works:
.solve.dgC <- function(a, b, sparse, tol = .Machine$double.eps)
    if(sparse) .solve.sparse.dgC(a, b, tol=tol) else .Call(dgCMatrix_matrix_solve, a, b, FALSE)

.solve.dgC.mat <- function(a, b, sparse=FALSE, tol = .Machine$double.eps, ...) {
    chk.s(...)
    if(sparse) .solve.sparse.dgC(a, b, tol=tol) else .Call(dgCMatrix_matrix_solve, a, b, FALSE)
}

setMethod("solve", signature(a = "dgCMatrix", b = "matrix"), .solve.dgC.mat)

setMethod("solve", signature(a = "dgCMatrix", b = "ddenseMatrix"), .solve.dgC.mat)

setMethod("solve", signature(a = "dgCMatrix", b = "dsparseMatrix"),
	  function(a, b, sparse=FALSE, tol = .Machine$double.eps, ...) {
	      ## TODO:	 ^^^^^^^^^^^^ rather TRUE [not back compatible] ??
	      chk.s(...)
	  ##     if(isSymmetric(a)) # TODO: fast cholmod_symmetric() for Cholesky
	  ##         solve(forceCspSymmetric(a, isTri=FALSE), b) #-> sparse result
	  ##     else ## FIXME: be better when sparse=TRUE (?)
		  .solve.dgC(a, as(b, "denseMatrix"), tol=tol, sparse=sparse)
	  })

## This is a really dumb method but some people apparently want it
## (MM: a bit less dumb now with possibility of staying sparse)
setMethod("solve", signature(a = "dgCMatrix", b = "missing"),
	  function(a, b, sparse=FALSE, tol = .Machine$double.eps, ...) {
	      ## TODO:	 ^^^^^^^^^^^^ rather TRUE [not back compatible] ??
	      chk.s(...)
	   ##    if(isSymmetric(a)) # TODO: fast cholmod_symmetric() for Cholesky
	   ##        solve(forceCspSymmetric(a, isTri=FALSE),
	   ##      	b = Diagonal(nrow(a))) #-> sparse result
	   ##    else ## FIXME: be better when sparse=TRUE (?)
		  ## .solve.dgC(a, b=diag(nrow(a)), sparse)
		  if(sparse) .solve.sparse.dgC(a, tol=tol) # -> "smart" diagonal b
	      else .Call(dgCMatrix_matrix_solve, a, b=diag(nrow(a)), FALSE)
	  })
