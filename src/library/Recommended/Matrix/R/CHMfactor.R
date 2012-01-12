
### FIXME:  We really want the separate parts (P,L,D)  of  A = P' L D L' P'
### -----   --> ~/R/MM/Pkg-ex/Matrix/chol-ex.R             ----------------
setAs("CHMfactor", "sparseMatrix",
      function(from) .Call(CHMfactor_to_sparse, from))

setAs("CHMfactor", "Matrix", function(from) as(from, "sparseMatrix"))

setAs("CHMfactor", "pMatrix", function(from) as(from@perm + 1L, "pMatrix"))

setMethod("expand", signature(x = "CHMfactor"),
          function(x, ...)
          list(P = as(x, "pMatrix"), L = as(x, "sparseMatrix")))

##' Determine if a CHMfactor object is LDL or LL
##' @param x - a CHMfactor object
##' @return TRUE if x is LDL, otherwise FALSE
isLDL <- function(x)
{
    stopifnot(is(x, "CHMfactor"))
    as.logical(! x@type[2])# "!" = not as type[2] := (cholmod_factor)->is_ll
}

setMethod("image", "CHMfactor",
          function(x, ...) {
              x <- as(as(x, "sparseMatrix"), "dgTMatrix")
              callGeneric()
          })

.CHM_solve <-
    function(a, b,
	     system = c("A", "LDLt", "LD", "DLt", "L", "Lt", "D", "P", "Pt"),
	     ...)
{
    if(length(list(...)))
	warning("arguments in ",deparse(list(...))," are disregarded")
    sysDef <- eval(formals()$system)
    .Call(CHMfactor_solve, a, b,
	  match(match.arg(system, sysDef), sysDef, nomatch = 0))
}

setMethod("solve", signature(a = "CHMfactor", b = "ddenseMatrix"),
	  .CHM_solve, valueClass = "dgeMatrix")

setMethod("solve", signature(a = "CHMfactor", b = "matrix"),
	  .CHM_solve, valueClass = "dgeMatrix")

setMethod("solve", signature(a = "CHMfactor", b = "numeric"),
	  function(a, b, ...)
	  .CHM_solve(a, matrix(if(is.double(b)) b else as.double(b),
			       length(b), 1L), ...),
	  valueClass = "dgeMatrix")

setMethod("solve", signature(a = "CHMfactor", b = "dsparseMatrix"),
	  function(a, b,
		   system = c("A", "LDLt", "LD", "DLt", "L", "Lt", "D", "P", "Pt"),
		   ...) {
	      if(length(list(...)))
		  warning("arguments in ",deparse(list(...))," are disregarded")
	      sysDef <- eval(formals()$system)
	      .Call(CHMfactor_spsolve, a, as(as(b, "CsparseMatrix"), "dgCMatrix"),
		    match(match.arg(system, sysDef), sysDef, nomatch = 0))
	  }, valueClass = "CsparseMatrix")# < virtual value ?

setMethod("solve", signature(a = "CHMfactor", b = "diagonalMatrix"),
	  function(a, b, ...) solve(a, as(b, "dsparseMatrix"), ...))

setMethod("solve", signature(a = "CHMfactor", b = "missing"),
	  ## <--> b = Diagonal(.)
	  function(a, b,
		   system = c("A", "LDLt", "LD","DLt", "L","Lt", "D", "P","Pt"),
		   ...) {
	      if(length(list(...)))
		  warning("arguments in ",deparse(list(...))," are disregarded")
	      sysDef <- eval(formals()$system)
	      system <- match.arg(system, sysDef)
	      i.sys <- match(system, sysDef, nomatch = 0L)
	      as(.Call(CHMfactor_spsolve, a,
		       .sparseDiagonal(a@Dim[1], shape="g"), i.sys),
		 switch(system,
			A=, LDLt = "symmetricMatrix",# was "dsCMatrix"
			LD=, DLt=, L=, Lt =,
			D = "dtCMatrix", # < diagonal: still as "Csparse.."
			P=, Pt = "pMatrix"))
	  })

## Catch-all the rest : make sure 'system' is not lost
setMethod("solve", signature(a = "CHMfactor", b = "ANY"),
	  function(a, b, system = c("A", "LDLt", "LD","DLt", "L","Lt", "D", "P","Pt"),
		   ...)
	      solve(a, as(b, "dMatrix"), system, ...))

setMethod("chol2inv", signature(x = "CHMfactor"),
	  function (x, ...) {
	      if(length(list(...)))
		  warning("arguments in ",deparse(list(...))," are disregarded")
	      solve(x, system = "A")
	  })

setMethod("determinant", signature(x = "CHMfactor", logarithm = "missing"),
          function(x, logarithm, ...) determinant(x, TRUE))

setMethod("determinant", signature(x = "CHMfactor", logarithm = "logical"),
          function(x, logarithm, ...)
      {
	  ldet <- .Call(CHMfactor_ldetL2, x) / 2
	  mkDet(logarithm=logarithm, ldet=ldet, sig = 1L)
      })

setMethod("update", signature(object = "CHMfactor"),
          function(object, parent, mult = 0, ...)
      {
          stopifnot(is(parent, "sparseMatrix"))
          .Call(CHMfactor_update, object, parent, mult)
      })

## Currently hidden:
ldetL2up <- function(x, parent, Imult)
{
    ## Purpose: compute  log Det |A + m*I|  for many values of m
    ## ----------------------------------------------------------------------
    ## Arguments: x: CHMfactor to be updated
    ##      parent : CsparseMatrix M; for symmetric M, A = M, otherwise A = MM'
    ##       Imult : a numeric *vector* of 'm's (= I multipliers)
    ## ----------------------------------------------------------------------
    ## Author: Doug Bates, Date: 19 Mar 2008

    stopifnot(is(x, "CHMfactor"),
              is(parent, "CsparseMatrix"),
              nrow(x) == nrow(parent))
    .Call(CHMfactor_ldetL2up, x, parent, as.double(Imult))
}

##' Update a sparse Cholesky factorization in place
##' @param L A sparse Cholesky factor that inherits from CHMfactor
##' @param parent a sparse matrix for updating the factor.  Either a
##'   dsCMatrix, in which case L is updated to the Cholesky
##'   factorization of parent, or a dgCMatrix, in which case L is
##'   updated to the Cholesky factorization of tcrossprod(parent)
##' @param Imult an optional positive scalar to be added to the
##'   diagonal before factorization,

##' @return NULL.  This function always returns NULL.  It is called
##'   for its side-effect of updating L in place.

##' @note This function violates the functional language semantics of
##'   R in that it updates its argument L in place (i.e. without copying).
##'   This is intentional but it means the function should be used
##'   with caution.  If the preceding sentences do not make sense to
##'   you, you should not use this function,.
destructive_Chol_update <- function(L, parent, Imult = 1)
{
    stopifnot(is(L, "CHMfactor"),
              is(parent, "sparseMatrix"))
    .Call(destructive_CHM_update, L, parent, Imult)
}
