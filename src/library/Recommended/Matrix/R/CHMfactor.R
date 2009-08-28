
### FIXME:  We really want the separate parts (P,L,D)  of  A = P' L D L' P'
### -----   --> ~/R/MM/Pkg-ex/Matrix/chol-ex.R             ----------------
setAs("CHMfactor", "sparseMatrix",
      function(from) .Call(CHMfactor_to_sparse, from))

setAs("CHMfactor", "Matrix", function(from) as(from, "sparseMatrix"))

setAs("CHMfactor", "pMatrix", function(from) as(from@perm + 1L, "pMatrix"))

setMethod("expand", signature(x = "CHMfactor"),
          function(x, ...)
          list(P = as(x, "pMatrix"), L = as(x, "sparseMatrix")))

## nowhere used:
isLDL <- function(x) {
    stopifnot(is(x, "CHMfactor"))
    x@type[2]
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
	warning("arguments in", deparse(list(...)), "are disregarded")
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
		  warning("arguments in", deparse(list(...)), "are disregarded")
	      sysDef <- eval(formals()$system)
	      .Call(CHMfactor_spsolve, a, as(b, "dgCMatrix"),
		    match(match.arg(system, sysDef), sysDef, nomatch = 0))
	  }, valueClass = "dgCMatrix")

## Catch-all the rest : make sure 'system' is not lost
setMethod("solve", signature(a = "CHMfactor", b = "ANY"),
	  function(a, b, system = c("A", "LDLt", "LD", "DLt", "L", "Lt", "D", "P", "Pt"),
		   ...)
	      solve(a, as(b, "dMatrix"), system, ...))

setMethod("determinant", signature(x = "CHMfactor", logarithm = "missing"),
          function(x, logarithm, ...) determinant(x, TRUE))

setMethod("determinant", signature(x = "CHMfactor", logarithm = "logical"),
          function(x, logarithm, ...)
      {
          ldet <- .Call(CHMfactor_ldetL2, x)
          modulus <- if (logarithm) ldet else exp(ldet)
          attr(modulus, "logarithm") <- logarithm
          val <- list(modulus = modulus, sign = as.integer(1))
          class(val) <- "det"
          val
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

