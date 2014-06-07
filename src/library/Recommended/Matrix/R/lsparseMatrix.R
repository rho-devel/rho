#### Superclass Methods for all sparse logical matrices


C2l <- function(from) {
    if(extends(cld <- getClassDef(cl <- class(from)), "lsparseMatrix"))
	return(from)
    ## else
    if(!(is.n <- extends(cld, "nsparseMatrix"))) {
        ## len.x <- length(from@x)
        from <- .Call(Csparse_drop, from, 0)
        ## did.drop <- length(from@x) != len.x
    }
    r <- as(.Call(Csparse_to_nz_pattern, from, extends(cld, "triangularMatrix")),
	    "lsparseMatrix")
    if(!is.n && any(ina <- is.na(from@x))) { ## NAs must remain NA
        ## since we dropped, we "know"  that the 'x' slots match:
        stopifnot(length(from@x) == length(r@x))
        is.na(r@x) <- ina
    }
    r
}

setAs("CsparseMatrix", "lMatrix", C2l)
setAs("CsparseMatrix", "lsparseMatrix", C2l)

setAs("lsparseMatrix", "matrix",
      function(from) as(as(from, "ldenseMatrix"), "matrix"))

setAs("lsparseMatrix", "dsparseMatrix", function(from) as(from, "dMatrix"))


###------- Work via  as(*, lgC) : ------------

setMethod("all", signature(x = "lsparseMatrix"),
	  function(x, ..., na.rm = FALSE) {
	      d <- x@Dim
	      l.x <- length(x@x)
	      if(l.x == prod(d)) ## fully non-zero
		  all(x@x, ..., na.rm = na.rm)
	      else if(is(x, "symmetricMatrix") && l.x == choose(d[1]+1, 2)) {
		  if(.Generic %in% summGener1)
		      all(x@x, ..., na.rm = na.rm)
		  else all(as(x, "generalMatrix")@x, ..., na.rm = na.rm)
	      }
	      else FALSE ## has at least one structural 0
	  })

## setMethod("any", ) ---> ./lMatrix.R

setMethod("image", "lsparseMatrix", function(x, ...) image(as(x,"dMatrix")))
