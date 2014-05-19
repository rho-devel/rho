#### Superclass Methods for all sparse nonzero-pattern matrices

setAs("CsparseMatrix", "nsparseMatrix",
      function(from) .Call(Csparse_to_nz_pattern, from,
			   is(from, "triangularMatrix")))
setAs("CsparseMatrix", "nMatrix",
      function(from) .Call(Csparse_to_nz_pattern, from,
			   is(from, "triangularMatrix")))

setAs("nsparseMatrix", "dsparseMatrix", function(from) as(from, "dMatrix"))

###------- Work via  as(*, lgC) : ------------

setMethod("is.na", signature(x = "nsparseMatrix"), is.na_nsp)

if(getRversion() > "3.1.0")
setMethod("anyNA", signature(x = "nsparseMatrix"), function(x) FALSE)

setMethod("all", signature(x = "nsparseMatrix"),
	  function(x, ..., na.rm = FALSE) {
	      pd <- prod(d <- dim(x))
	      if(pd == 0) return(TRUE)
	      cld <- getClassDef(class(x))
	      if(extends(cld, "triangularMatrix"))
		  return(FALSE)
	      ## else
	      if(extends(cld, "TsparseMatrix"))
		  cld <- getClassDef(class(x <- as(x, "CsparseMatrix")))
	      ## now have Csparse or Rsparse: length of index slot = no.{TRUE}
	      l.x <- length(if(extends(cld, "CsparseMatrix")) x@i else x@j)

	      (l.x == pd) || ## fully non-zero
	      (extends(cld, "symmetricMatrix") && l.x == choose(d[1]+1, 2))
	      ## else FALSE
	  })

setMethod("any", signature(x = "nsparseMatrix"),
	  function(x, ..., na.rm = FALSE) {
	      if(any(dim(x) == 0)) return(FALSE)
	      cld <- getClassDef(class(x))
	      if(extends(cld, "triangularMatrix") && x@diag == "U")
		  TRUE # unit-diagonal
	      else if(extends(cld, "CsparseMatrix") ||
		      extends(cld, "TsparseMatrix"))
		  length(x@i) > 0
	      else # RsparseMatrix
		  length(x@j) > 0
	  })


setMethod("image", "nsparseMatrix", function(x, ...) image(as(x,"dMatrix")))
