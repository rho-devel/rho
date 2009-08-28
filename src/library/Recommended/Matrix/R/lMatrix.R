setAs("matrix", "lMatrix",
      function(from) { storage.mode(from) <- "logical" ; Matrix(from) })

## NOTE: This is *VERY* parallel to  ("dMatrix" -> "nMatrix") in ./dMatrix.R :
setAs("lMatrix", "nMatrix",
      function(from) {
	  if(any(is.na(from@x)))
	      stop("\"lMatrix\" object with NAs cannot be coerced to \"nMatrix\"")
	  ## i.e. from@x are only TRUE or FALSE
	  cld <- getClassDef(cl <- MatrixClass(class(from)))
	  if(extends(cld, "diagonalMatrix")) { # have no "ndi*" etc class
	      cl <- class(from <- as(from, "sparseMatrix"))
	      isSp <- TRUE
	  } else {
	      isSp <- extends(cld, "sparseMatrix")
	      if(isSp && !all(from@x)) {
		  from <- drop0(from) # was drop0(from, cld)
		  if(cl != (c. <- class(from)))
		      cld <- getClassDef(cl <- c.)
	      }
	  }
	  sNams <- slotNames(cld)
	  copyClass(from, sub("^l", "n", cl),
		    if(isSp) sNams[sNams != "x"] else sNams)
      })

## and the reverse as well :

setAs("nMatrix", "lMatrix",
      function(from) {
	  cld <- getClassDef(cl <- MatrixClass(class(from)))
	  r <- copyClass(from, sub("^n", "l", cl), slotNames(cld))
	  if(extends(cld, "sparseMatrix"))
	      r@x <- rep.int(TRUE, length(if(!extends(cld, "RsparseMatrix"))
					  from@i else from@j))
	  r
      })

setAs("dMatrix", "lMatrix",
      function(from) {
	  cld <- getClassDef(newCl <- class2(cl <- class(from), "l"))
	  sNams <- slotNames(cld)
	  r <- copyClass(from, newCl, sNames = sNams[sNams != "x"])
	  r@x <- as.logical(from@x)
	  r
      })

setAs("lMatrix", "dMatrix",
      function(from) {
	  cld <- getClassDef(cl <- MatrixClass(class(from)))
	  sNams <- slotNames(cld)
	  r <- copyClass(from, newCl = sub("^l", "d", cl),
			 sNames = sNams[sNams != "x"])
	  r@x <- as.double(from@x)
	  r
      })

## needed at least for lsparse* :
setAs("lMatrix", "dgCMatrix",
      function(from) as(as(from, "lgCMatrix"), "dgCMatrix"))

## all() methods ---> ldenseMatrix.R and lsparseMatrix.R

setMethod("any", signature(x = "lMatrix"),
	  function(x, ..., na.rm = FALSE)
	  ## logical unit-triangular has TRUE diagonal:
	  (prod(dim(x)) >= 1 && is(x, "triangularMatrix") && x@diag == "U") ||
	  any(x@x, ..., na.rm = na.rm))
