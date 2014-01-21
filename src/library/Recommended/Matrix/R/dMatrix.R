### Define Methods that can be inherited for all subclasses

##-> "dMatrix" <--> "lMatrix"   ---> ./lMatrix.R

## these two are parallel to "n <-> l" in the above :
setAs("nMatrix", "dMatrix",
      function(from) {
	  cld <- getClassDef(cl <- MatrixClass(class(from)))
	  isSp <- extends(cld, "sparseMatrix")
	  ## faster(not "nicer"): any(substr(cl,3,3) == c("C","T","R"))
	  sNams <- slotNames(cld)
	  r <- copyClass(from, sub("^n", "d", cl),
			 if(isSp) sNams else sNams[sNams != "x"])
	  r@x <- if(isSp) rep.int(1., nnzSparse(from)) else as.double(from@x)
	  r
      })

## NOTE: This is *VERY* parallel to  ("lMatrix" -> "nMatrix") in ./lMatrix.R :
setAs("dMatrix", "nMatrix",
      function(from) {
	  if(any(is.na(from@x)))
	      stop("\"dMatrix\" object with NAs cannot be coerced to \"nMatrix\"")
	  ## i.e. from@x are only TRUE (or FALSE in dense case)
	  cld <- getClassDef(cl <- MatrixClass(class(from)))
	  if(extends(cld, "diagonalMatrix")) { # have no "ndi*" etc class
	      cl <- class(from <- as(from, "sparseMatrix"))
	      isSp <- TRUE
	  } else {
	      isSp <- extends(cld, "sparseMatrix")
	      if(isSp && any(from@x == 0)) {
		  from <- drop0(from) # was drop0(from, cld)
		  if(cl != (c. <- class(from)))
		      cld <- getClassDef(cl <- c.)
	      }
	  }
	  sNams <- slotNames(cld)
	  r <- copyClass(from, sub("^d", "n", cl), sNams[sNams != "x"])
	  if(!isSp) #  'x' slot |--> logical
	      r@x <- as.logical(from@x)
	  r
      })


## Group Methods, see ?Arith (e.g.)
## -----
## >>> More specific methods for sub-classes (sparse), use these as "catch-all":

## the non-Ops ones :
setMethod("Math2",
          ## Assume that  Generic(u, k) |--> u for u in {0,1}
          ## which is true for round(), signif() ==> all structure maintained
          signature(x = "dMatrix"),
	  function(x, digits) {
              x@x <- callGeneric(x@x, digits = digits)
              x
          })

## at installation time:
## "max" "min" "range"  "prod" "sum"   "any" "all" :
summGenerics <- getGroupMembers("Summary")
## w/o "prod" & "sum":
summGener1 <- summGenerics[match(summGenerics, c("prod","sum"), 0) == 0]

## [also needs extra work in ./AllGeneric.R ] :
setMethod("Summary", signature(x = "ddenseMatrix", na.rm = "ANY"),
	  function(x, ..., na.rm) {
	      d <- x@Dim
	      if(any(d == 0)) return(callGeneric(numeric(0), ..., na.rm=na.rm))
	      clx <- getClassDef(class(x))
	      if(extends(clx, "generalMatrix"))
		  callGeneric(x@x, ..., na.rm = na.rm)
	      else if(extends(clx, "symmetricMatrix")) { # incl packed, pos.def.
		  if(.Generic %in% summGener1) {
		      callGeneric(if (length(x@x) < prod(d)) x@x
				  else x@x[indTri(d[1], upper= x@uplo == "U",
						  diag= TRUE)],
				  ..., na.rm = na.rm)
		  } else callGeneric(as(x, "dgeMatrix")@x, ..., na.rm = na.rm)
	      }
	      else { ## triangular , packed
		  if(.Generic %in% summGener1) {
                      if(.Generic %in% c("any","all")) {
                          Zero <- FALSE; One <- TRUE
                      } else {
                          Zero <- 0; One <- 1
                      }
		      callGeneric(x@x, Zero, if(x@diag == "U") One, ..., na.rm = na.rm)
		  } else callGeneric(as(x, "dgeMatrix")@x, ..., na.rm = na.rm)
	      }
	  })

setMethod("Summary", signature(x = "dsparseMatrix", na.rm = "ANY"),
	  function(x, ..., na.rm)
      {
	  ne <- prod(d <- dim(x))
	  if(ne == 0) return(callGeneric(numeric(0), ..., na.rm=na.rm))
	  n <- d[1]
	  clx <- getClassDef(class(x))
	  isTri <- extends(clx, "triangularMatrix")
	  if(extends(clx, "TsparseMatrix") && is_duplicatedT(x, di = d))
	      x <- .Call(Tsparse_to_Csparse, x, isTri)# = as(x, "Csparsematrix")
	  l.x <- length(x@x)
	  if(l.x == ne) ## fully non-zero (and "general") - very rare but quick
	      return( callGeneric(x@x, ..., na.rm = na.rm) )
	  ## else  l.x < ne

	  isSym <- !isTri && extends(clx, "symmetricMatrix")
	  isU.tri <- isTri && x@diag == "U"
	  ## "full": has *no* structural zero : very rare, but need to catch :
	  full.x <- ((isSym && l.x == choose(n+1, 2)) ||
		     (n == 1 && (isU.tri || l.x == 1)))
	  isGener1 <- .Generic %in% summGener1
	  if(isGener1) { ## not prod() or sum() -> no need check for symmetric
	      logicF <- .Generic %in% c("any","all")
	      ## we rely on  <generic>(x, NULL, y, ..)	:==  <generic>(x, y, ..):
	      callGeneric(x@x,
			  if(!full.x) { if(logicF) FALSE else 0 },
			  if(isU.tri) { if(logicF) TRUE	 else 1 },
			  ..., na.rm = na.rm)
	  }
	  else { ## prod() or sum() : care for "symmetric" and U2N
	      if(!full.x && .Generic == "prod") {
		  if(any(is.na(x@x))) NaN else 0
	      }
	      else
		  callGeneric((if(isSym) as(x, "generalMatrix") else x)@x,
			      if(!full.x) 0, # one 0 <==> many 0's
			      if(isU.tri) rep.int(1, n),
			      ..., na.rm = na.rm)
	  }
      })


## "Ops" ("Arith", "Compare", "Logic") --> ./Ops.R

## -- end{group generics} -----------------------




## Methods for single-argument transformations

setMethod("zapsmall", signature(x = "dMatrix"),
          function(x, digits = getOption("digits")) {
              x@x <- zapsmall(x@x, digits)
              x
          })

## -- end(single-argument transformations) ------


