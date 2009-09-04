#### All methods for "diagonalMatrix" and its subclasses,
####  currently "ddiMatrix", "ldiMatrix"

## Purpose: Constructor of diagonal matrices -- ~= diag() ,
##          but *not* diag() extractor!
Diagonal <- function(n, x = NULL)
{
    ## Allow  Diagonal(4)  and	Diagonal(x=1:5)
    if(missing(n))
	n <- length(x)
    else {
	stopifnot(length(n) == 1, n == as.integer(n), n >= 0)
	n <- as.integer(n)
    }

    if(missing(x)) ## unit diagonal matrix
	new("ddiMatrix", Dim = c(n,n), diag = "U")
    else {
	lx <- length(x)
	stopifnot(lx == 1 || lx == n) # but keep 'x' short for now
	if(is.logical(x))
	    cl <- "ldiMatrix"
	else if(is.numeric(x)) {
	    cl <- "ddiMatrix"
	    x <- as.numeric(x)
	}
	else if(is.complex(x)) {
	    cl <- "zdiMatrix"  # will not yet work
	} else stop("'x' has invalid data type")
	new(cl, Dim = c(n,n), diag = "N",
	    x = if(lx == 1) rep.int(x,n) else x)
    }
}

.sparseDiagonal <- function(n, x = rep.int(1,n), uplo = "U", shape = "t") {
    stopifnot(n == (n. <- as.integer(n)), (n <- n.) >= 0)
    if((lx <- length(x)) == 1) x <- rep.int(x, n)
    else if(lx != n) stop("length(x) must be 1 or n")
    stopifnot(is.character(shape), nchar(shape) == 1,
	      any(shape == c("t","s","g"))) # triangular / symmetric / general
    kind <-
	if(is.double(x)) "d"
	else if(is.logical(x)) "l"
	else { ## for now
	    storage.mode(x) <- "double"
	    "d"
	}
    new(paste(kind, shape, "CMatrix", sep=''),
	Dim = c(n,n), x = x, uplo = uplo,
	i = if(n) 0:(n - 1L) else integer(0), p = 0:n)
}

## Pkg 'spdep' had (relatively slow) versions of this as_dsCMatrix_I()
.symDiagonal <- function(n, x = rep.int(1,n), uplo = "U")
    .sparseDiagonal(n, x, uplo, shape = "s")

## instead of   diagU2N(as(Diagonal(n), "CsparseMatrix")), diag = "N" in any case:
.trDiagonal <- function(n, x = rep.int(1,n), uplo = "U")
    .sparseDiagonal(n, x, uplo, shape = "t")


### This is modified from a post of Bert Gunter to R-help on  1 Sep 2005.
### Bert's code built on a post by Andy Liaw who most probably was influenced
### by earlier posts, notably one by Scott Chasalow on S-news, 16 Jan 2002
### who posted his bdiag() function written in December 1995.
if(FALSE)##--- no longer used:
.bdiag <- function(lst) {
    ### block-diagonal matrix [a dgTMatrix] from list of matrices
    stopifnot(is.list(lst), length(lst) >= 1)
    dims <- sapply(lst, dim, USE.NAMES=FALSE)
    ## make sure we had all matrices:
    if(!(is.matrix(dims) && nrow(dims) == 2))
	stop("some arguments are not matrices")
    csdim <- rbind(rep.int(0L, 2),
                   apply(dims, 1, cumsum))
    r <- new("dgTMatrix")
    r@Dim <- as.integer(csdim[nrow(csdim),])
    add1 <- matrix(1:0, 2,2)
    for(i in seq_along(lst)) {
	indx <- apply(csdim[i:(i+1),] + add1, 2, function(n) n[1]:n[2])
	if(is.null(dim(indx))) ## non-square matrix
	    r[indx[[1]],indx[[2]]] <- lst[[i]]
	else ## square matrix
	    r[indx[,1], indx[,2]] <- lst[[i]]
    }
    r
}
## expand(<mer>) needed something like bdiag() for lower-triangular
## (Tsparse) Matrices; hence Doug Bates provided a much more efficient
##  implementation for those; now extended and generalized:
.bdiag <- function(lst) {
    ## block-diagonal matrix [a dgTMatrix] from list of matrices
    stopifnot(is.list(lst), (nl <- length(lst)) >= 1)

    Tlst <- lapply(lapply(lst, Matrix:::as_Csp2), # includes "diagU2N"
		   as, "TsparseMatrix")
    if(nl == 1) return(Tlst[[1]])
    ## else
    i_off <- c(0L, cumsum(sapply(Tlst, nrow)))
    j_off <- c(0L, cumsum(sapply(Tlst, ncol)))

    clss <- sapply(Tlst, class)
    knds <- substr(clss, 2, 2)
    sym	 <- knds == "s" # symmetric ones
    tri	 <- knds == "t" # triangular ones
    use.n <- any(is.n <- substr(clss,1,1) == "n")
    if(use.n && !(use.n <- all(is.n)))
	Tlst[is.n] <- lapply(Tlst[is.n], as, "lMatrix")
    if(all(sym)) { ## result should be *symmetric*
	uplos <- sapply(Tlst, slot, "uplo") ## either "U" or "L"
	tLU <- table(uplos)# of length 1 or 2 ..
	if(length(tLU) == 1) { ## all "U" or all "L"
	    useU <- uplos[1] == "U"
	} else { ## length(tLU) == 2, counting "L" and "U"
	    useU <- diff(tLU) >= 0
	    if(useU && (hasL <- tLU[1] > 0))
		Tlst[hasL] <- lapply(Tlst[hasL], t)
	    else if(!useU && (hasU <- tLU[2] > 0))
		Tlst[hasU] <- lapply(Tlst[hasU], t)
	}
	if(use.n) { ## return nsparseMatrix :
	    r <- new("nsTMatrix")
	} else {
	    r <- new("dsTMatrix")
	    r@x <- unlist(lapply(Tlst, slot, "x"))
	}
	r@uplo <- if(useU) "U" else "L"
    }
    else if(all(tri) && { ULs <- sapply(Tlst, slot, "uplo")##  "U" or "L"
			  all(ULs[1L] == ULs[-1L]) } ## all upper or all lower
       ){ ## *triangular* result

	if(use.n) { ## return nsparseMatrix :
	    r <- new("ntTMatrix")
	} else {
	    r <- new("dtTMatrix")
	    r@x <- unlist(lapply(Tlst, slot, "x"))
	}
	r@uplo <- ULs[1L]
    }
    else {
	if(any(sym))
	    Tlst[sym] <- lapply(Tlst[sym], as, "generalMatrix")
	if(use.n) { ## return nsparseMatrix :
	    r <- new("ngTMatrix")
	} else {
	    r <- new("dgTMatrix")
	    r@x <- unlist(lapply(Tlst, slot, "x"))
	}
    }
    r@Dim <- c(i_off[nl+1], j_off[nl + 1])
    r@i <- unlist(lapply(1:nl, function(k) Tlst[[k]]@i + i_off[k]))
    r@j <- unlist(lapply(1:nl, function(k) Tlst[[k]]@j + j_off[k]))
    r
}

bdiag <- function(...) {
    if((nA <- nargs()) == 0) return(new("dgCMatrix"))
    if(nA == 1 && !is.list(...))
	return(as(..., "CsparseMatrix"))
    alis <- if(nA == 1 && is.list(..1)) ..1 else list(...)
    if(length(alis) == 1)
	return(as(alis[[1]], "CsparseMatrix"))

    ## else : two or more arguments
    as(.bdiag(alis), "CsparseMatrix")
}


.diag2tT <- function(from, uplo = "U", kind = .M.kind(from)) {
    ## to triangular Tsparse
    i <- if(from@diag == "U") integer(0) else seq_len(from@Dim[1]) - 1L
    new(paste(kind, "tTMatrix", sep=''),
	diag = from@diag, Dim = from@Dim, Dimnames = from@Dimnames,
	uplo = uplo,
	x = from@x, # <- ok for diag = "U" and "N" (!)
	i = i, j = i)
}

.diag2sT <- function(from, uplo = "U", kind = .M.kind(from)) {
    ## to symmetric Tsparse
    n <- from@Dim[1]
    i <- seq_len(n) - 1L
    new(paste(kind, "sTMatrix", sep=''),
	Dim = from@Dim, Dimnames = from@Dimnames,
	i = i, j = i, uplo = uplo,
	x = if(from@diag == "N") from@x else ## "U"-diag
	rep.int(switch(kind,
		       "d" = 1.,
		       "l" =,
		       "n" = TRUE,
		       ## otherwise
		       stop("'", kind,"' kind not yet implemented")), n))
}

## diagonal -> triangular,  upper / lower depending on "partner":
diag2tT.u <- function(d, x, kind = .M.kind(d))
    .diag2tT(d, uplo = if(is(x,"triangularMatrix")) x@uplo else "U", kind)

## diagonal -> sparse {triangular OR symmetric} (upper / lower) depending on "partner":
diag2Tsmart <- function(d, x, kind = .M.kind(d)) {
    clx <- getClassDef(class(x))
    if(extends(clx, "symmetricMatrix"))
	.diag2sT(d, uplo = x@uplo, kind)
    else
	.diag2tT(d, uplo = if(extends(clx,"triangularMatrix")) x@uplo else "U", kind)
}


## In order to evade method dispatch ambiguity warnings,
## and because we can save a .M.kind() call, we use this explicit
## "hack"  instead of signature  x = "diagonalMatrix" :
##
## ddi*:
diag2tT <- function(from) .diag2tT(from, "U", "d")
setAs("ddiMatrix", "triangularMatrix", diag2tT)
##_no_longer_ setAs("ddiMatrix", "sparseMatrix", diag2tT)
## needed too (otherwise <dense> -> Tsparse is taken):
setAs("ddiMatrix", "TsparseMatrix", diag2tT)
setAs("ddiMatrix", "CsparseMatrix",
      function(from) as(.diag2tT(from, "U", "d"), "CsparseMatrix"))
setAs("ddiMatrix", "symmetricMatrix",
      function(from) .diag2sT(from, "U", "d"))
##
## ldi*:
diag2tT <- function(from) .diag2tT(from, "U", "l")
setAs("ldiMatrix", "triangularMatrix", diag2tT)
##_no_longer_ setAs("ldiMatrix", "sparseMatrix", diag2tT)
## needed too (otherwise <dense> -> Tsparse is taken):
setAs("ldiMatrix", "TsparseMatrix", diag2tT)
setAs("ldiMatrix", "CsparseMatrix",
      function(from) as(.diag2tT(from, "U", "l"), "CsparseMatrix"))
setAs("ldiMatrix", "symmetricMatrix",
      function(from) .diag2sT(from, "U", "l"))


setAs("diagonalMatrix", "nMatrix",
      function(from) {
	  n <- from@Dim[1]
	  i <- if(from@diag == "U") integer(0) else which(isN0(from@x)) - 1L
	  new("ntTMatrix", i = i, j = i, diag = from@diag,
	      Dim = from@Dim, Dimnames = from@Dimnames)
      })

setAs("diagonalMatrix", "nsparseMatrix", function(from) as(from, "nMatrix"))

## Cheap fast substitute for diag() which *does* preserve the mode of x :
mkDiag <- function(x, n) {
    y <- matrix(as0(mod=mode(x)), n,n)
    if (n > 0) y[1L + 0:(n - 1L) * (n + 1L)] <- x
    y
}

setAs("diagonalMatrix", "matrix",
      function(from) {
	  ## want "ldiMatrix" -> <logical> "matrix" :
	  mkDiag(if(from@diag == "U") as1(from@x) else from@x,
		 n = from@Dim[1])
      })

setMethod("as.vector", signature(x = "diagonalMatrix", mode="missing"),
	  function(x, mode) {
	      n <- x@Dim[1]
              mod.x <- mode(x@x)
	      r <- vector(mod.x, length = n^2)
	      if(n)
		  r[1 + 0:(n - 1) * (n + 1)] <-
		      if(x@diag == "U") as1(mod=mod.x) else x@x
	      r
	  })

setAs("diagonalMatrix", "generalMatrix", # prefer sparse:
      function(from) as(as(from, "CsparseMatrix"), "generalMatrix"))

setAs("diagonalMatrix", "denseMatrix",
      function(from) as(as(from, "CsparseMatrix"), "denseMatrix"))

.diag.x <- function(m) if(m@diag == "U") rep.int(as1(m@x), m@Dim[1]) else m@x

.diag.2N <- function(m) {
    if(m@diag == "U") m@diag <- "N"
    m
}

setAs("ddiMatrix", "dgeMatrix",
      function(from) .Call(dup_mMatrix_as_dgeMatrix, from))
setAs("ddiMatrix", "ddenseMatrix",
      function(from) as(as(from, "triangularMatrix"),"denseMatrix"))
setAs("ldiMatrix", "ldenseMatrix",
      function(from) as(as(from, "triangularMatrix"),"denseMatrix"))


setAs("matrix", "diagonalMatrix",
      function(from) {
	  d <- dim(from)
	  if(d[1] != (n <- d[2])) stop("non-square matrix")
	  if(any(from[row(from) != col(from)] != 0))
	      stop("matrix with non-zero off-diagonals cannot be coerced to diagonalMatrix")
	  x <- diag(from)
	  if(is.logical(x)) {
	      cl <- "ldiMatrix"
	      uni <- allTrue(x) ## uni := {is it unit-diagonal ?}
	  } else {
	      cl <- "ddiMatrix"
	      uni <- allTrue(x == 1)
	      storage.mode(x) <- "double"
	  } ## TODO: complex
	  new(cl, Dim = c(n,n), diag = if(uni) "U" else "N",
	      x = if(uni) x[FALSE] else x)
      })

## ``generic'' coercion to  diagonalMatrix : build on  isDiagonal() and diag()
setAs("Matrix", "diagonalMatrix",
      function(from) {
	  d <- dim(from)
	  if(d[1] != (n <- d[2])) stop("non-square matrix")
	  if(!isDiagonal(from)) stop("matrix is not diagonal")
	  ## else:
	  x <- diag(from)
	  if(is.logical(x)) {
	      cl <- "ldiMatrix"
	      uni <- allTrue(x)
	  } else {
	      cl <- "ddiMatrix"
	      uni <- allTrue(x == 1)
	      storage.mode(x) <- "double"
	  } ## TODO: complex
	  new(cl, Dim = c(n,n), diag = if(uni) "U" else "N",
	      x = if(uni) x[FALSE] else x)
      })


setMethod("diag", signature(x = "diagonalMatrix"),
          function(x = 1, nrow, ncol) .diag.x(x))

subDiag <- function(x, i, j, ..., drop) {
    x <- as(x, "TsparseMatrix")
    x <- if(missing(i))
	x[, j, drop=drop]
    else if(missing(j))
	if(nargs() == 4) x[i, , drop=drop] else x[i, drop=drop]
    else
	x[i,j, drop=drop]
    if(isS4(x) && isDiagonal(x)) as(x, "diagonalMatrix") else x
}

setMethod("[", signature(x = "diagonalMatrix", i = "index",
			 j = "index", drop = "logical"), subDiag)
setMethod("[", signature(x = "diagonalMatrix", i = "index",
			 j = "missing", drop = "logical"),
	  function(x, i, j, ..., drop) {
	      na <- nargs()
	      Matrix.msg("diag[i,m,l] : nargs()=", na, .M.level = 2)
	      if(na == 4)
		   subDiag(x, i=i, , drop=drop)
	      else subDiag(x, i=i,   drop=drop)
	  })
setMethod("[", signature(x = "diagonalMatrix", i = "missing",
			 j = "index", drop = "logical"),
	  function(x, i, j, ..., drop) subDiag(x, j=j, drop=drop))

## When you assign to a diagonalMatrix, the result should be
## diagonal or sparse ---
## FIXME: this now fails because the "denseMatrix" methods come first in dispatch
## Only(?) current bug:  x[i] <- value  is wrong when  i is *vector*
replDiag <- function(x, i, j, ..., value) {
    x <- as(x, "TsparseMatrix")
    if(missing(i))
	x[, j] <- value
    else if(missing(j)) { ##  x[i , ] <- v  *OR*   x[i] <- v
        na <- nargs()
##         message("diagnosing replDiag() -- nargs()= ", na)
	if(na == 4)
            x[i, ] <- value
	else if(na == 3)
            x[i] <- value
        else stop("Internal bug: nargs()=",na,"; please report")
    } else
	x[i,j] <- value
    if(isDiagonal(x)) as(x, "diagonalMatrix") else x
}

setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index",
				j = "index", value = "replValue"), replDiag)

setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index",
				j = "missing", value = "replValue"),
		 function(x,i,j, ..., value) {
                     ## message("before replDiag() -- nargs()= ", nargs())
                     if(nargs() == 3)
                         replDiag(x, i=i, value=value)
                     else ## nargs() == 4 :
                         replDiag(x, i=i, , value=value)
                 })

setReplaceMethod("[", signature(x = "diagonalMatrix",
                                i = "matrix", # 2-col.matrix
				j = "missing", value = "replValue"),
		 function(x,i,j, ..., value) {
		     if(ncol(i) == 2) {
			 if(all((ii <- i[,1]) == i[,2])) { # replace in diagonal only
			     if(x@diag == "U") {
				 one <- as1(x@x)
				 if(any(value != one | is.na(value))) {
				     x@diag <- "N"
				     x@x <- rep.int(one, x@Dim[1])
				 }
			     }
			     x@x[ii] <- value
			     x
			 } else { ## no longer diagonal, but remain sparse:
			     x <- as(x, "TsparseMatrix")
			     x[i] <- value
			     x
			 }
		     }
		     else { # behave as "base R": use as if vector
			 x <- as(x, "matrix")
			 x[i] <- value
			 Matrix(x)
		     }
		 })

setReplaceMethod("[", signature(x = "diagonalMatrix", i = "missing",
				j = "index", value = "replValue"),
		 function(x,i,j, ..., value) replDiag(x, j=j, value=value))

setReplaceMethod("[", signature(x = "diagonalMatrix", i = "missing", j = "index",
				value = "sparseMatrix"),
		 function (x, i, j, ..., value)
		 callGeneric(x=x, , j=j, value = as(value, "sparseVector")))
setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index", j = "missing",
				value = "sparseMatrix"),
		 function (x, i, j, ..., value)
		 callGeneric(x=x, i=i, , value = as(value, "sparseVector")))
setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index", j = "index",
				value = "sparseMatrix"),
		 function (x, i, j, ..., value)
		 callGeneric(x=x, i=i, j=j, value = as(value, "sparseVector")))

setReplaceMethod("[", signature(x = "diagonalMatrix", i = "missing", j = "index",
				value = "sparseVector"),
		 replDiag)
setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index", j = "missing",
				value = "sparseVector"),
		 replDiag)
setReplaceMethod("[", signature(x = "diagonalMatrix", i = "index", j = "index",
				value = "sparseVector"),
		 replDiag)


setMethod("t", signature(x = "diagonalMatrix"),
          function(x) { x@Dimnames <- x@Dimnames[2:1] ; x })

setMethod("isDiagonal", signature(object = "diagonalMatrix"),
          function(object) TRUE)
setMethod("isTriangular", signature(object = "diagonalMatrix"),
          function(object) TRUE)
setMethod("isSymmetric", signature(object = "diagonalMatrix"),
	  function(object, ...) TRUE)

setMethod("symmpart", signature(x = "diagonalMatrix"), function(x) x)
setMethod("skewpart", signature(x = "diagonalMatrix"), setZero)

setMethod("chol", signature(x = "ddiMatrix"),
	  function(x, pivot, ...) {
	      if(x@diag == "U") return(x)
	      ## else
	      if(any(x@x < 0))
		  stop("chol() is undefined for diagonal matrix with negative entries")
	      x@x <- sqrt(x@x)
	      x
	  })
## chol(L) is L for logical diagonal:
setMethod("chol", signature(x = "ldiMatrix"), function(x, pivot, ...) x)

setMethod("determinant", signature(x = "diagonalMatrix", logarithm = "logical"),
	  function(x, logarithm, ...) mkDet(.diag.x(x), logarithm))

setMethod("norm", signature(x = "diagonalMatrix", type = "character"),
	  function(x, type, ...) {
	      if((n <- x@Dim[1]) == 0) return(0) # as for "sparseMatrix"
	      type <- toupper(substr(type[1], 1, 1))
	      isU <- (x@diag == "U") # unit-diagonal
	      if(type == "F") sqrt(if(isU) n else sum(x@x^2))
	      else { ## norm == "I","1","O","M" :
		  if(isU) 1 else max(abs(x@x))
	      }
	  })



## Basic Matrix Multiplication {many more to add}
##       ---------------------
## Note that "ldi" logical are treated as numeric
diagdiagprod <- function(x, y) {
    n <- dimCheck(x,y)[1]
    if(x@diag != "U") {
	if(y@diag != "U") {
	    nx <- x@x * y@x
	    if(is.numeric(nx) && !is.numeric(x@x))
		x <- as(x, "dMatrix")
	    x@x <- as.numeric(nx)
	}
	return(x)
    } else ## x is unit diagonal
    return(y)
}

setMethod("%*%", signature(x = "diagonalMatrix", y = "diagonalMatrix"),
	  diagdiagprod, valueClass = "ddiMatrix")

formals(diagdiagprod) <- alist(x=, y=x)
setMethod("crossprod", signature(x = "diagonalMatrix", y = "diagonalMatrix"),
	  diagdiagprod, valueClass = "ddiMatrix")
setMethod("tcrossprod", signature(x = "diagonalMatrix", y = "diagonalMatrix"),
	  diagdiagprod, valueClass = "ddiMatrix")
setMethod("crossprod", signature(x = "diagonalMatrix", y = "missing"),
	  diagdiagprod, valueClass = "ddiMatrix")
setMethod("tcrossprod", signature(x = "diagonalMatrix", y = "missing"),
	  diagdiagprod, valueClass = "ddiMatrix")


diagmatprod <- function(x, y) {
    ## x is diagonalMatrix
    dx <- dim(x)
    dy <- dim(y)
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    n <- dx[1]
    as(if(x@diag == "U") y else x@x * y, "Matrix")
}
setMethod("%*%", signature(x = "diagonalMatrix", y = "matrix"),
	  diagmatprod)
## sneaky .. :
formals(diagmatprod) <- alist(x=, y=NULL)
setMethod("crossprod", signature(x = "diagonalMatrix", y = "matrix"),
	  diagmatprod)

diagGeprod <- function(x, y) {
    dx <- dim(x)
    dy <- dim(y)
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    if(x@diag != "U")
        y@x <- x@x * y@x
    y
}
setMethod("%*%", signature(x= "diagonalMatrix", y= "dgeMatrix"), diagGeprod)
setMethod("%*%", signature(x= "diagonalMatrix", y= "lgeMatrix"), diagGeprod)
formals(diagGeprod) <- alist(x=, y=NULL)
setMethod("crossprod", signature(x = "diagonalMatrix", y = "dgeMatrix"),
	  diagGeprod, valueClass = "dgeMatrix")
setMethod("crossprod", signature(x = "diagonalMatrix", y = "lgeMatrix"),
	  diagGeprod)

matdiagprod <- function(x, y) {
    dx <- dim(x)
    dy <- dim(y)
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    Matrix(if(y@diag == "U") x else x * rep(y@x, each = dx[1]))
}
setMethod("%*%", signature(x = "matrix", y = "diagonalMatrix"),
	  matdiagprod)
formals(matdiagprod) <- alist(x=, y=NULL)
setMethod("tcrossprod", signature(x = "matrix", y = "diagonalMatrix"),
	  matdiagprod)

gediagprod <- function(x, y) {
    dx <- dim(x)
    dy <- dim(y)
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    if(y@diag == "N")
	x@x <- x@x * rep(y@x, each = dx[1])
    x
}
setMethod("%*%", signature(x= "dgeMatrix", y= "diagonalMatrix"), gediagprod)
setMethod("%*%", signature(x= "lgeMatrix", y= "diagonalMatrix"), gediagprod)
formals(gediagprod) <- alist(x=, y=NULL)
setMethod("tcrossprod", signature(x = "dgeMatrix", y = "diagonalMatrix"),
	  gediagprod)
setMethod("tcrossprod", signature(x = "lgeMatrix", y = "diagonalMatrix"),
	  gediagprod)

## crossprod {more of these}

## tcrossprod --- all are not yet there: do the dense ones here:

setMethod("%*%", signature(x = "diagonalMatrix", y = "denseMatrix"),
	  function(x, y) if(x@diag == "U") y else x %*% as(y, "generalMatrix"))
setMethod("%*%", signature(x = "denseMatrix", y = "diagonalMatrix"),
	  function(x, y) if(y@diag == "U") x else as(x, "generalMatrix") %*% y)


## FIXME:
## setMethod("tcrossprod", signature(x = "diagonalMatrix", y = "denseMatrix"),
## 	  function(x, y = NULL) {
##           })

Cspdiagprod <- function(x, y) {
    dx <- dim(x <- .Call(Csparse_diagU2N, x))
    dy <- dim(y)
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    ind <- rep.int(seq_len(dx[2]), x@p[-1] - x@p[-dx[2]-1L])
    if(y@diag == "N")
        x@x <- x@x * y@x[ind]
    x
}

diagCspprod <- function(x, y) {
    dx <- dim(x)
    dy <- dim(y <- .Call(Csparse_diagU2N, y))
    if(dx[2] != dy[1]) stop("non-matching dimensions")
    if(x@diag == "N")
        y@x <- y@x * x@x[y@i + 1L]
    y
}

setMethod("crossprod", signature(x = "diagonalMatrix", y = "CsparseMatrix"),
	  function(x, y = NULL) diagCspprod(x, y))

setMethod("crossprod", signature(x = "diagonalMatrix", y = "sparseMatrix"),
	  function(x, y = NULL) diagCspprod(x, as(y, "CsparseMatrix")))

## Prefer calling diagCspprod to Cspdiagprod if going to transpose anyway
##  x'y == (y'x)'
setMethod("crossprod", signature(x = "CsparseMatrix", y = "diagonalMatrix"),
	  function(x, y = NULL) t(diagCspprod(y, x)))

setMethod("crossprod", signature(x = "sparseMatrix", y = "diagonalMatrix"),
	  function(x, y = NULL) t(diagCspprod(y, as(x, "Csparsematrix"))))

setMethod("tcrossprod", signature(x = "diagonalMatrix", y = "CsparseMatrix"),
	  function(x, y = NULL) diagCspprod(x, t(y)))

setMethod("tcrossprod", signature(x = "diagonalMatrix", y = "sparseMatrix"),
	  function(x, y = NULL) diagCspprod(x, t(as(y, "CsparseMatrix"))))

setMethod("tcrossprod", signature(x = "CsparseMatrix", y = "diagonalMatrix"),
	  function(x, y = NULL) Cspdiagprod(x, y))

setMethod("tcrossprod", signature(x = "sparseMatrix", y = "diagonalMatrix"),
	  function(x, y = NULL) Cspdiagprod(as(x, "CsparseMatrix"), y))
          
setMethod("%*%", signature(x = "diagonalMatrix", y = "CsparseMatrix"),
	  function(x, y) diagCspprod(x, y))

setMethod("%*%", signature(x = "diagonalMatrix", y = "sparseMatrix"),
	  function(x, y) diagCspprod(as(x, "CsparseMatrix"), y))
          
setMethod("%*%", signature(x = "sparseMatrix", y = "diagonalMatrix"),
	  function(x, y) Cspdiagprod(as(x, "CsparseMatrix"), y))

setMethod("%*%", signature(x = "CsparseMatrix", y = "diagonalMatrix"),
	  function(x, y) Cspdiagprod(x, y))

## TODO: Write tests in ./tests/ which ensure that many "ops" with diagonal*
##       do indeed work by going through sparse (and *not* ddense)!

setMethod("solve", signature(a = "diagonalMatrix", b = "missing"),
	  function(a, b, ...) {
	      a@x <- 1/ a@x
	      a@Dimnames <- a@Dimnames[2:1]
	      a
	  })

solveDiag <- function(a, b, ...) {
    if((n <- a@Dim[1]) != nrow(b))
        stop("incompatible matrix dimensions")
    ## trivially invert a 'in place' and multiply:
    a@x <- 1/ a@x
    a@Dimnames <- a@Dimnames[2:1]
    a %*% b
}
setMethod("solve", signature(a = "diagonalMatrix", b = "matrix"),
          solveDiag)
setMethod("solve", signature(a = "diagonalMatrix", b = "Matrix"),
          solveDiag)

## Schur()  ---> ./eigen.R



###---------------- <Ops> (<Arith>, <Logic>, <Compare> ) ----------------------

## Use function for several signatures, in order to evade
## ambiguous dispatch for "ddi", since there's also Arith(ddense., ddense.)
diagOdiag <- function(e1,e2) {
    ## result should also be diagonal _ if possible _
    r <- callGeneric(.diag.x(e1), .diag.x(e2)) # error if not "compatible"
    ## Check what happens with non-diagonals, i.e. (0 o 0), (FALSE o 0), ...:
    r00 <- callGeneric(if(is.numeric(e1@x)) 0 else FALSE,
		       if(is.numeric(e2@x)) 0 else FALSE)
    if(is0(r00)) { ##  r00 == 0 or FALSE --- result *is* diagonal
	if(is.numeric(r)) {
	    if(is.numeric(e2@x)) {
		e2@x <- r; return(.diag.2N(e2)) }
	    if(!is.numeric(e1@x))
		## e.g. e1, e2 are logical;
		e1 <- as(e1, "dMatrix")
	}
	else if(is.logical(r))
	    e1 <- as(e1, "lMatrix")
	else stop("intermediate 'r' is of type", typeof(r))
	e1@x <- r
	.diag.2N(e1)
    }
    else { ## result not diagonal, but at least symmetric:
	isNum <- (is.numeric(r) || is.numeric(r00))
	isLog <- (is.logical(r) || is.logical(r00))

	Matrix.msg("exploding	<diag>	o  <diag>  into dense matrix")
	d <- e1@Dim
	n <- d[1]
	stopifnot(length(r) == n)
	xx <- as.vector(matrix(rbind(r, matrix(r00,n,n)), n,n))
	newcl <-
	    paste(if(isNum) "d" else if(isLog) {
		if(!any(is.na(r)) && !any(is.na(r00))) "n" else "l"
	    } else stop("not yet implemented .. please report")
		  ,
		  "syMatrix", sep='')

	new(newcl, Dim = e1@Dim, Dimnames = e1@Dimnames, x = xx)
    }
}

### This would be *the* way, but we get tons of "ambiguous method dispatch"
## we use this hack instead of signature  x = "diagonalMatrix" :
diCls <- names(getClass("diagonalMatrix")@subclasses)
if(FALSE) {
setMethod("Ops", signature(e1 = "diagonalMatrix", e2 = "diagonalMatrix"),
          diagOdiag)
} else { ## These are just for method disambiguation:
    for(c1 in diCls)
	for(c2 in diCls)
	    setMethod("Ops", signature(e1 = c1, e2 = c2), diagOdiag)
}

## FIXME:    diagonal  o  triangular  |-->  triangular
## -----     diagonal  o  symmetric   |-->  symmetric
##    {also when other is sparse: do these "here" --
##     before conversion to sparse, since that loses "diagonality"}

## For almost everything else, diag* shall be treated "as sparse" :
## These are cheap implementations via coercion

## For disambiguation --- define this for "sparseMatrix" , then for "ANY";
## and because we can save an .M.kind() call, we use this explicit
## "hack" for all diagonalMatrix *subclasses* instead of just "diagonalMatrix" :
##
## ddi*:
setMethod("Ops", signature(e1 = "ddiMatrix", e2 = "sparseMatrix"),
	  function(e1,e2) callGeneric(diag2Tsmart(e1,e2, "d"), e2))
setMethod("Ops", signature(e1 = "sparseMatrix", e2 = "ddiMatrix"),
	  function(e1,e2) callGeneric(e1, diag2Tsmart(e2,e1, "d")))
## ldi*
setMethod("Ops", signature(e1 = "ldiMatrix", e2 = "sparseMatrix"),
	  function(e1,e2) callGeneric(diag2Tsmart(e1,e2, "l"), e2))
setMethod("Ops", signature(e1 = "sparseMatrix", e2 = "ldiMatrix"),
	  function(e1,e2) callGeneric(e1, diag2Tsmart(e2,e1, "l")))

## Ops:  Arith  --> numeric : "dMatrix"
##       Compare --> logical
##       Logic   --> logical: "lMatrix"

##  other = "numeric" : stay diagonal if possible
## ddi*: Arith: result numeric, potentially ddiMatrix
setMethod("Arith", signature(e1 = "ddiMatrix", e2 = "numeric"),
	  function(e1,e2) {
	      n <- e1@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(0, e2)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e2)) == 1L
		  if(!L1 && le != nsq) e2 <- rep(e2, length.out = nsq)
		  if(e1@diag == "U" && any((r <- callGeneric(1, e2)) != 1)) {
		      e1@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1@x, e2)
		  e1@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e1)
	      }
	      callGeneric(diag2tT.u(e1,e2, "d"), e2)
	  })

setMethod("Arith", signature(e1 = "numeric", e2 = "ddiMatrix"),
	  function(e1,e2) {
	      n <- e2@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(e1, 0)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e1)) == 1L
		  if(!L1 && le != nsq) e1 <- rep(e1, length.out = nsq)
		  if(e2@diag == "U" && any((r <- callGeneric(e1, 1)) != 1)) {
		      e2@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1, e2@x)
		  e2@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e2)
	      }
	      callGeneric(e1, diag2tT.u(e2,e1, "d"))
	  })

## ldi* Arith --> result numeric, potentially ddiMatrix
setMethod("Arith", signature(e1 = "ldiMatrix", e2 = "numeric"),
	  function(e1,e2) {
	      n <- e1@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(0, e2)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e2)) == 1L
		  if(!L1 && le != nsq) e2 <- rep(e2, length.out = nsq)
		  if(e1@diag == "U" && any((r <- callGeneric(1, e2)) != 1)) {
		      e1@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1@x, e2)
		  e1 <- copyClass(e1, "ddiMatrix", c("diag", "Dim", "Dimnames"))
		  e1@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e1)
	      }
	      callGeneric(diag2tT.u(e1,e2, "d"), e2)
	  })

setMethod("Arith", signature(e1 = "numeric", e2 = "ldiMatrix"),
	  function(e1,e2) {
	      n <- e2@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(e1, 0)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e1)) == 1L
		  if(!L1 && le != nsq) e1 <- rep(e1, length.out = nsq)
		  if(e2@diag == "U" && any((r <- callGeneric(e1, 1)) != 1)) {
		      e2@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1, e2@x)
		  e2 <- copyClass(e2, "ddiMatrix", c("diag", "Dim", "Dimnames"))
		  e2@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e2)
	      }
	      callGeneric(e1, diag2tT.u(e2,e1, "d"))
	  })

## ddi*: for "Ops" without Arith --> result logical, potentially ldi
setMethod("Ops", signature(e1 = "ddiMatrix", e2 = "numeric"),
	  function(e1,e2) {
	      n <- e1@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(0, e2)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e2)) == 1L
		  if(!L1 && le != nsq) e2 <- rep(e2, length.out = nsq)
		  if(e1@diag == "U" && any((r <- callGeneric(1, e2)) != 1)) {
		      e1@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1@x, e2)
		  e1 <- copyClass(e1, "ldiMatrix", c("diag", "Dim", "Dimnames"))
		  e1@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e1)
	      }
	      callGeneric(diag2tT.u(e1,e2, "l"), e2)
	  })

setMethod("Ops", signature(e1 = "numeric", e2 = "ddiMatrix"),
	  function(e1,e2) {
	      n <- e2@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(e1, 0)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e1)) == 1L
		  if(!L1 && le != nsq) e1 <- rep(e1, length.out = nsq)
		  if(e2@diag == "U" && any((r <- callGeneric(e1, 1)) != 1)) {
		      e2@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1, e2@x)
		  e2 <- copyClass(e2, "ldiMatrix", c("diag", "Dim", "Dimnames"))
		  e2@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e2)
	      }
	      callGeneric(e1, diag2tT.u(e2,e1, "l"))
	  })

## ldi*: for "Ops" without Arith --> result logical, potentially ldi
setMethod("Ops", signature(e1 = "ldiMatrix", e2 = "numeric"),
	  function(e1,e2) {
	      n <- e1@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(FALSE, e2)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e2)) == 1L
		  if(!L1 && le != nsq) e2 <- rep(e2, length.out = nsq)
		  if(e1@diag == "U" && any((r <- callGeneric(TRUE, e2)) != 1)) {
		      e1@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1@x, e2)
		  e1@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e1)
	      }
	      callGeneric(diag2tT.u(e1,e2, "l"), e2)
	  })

setMethod("Ops", signature(e1 = "numeric", e2 = "ldiMatrix"),
	  function(e1,e2) {
	      n <- e2@Dim[1]; nsq <- n^2
	      f0 <- callGeneric(e1, FALSE)
	      if(all(is0(f0))) { # remain diagonal
		  L1 <- (le <- length(e1)) == 1L
		  if(!L1 && le != nsq) e1 <- rep(e1, length.out = nsq)
		  if(e2@diag == "U" && any((r <- callGeneric(e1, TRUE)) != 1)) {
		      e2@diag <- "N"
		      if(L1) r <- rep.int(r, n)
		  } else
		      r <- callGeneric(e1, e2@x)
		  e2@x <- if(L1) r else r[1L + (n+1L)*(0:(n-1L))]
		  return(e2)
	      }
	      callGeneric(e1, diag2tT.u(e2,e1, "l"))
	  })



## Not {"sparseMatrix", "numeric} :  {"denseMatrix", "matrix", ... }
for(other in c("ANY", "Matrix", "dMatrix")) {
    ## ddi*:
    setMethod("Ops", signature(e1 = "ddiMatrix", e2 = other),
	      function(e1,e2) callGeneric(diag2tT.u(e1,e2, "d"), e2))
    setMethod("Ops", signature(e1 = other, e2 = "ddiMatrix"),
	      function(e1,e2) callGeneric(e1, diag2tT.u(e2,e1, "d")))
    ## ldi*:
    setMethod("Ops", signature(e1 = "ldiMatrix", e2 = other),
	      function(e1,e2) callGeneric(diag2tT.u(e1,e2, "l"), e2))
    setMethod("Ops", signature(e1 = other, e2 = "ldiMatrix"),
	      function(e1,e2) callGeneric(e1, diag2tT.u(e2,e1, "l")))
}

## Direct subclasses of "denseMatrix": currently ddenseMatrix, ldense... :
dense.subCl <- local({ dM.scl <- getClass("denseMatrix")@subclasses
                       names(dM.scl)[sapply(dM.scl, slot, "distance") == 1] })
for(DI in diCls) {
    for(c2 in c(dense.subCl, "Matrix")) {
	for(Fun in c("*", "^", "&")) {
	    setMethod(Fun, signature(e1 = DI, e2 = c2),
		      function(e1,e2) callGeneric(e1, Diagonal(x = diag(e2))))
	    setMethod(Fun, signature(e1 = c2, e2 = DI),
		      function(e1,e2) callGeneric(Diagonal(x = diag(e1)), e2))
	}
	## NB: This arguably implicitly uses  0/0 :== 0	 to keep diagonality
	for(Fun in c("%%", "%/%", "/")) {
	    setMethod(Fun, signature(e1 = DI, e2 = c2),
		      function(e1,e2) callGeneric(e1, Diagonal(x = diag(e2))))
	}
    }
}


### "Summary" : "max"   "min"   "range" "prod"  "sum"   "any"   "all"
### ----------   the last 4: separately here
for(cl in diCls) {
setMethod("any", cl,
	  function (x, ..., na.rm) {
	      if(any(x@Dim == 0)) FALSE
	      else if(x@diag == "U") TRUE else any(x@x, ..., na.rm = na.rm)
	  })
setMethod("all",  cl, function (x, ..., na.rm) {
    n <- x@Dim[1]
    if(n >= 2) FALSE
    else if(n == 0 || x@diag == "U") TRUE
    else all(x@x, ..., na.rm = na.rm)
})
setMethod("prod", cl, function (x, ..., na.rm) {
    n <- x@Dim[1]
    if(n >= 2) 0
    else if(n == 0 || x@diag == "U") 1
    else ## n == 1, diag = "N" :
	prod(x@x, ..., na.rm = na.rm)
})

setMethod("sum", cl,
	  function(x, ..., na.rm) {
	      r <- sum(x@x, ..., na.rm = na.rm)# double or integer, correctly
	      if(x@diag == "U" && !is.na(r)) r + x@Dim[1] else r
	  })
}

## The remaining ones are  max, min, range :

setMethod("Summary", "ddiMatrix",
	  function(x, ..., na.rm) {
	      if(any(x@Dim == 0)) callGeneric(numeric(0), ..., na.rm=na.rm)
	      else if(x@diag == "U")
		  callGeneric(x@x, 0, 1, ..., na.rm=na.rm)
	      else callGeneric(x@x, 0, ..., na.rm=na.rm)
	  })
setMethod("Summary", "ldiMatrix",
	  function(x, ..., na.rm) {
	      if(any(x@Dim == 0)) callGeneric(logical(0), ..., na.rm=na.rm)
	      else if(x@diag == "U")
		  callGeneric(x@x, FALSE, TRUE, ..., na.rm=na.rm)
	      else callGeneric(x@x, FALSE, ..., na.rm=na.rm)
	  })



## similar to prTriang() in ./Auxiliaries.R :
prDiag <-
    function(x, digits = getOption("digits"), justify = "none", right = TRUE)
{
    cf <- array(".", dim = x@Dim, dimnames = x@Dimnames)
    cf[row(cf) == col(cf)] <-
        sapply(diag(x), format, digits = digits, justify = justify)
    print(cf, quote = FALSE, right = right)
    invisible(x)
}

## somewhat consistent with "print" for sparseMatrix :
setMethod("print", signature(x = "diagonalMatrix"), prDiag)

setMethod("show", signature(object = "diagonalMatrix"),
	  function(object) {
	      d <- dim(object)
	      cl <- class(object)
	      cat(sprintf('%d x %d diagonal matrix of class "%s"',
			  d[1], d[2], cl))
	      if(d[1] < 50) {
		  cat("\n")
		  prDiag(object)
	      } else {
		  cat(", with diagonal entries\n")
		  show(diag(object))
		  invisible(object)
	      }
	  })
