#### Containing all  cbind2() and rbind2() methods for all our Matrices

###-- General -----------------------------------------------------------

###-- Dense, incl Diagonal ----------------------------------------------

###-- Sparse ------------------------------------------------------------

setMethod("cbind2", signature(x = "sparseMatrix", y = "matrix"),
	  function(x, y) cbind2(x, .Call(dense_to_Csparse, y)))
setMethod("cbind2", signature(x = "matrix", y = "sparseMatrix"),
	  function(x, y) cbind2(.Call(dense_to_Csparse, x), y))
setMethod("rbind2", signature(x = "sparseMatrix", y = "matrix"),
	  function(x, y) rbind2(x, .Call(dense_to_Csparse, y)))
setMethod("rbind2", signature(x = "matrix", y = "sparseMatrix"),
	  function(x, y) rbind2(.Call(dense_to_Csparse, x), y))

## originally from ./Matrix.R : -------------------------------

## The trivial methods :
setMethod("cbind2", signature(x = "Matrix", y = "NULL"),
          function(x, y) x)
setMethod("cbind2", signature(x = "Matrix", y = "missing"),
          function(x, y) x)
setMethod("cbind2", signature(x = "NULL", y="Matrix"),
          function(x, y) x)
## using "atomicVector" not just "numeric"
setMethod("cbind2", signature(x = "Matrix", y = "atomicVector"),
	  function(x, y) cbind2(x, matrix(y, nrow = nrow(x))))
setMethod("cbind2", signature(x = "atomicVector", y = "Matrix"),
	  function(x, y) cbind2(matrix(x, nrow = nrow(y)), y))
setMethod("cbind2", signature(x = "ANY", y = "Matrix"),
	  function(x, y) .bail.out.2(.Generic, class(x), class(y)))
setMethod("cbind2", signature(x = "Matrix", y = "ANY"),
	  function(x, y) .bail.out.2(.Generic, class(x), class(y)))

setMethod("rbind2", signature(x = "Matrix", y = "NULL"),
          function(x, y) x)
setMethod("rbind2", signature(x = "Matrix", y = "missing"),
          function(x, y) x)
setMethod("rbind2", signature(x = "NULL", y="Matrix"),
          function(x, y) x)
setMethod("rbind2", signature(x = "Matrix", y = "atomicVector"),
	  function(x, y) rbind2(x, matrix(y, ncol = ncol(x))))
setMethod("rbind2", signature(x = "atomicVector", y = "Matrix"),
	  function(x, y) rbind2(matrix(x, ncol = ncol(y)), y))
setMethod("rbind2", signature(x = "ANY", y = "Matrix"),
	  function(x, y) .bail.out.2(.Generic, class(x), class(y)))
setMethod("rbind2", signature(x = "Matrix", y = "ANY"),
	  function(x, y) .bail.out.2(.Generic, class(x), class(y)))

## Makes sure one gets x decent error message for the unimplemented cases:
setMethod("cbind2", signature(x = "Matrix", y = "Matrix"),
          function(x, y) {
              rowCheck(x,y)
              stop(gettextf("cbind2() method for (%s,%s) not-yet defined",
                            class(x), class(y)))
          })

## Use a working fall back {particularly useful for sparse}:
## FIXME: implement rbind2 via "cholmod" for C* and Tsparse ones
setMethod("rbind2", signature(x = "Matrix", y = "Matrix"),
          function(x, y) {
              colCheck(x,y)
              t(cbind2(t(x), t(y)))
          })

## originally from ./denseMatrix.R : -------------------------------

### cbind2
setMethod("cbind2", signature(x = "denseMatrix", y = "numeric"),
	  function(x, y) {
	      d <- dim(x); nr <- d[1]; nc <- d[2]
	      y <- rep(y, length.out = nr) # 'silent procrustes'
	      ## beware of (packed) triangular, symmetric, ...
	      x <- as(x, geClass(x))
	      x@x <- c(x@x, as.double(y))
	      x@Dim[2] <- nc + 1L
	      if(is.character(dn <- x@Dimnames[[2]]))
		  x@Dimnames[[2]] <- c(dn, "")
	      x
	  })
## the same, (x,y) <-> (y,x):
setMethod("cbind2", signature(x = "numeric", y = "denseMatrix"),
	  function(x, y) {
	      d <- dim(y); nr <- d[1]; nc <- d[2]
	      x <- rep(x, length.out = nr)
	      y <- as(y, geClass(y))
	      y@x <- c(as.double(x), y@x)
	      y@Dim[2] <- nc + 1L
	      if(is.character(dn <- y@Dimnames[[2]]))
		  y@Dimnames[[2]] <- c("", dn)
	      y
	  })


setMethod("cbind2", signature(x = "denseMatrix", y = "matrix"),
	  function(x, y) cbind2(x, as_geSimpl(y)))
setMethod("cbind2", signature(x = "matrix", y = "denseMatrix"),
	  function(x, y) cbind2(as_geSimpl(x), y))

setMethod("cbind2", signature(x = "denseMatrix", y = "denseMatrix"),
	  function(x, y) {
	      nr <- rowCheck(x,y)
	      ncx <- x@Dim[2]
	      ncy <- y@Dim[2]
	      ## beware of (packed) triangular, symmetric, ...
	      hasDN <- !is.null(dnx <- dimnames(x)) | !is.null(dny <- dimnames(y))
	      x <- as(x, geClass(x))
	      y <- as(y, geClass(y))
	      x@x <- c(x@x, y@x)
	      x@Dim[2] <- ncx + ncy
	      if(hasDN) {
		  ## R and S+ are different in which names they take
		  ## if they differ -- but there's no warning in any case
		  rn <- if(!is.null(dnx[[1]])) dnx[[1]] else dny[[1]]
		  cx <- dnx[[2]] ; cy <- dny[[2]]
		  cn <- if(is.null(cx) && is.null(cy)) NULL
		  else c(if(!is.null(cx)) cx else rep.int("", ncx),
			 if(!is.null(cy)) cy else rep.int("", ncy))
		  x@Dimnames <- list(rn, cn)
	      }
	      x
	  })

### rbind2 -- analogous to cbind2 --- more to do for @x though:

setMethod("rbind2", signature(x = "denseMatrix", y = "numeric"),
	  function(x, y) {
	      if(is.character(dn <- x@Dimnames[[1]])) dn <- c(dn, "")
	      y <- rbind2(as(x,"matrix"), y)
	      new(paste(.M.kind(y), "geMatrix", sep=''), x = c(y),
                  Dim = x@Dim + 1:0, Dimnames = list(dn, x@Dimnames[[2]]))
	  })
## the same, (x,y) <-> (y,x):
setMethod("rbind2", signature(x = "numeric", y = "denseMatrix"),
	  function(x, y) {
	      if(is.character(dn <- y@Dimnames[[1]])) dn <- c("", dn)
	      x <- rbind2(x, as(y,"matrix"))
	      new(paste(.M.kind(x), "geMatrix", sep=''), x = c(x),
                  Dim = y@Dim + 1:0, Dimnames = list(dn, y@Dimnames[[2]]))
	  })

setMethod("rbind2", signature(x = "denseMatrix", y = "matrix"),
	  function(x, y) rbind2(x, as_geSimpl(y)))
setMethod("rbind2", signature(x = "matrix", y = "denseMatrix"),
	  function(x, y) rbind2(as_geSimpl(x), y))

setMethod("rbind2", signature(x = "denseMatrix", y = "denseMatrix"),
	  function(x, y) {
	      nc <- colCheck(x,y)
	      nrx <- x@Dim[1]
	      nry <- y@Dim[1]
	      dn <-
		  if(!is.null(dnx <- dimnames(x)) |
		     !is.null(dny <- dimnames(y))) {
		      ## R and S+ are different in which names they take
		      ## if they differ -- but there's no warning in any case
		      list(if(is.null(rx <- dnx[[1]]) & is.null(ry <- dny[[1]]))
			   NULL else
			   c(if(!is.null(rx)) rx else rep.int("", nrx),
			     if(!is.null(ry)) ry else rep.int("", nry)),
			   if(!is.null(dnx[[2]])) dnx[[2]] else dny[[2]])

		  } else list(NULL, NULL)
	      ## beware of (packed) triangular, symmetric, -> "cheap" (FIXME):
              x <- rbind2(as(x,"matrix"), as(y,"matrix"))
	      new(paste(.M.kind(x), "geMatrix", sep=''), x = c(x),
                  Dim = c(nrx + nry, nc), Dimnames = dn)
	  })

## originally from ./diagMatrix.R : --------------------------------------

## For diagonalMatrix:  preserve sparseness {not always optimal, but "the law"}

## hack to suppress the obnoxious dispatch ambiguity warnings:
diag2Sp <- function(x) suppressWarnings(as(x, "CsparseMatrix"))

setMethod("cbind2", signature(x = "diagonalMatrix", y = "sparseMatrix"),
	  function(x,y) cbind2(diag2Sp(x), as(y,"CsparseMatrix")))
setMethod("cbind2", signature(x = "sparseMatrix", y = "diagonalMatrix"),
	  function(x,y) cbind2(as(x,"CsparseMatrix"), diag2Sp(y)))
setMethod("rbind2", signature(x = "diagonalMatrix", y = "sparseMatrix"),
	  function(x,y) rbind2(diag2Sp(x), as(y,"CsparseMatrix")))
setMethod("rbind2", signature(x = "sparseMatrix", y = "diagonalMatrix"),
	  function(x,y) rbind2(as(x,"CsparseMatrix"), diag2Sp(y)))

## in order to evade method dispatch ambiguity, but still remain "general"
## we use this hack instead of signature  x = "diagonalMatrix"
for(cls in names(getClass("diagonalMatrix")@subclasses)) {

 setMethod("cbind2", signature(x = cls, y = "matrix"),
	   function(x,y) cbind2(diag2Sp(x), .Call(dense_to_Csparse, y)))
 setMethod("cbind2", signature(x = "matrix", y = cls),
	   function(x,y) cbind2(.Call(dense_to_Csparse, x), diag2Sp(y)))
 setMethod("rbind2", signature(x = cls, y = "matrix"),
	   function(x,y) rbind2(diag2Sp(x), .Call(dense_to_Csparse, y)))
 setMethod("rbind2", signature(x = "matrix", y = cls),
	   function(x,y) rbind2(.Call(dense_to_Csparse, x), diag2Sp(y)))

 ## These are already defined for "Matrix"
 ## -- repeated here for method dispatch disambiguation	 {"design-FIXME" ?}
 setMethod("cbind2", signature(x = cls, y = "atomicVector"),
	   function(x, y) cbind2(x, matrix(y, nrow = nrow(x))))
 setMethod("cbind2", signature(x = "atomicVector", y = cls),
	   function(x, y) cbind2(matrix(x, nrow = nrow(y)), y))
 setMethod("rbind2", signature(x = cls, y = "atomicVector"),
	   function(x, y) rbind2(x, matrix(y, ncol = ncol(x))))
 setMethod("rbind2", signature(x = "atomicVector", y = cls),
	   function(x, y) rbind2(matrix(x, ncol = ncol(y)), y))
}


## originally from ./dsparseMatrix.R : --------------------------------

setMethod("cbind2", signature(x = "sparseMatrix", y = "sparseMatrix"),
	  function(x, y) {
	      nr <- rowCheck(x,y)
	      ## beware of (packed) triangular, symmetric, ...
	      hasDN <- !identical(c(dnx <- dimnames(x),
				    dny <- dimnames(y)),
				  list(NULL,NULL,NULL,NULL))
	      ans <- .Call(Csparse_horzcat, as_Csp2(x), as_Csp2(y))
	      if(hasDN) {
		  ## R and S+ are different in which names they take
		  ## if they differ -- but there's no warning in any case
		  rn <- if(!is.null(dnx[[1]])) dnx[[1]] else dny[[1]]
		  cx <- dnx[[2]] ; cy <- dny[[2]]
		  cn <- if(is.null(cx) && is.null(cy)) NULL
		  else c(if(!is.null(cx)) cx else rep.int("", ncol(x)),
			 if(!is.null(cy)) cy else rep.int("", ncol(y)))
		  ans@Dimnames <- list(rn, cn)
	      }
	      ans
	  })

setMethod("rbind2", signature(x = "sparseMatrix", y = "sparseMatrix"),
	  function(x, y) {
	      nc <- colCheck(x,y)
	      ## beware of (packed) triangular, symmetric, ...
	      hasDN <- !identical(c(dnx <- dimnames(x),
				    dny <- dimnames(y)),
				  list(NULL,NULL,NULL,NULL))
	      ans <- .Call(Csparse_vertcat, as_Csp2(x), as_Csp2(y))
	      if(hasDN) {
		  ## R and S+ are different in which names they take
		  ## if they differ -- but there's no warning in any case
		  cn <- if(!is.null(dnx[[2]])) dnx[[2]] else dny[[2]]
		  rx <- dnx[[1]] ; ry <- dny[[1]]
		  rn <- if(is.null(rx) && is.null(ry)) NULL
		  else c(if(!is.null(rx)) rx else rep.int("", nrow(x)),
			 if(!is.null(ry)) ry else rep.int("", nrow(y)))
		  ans@Dimnames <- list(rn, cn)
	      }
	      ans
	  })

if(FALSE) {
    ## FIXME
    ##------------- maybe a bit faster --- but too much to maintain
    ## would have to be done for "rbind2" as well ...
setMethod("cbind2", signature(x = "sparseMatrix", y = "numeric"),
          function(x, y) {
              d <- dim(x); nr <- d[1]; nc <- d[2]; cl <- class(x)
              x <- as(x, "CsparseMatrix")
              if(nr > 0) {
                  y <- rep(y, length.out = nr) # 'silent procrustes'
                  n0y <- y != 0
                  n.e <- length(x@i)
                  x@i <- c(x@i, (0:(nr-1))[n0y])
                  x@p <- c(x@p, n.e + sum(n0y))
                  x@x <- c(x@x, y[n0y])
              } else { ## nr == 0

              }
              x@Dim[2] <- nc + 1L
              if(is.character(dn <- x@Dimnames[[2]]))
                  x@Dimnames[[2]] <- c(dn, "")
              x
          })
## the same, (x,y) <-> (y,x):
setMethod("cbind2", signature(x = "numeric", y = "sparseMatrix"),
          function(x, y) {
              d <- dim(y); nr <- d[1]; nc <- d[2]; cl <- class(y)
              y <-  as(y, "CsparseMatrix")
              if(nr > 0) {
                  x <- rep(x, length.out = nr) # 'silent procrustes'
                  n0x <- x != 0
                  y@i <- c((0:(nr-1))[n0x], y@i)
                  y@p <- c(0:0, sum(n0x) + y@p)
                  y@x <- c(x[n0x], y@x)
              } else { ## nr == 0

              }
              y@Dim[2] <- nc + 1L
              if(is.character(dn <- y@Dimnames[[2]]))
                  y@Dimnames[[2]] <- c(dn, "")
              y
          })

}## -- no longer
