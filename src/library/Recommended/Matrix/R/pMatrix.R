#### Permutation Matrices -- Coercion and Methods

## The typical   'constructor' : coerce from  'index'
setAs("integer", "pMatrix",
      function(from) {
          n <- length(from)
          nn <- names(from)
          new("pMatrix", Dim = rep.int(n, 2), Dimnames = list(nn,nn),
              perm = from)
      })

setAs("numeric", "pMatrix",
      function(from)
	  if(all(from == (i <- as.integer(from)))) as(i, "pMatrix")
	  else stop("coercion to 'pMatrix' only works from integer numeric"))

setAs("pMatrix", "matrix",
      function(from) {
	  fp <- from@perm
	  r <- ldiag(n = length(fp))[fp,]
	  if(.has.DN(from)) dimnames(r) <- from@Dimnames
	  r
      })

## coerce to 0/1 sparse matrix, i.e. sparse pattern
setAs("pMatrix", "ngTMatrix",
      function(from) {
          d <- from@Dim
	  new("ngTMatrix", i = seq_len(d[1]) - 1L, j = from@perm - 1L,
              Dim = d, Dimnames = from@Dimnames)
      })

setAs("pMatrix", "TsparseMatrix", function(from) as(from, "ngTMatrix"))
setAs("pMatrix", "nMatrix",	  function(from) as(from, "ngTMatrix"))
setAs("pMatrix", "lMatrix", function(from) as(as(from, "nMatrix"), "lMatrix"))
setAs("pMatrix", "dMatrix", function(from) as(as(from, "nMatrix"), "dMatrix"))
setAs("pMatrix", "dsparseMatrix", function(from) as(from, "dMatrix"))
setAs("pMatrix", "nsparseMatrix", function(from) as(from, "nMatrix"))
setAs("pMatrix", "CsparseMatrix",
      function(from) as(as(from, "ngTMatrix"), "CsparseMatrix"))
setAs("pMatrix", "ngeMatrix", function(from) as(as(from, "ngTMatrix"),"ngeMatrix"))

setAs("nMatrix", "pMatrix",
      function(from) {
	  from <- as(as(from, "TsparseMatrix"), "ngTMatrix")
	  n <- (d <- from@Dim)[1]
	  if(n != d[2]) stop("not a square matrix")
	  if(length(i <- from@i) != n)
	      stop("the number of non-zero entries differs from nrow(.)")
	  if((need.sort <- is.unsorted(i))) {
	      ii <- sort.list(i)
	      i <- i[ii]
	  }
	  if(n >= 1 && !identical(i, 0:(n - 1)))
	      stop("must have exactly one non-zero entry per row")
	  new("pMatrix", ## validity checking checks the 'perm' slot:
	      perm = 1L + if(need.sort) from@j[ii] else from@j,
	      Dim = d, Dimnames = from@Dimnames)
      })

setAs("matrix", "pMatrix", function(from) as(as(from, "nMatrix"), "pMatrix"))

setAs("sparseMatrix", "pMatrix", function(from)
      as(as(from, "nsparseMatrix"), "pMatrix"))

setMethod("is.na", signature(x = "pMatrix"), is.na_nsp)

## how much faster would this be in C? -- less than a factor of two?
.inv.perm <- function(p) { p[p] <- seq_along(p) ; p }

setMethod("solve", signature(a = "pMatrix", b = "missing"),
	  function(a, b, ...) {
              a@perm <- .inv.perm(a@perm)
              a@Dimnames <- a@Dimnames[2:1]
              a
          })

setMethod("solve", signature(a = "pMatrix", b = "Matrix"),
	  function(a, b, ...) crossprod(a, b))
setMethod("solve", signature(a = "pMatrix", b = "matrix"),
	  function(a, b, ...) crossprod(a, b))

setMethod("solve", signature(a = "Matrix", b = "pMatrix"),
	  function(a, b, ...)
	  ## Or alternatively  solve(a, as(b, "CsparseMatrix"))
	  solve(a)[, .inv.perm(b@perm)])


setMethod("determinant", signature(x = "pMatrix", logarithm = "logical"),
	  function(x, logarithm, ...)
	  mkDet(logarithm=logarithm, ldet = 0, sig = signPerm(x@perm)))

## t(pM) is == the inverse  pM^(-1):
setMethod("t", signature(x = "pMatrix"), function(x) solve(x))


setMethod("%*%", signature(x = "matrix", y = "pMatrix"),
	  function(x, y) { mmultCheck(x,y); x[, .inv.perm(y@perm)] })
setMethod("%*%", signature(x = "Matrix", y = "pMatrix"),
	  function(x, y) { mmultCheck(x,y); x[, .inv.perm(y@perm)] })

setMethod("%*%", signature(x = "pMatrix", y = "matrix"),
	  function(x, y) { mmultCheck(x,y); y[x@perm ,] })
setMethod("%*%", signature(x = "pMatrix", y = "Matrix"),
	  function(x, y) { mmultCheck(x,y); y[x@perm ,] })

setMethod("%*%", signature(x = "pMatrix", y = "pMatrix"),
	  function(x, y) {
	      stopifnot(identical(d <- x@Dim, y@Dim))
	      ## n <- d[1]
	      ## FIXME: dimnames dealing: as with S3 matrix's  %*%
	      x@perm <- x@perm[y@perm]
	      x
	  })

setMethod("crossprod", signature(x = "pMatrix", y = "matrix"),
	  function(x, y) { mmultCheck(x,y, 2L); y[.inv.perm(x@perm) ,]})
setMethod("crossprod", signature(x = "pMatrix", y = "Matrix"),
	  function(x, y) { mmultCheck(x,y, 2L); y[.inv.perm(x@perm) ,]})
setMethod("crossprod", signature(x = "pMatrix", y = "pMatrix"),
	  function(x, y) {
	      stopifnot(identical(x@Dim, y@Dim))
	      x@perm <- .inv.perm(x@perm)[y@perm]
	      x
	  })

setMethod("tcrossprod", signature(x = "matrix", y = "pMatrix"),
	  function(x, y) { mmultCheck(x,y, 3L); x[, y@perm] })
setMethod("tcrossprod", signature(x = "Matrix", y = "pMatrix"),
	  function(x, y) { mmultCheck(x,y, 3L); x[, y@perm] })
setMethod("tcrossprod", signature(x = "pMatrix", y = "pMatrix"),
	  function(x, y) {
	      stopifnot(identical(x@Dim, y@Dim))
	      x@perm <- x@perm[.inv.perm(y@perm)]
	      x
	  })


setMethod("crossprod", signature(x = "pMatrix", y = "missing"),
          function(x, y=NULL) Diagonal(nrow(x)))
setMethod("tcrossprod", signature(x = "pMatrix", y = "missing"),
          function(x, y=NULL) Diagonal(nrow(x)))


.pMat.nosense <- function (x, i, j, ..., value)
    stop('replacing "pMatrix" entries is not allowed, as rarely sensible')
setReplaceMethod("[", signature(x = "pMatrix", i = "index"), .pMat.nosense)
setReplaceMethod("[", signature(x = "pMatrix", i = "missing", j = "index"),
		 .pMat.nosense) ##   explicit  ^^^^^^^^^^^^ for disambiguation
setReplaceMethod("[", signature(x = "pMatrix", i = "missing", j = "missing"),
		 .pMat.nosense)
