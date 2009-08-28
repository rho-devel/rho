#### "Namespace private" Auxiliaries  such as method functions
#### (called from more than one place --> need to be defined early)

## Need to consider NAs ;  "== 0" even works for logical & complex:
is0  <- function(x) !is.na(x) & x == 0
isN0 <- function(x)  is.na(x) | x != 0
all0 <- function(x) !any(is.na(x)) && all(x == 0)

## These work "identically" for  1 ('==' TRUE)  and 0 ('==' FALSE):
## TODO: C versions of these would be faster
allTrue  <- function(x) all(x)  && !any(is.na(x))
allFalse <- function(x) !any(x) && !any(is.na(x))

as1 <- function(x, mod=mode(x))
    switch(mod, "integer" = 1L, "numeric" = 1, "logical" = TRUE, "complex" = 1+0i)
as0 <- function(x, mod=mode(x))
    switch(mod, "integer" = 0L, "numeric" = 0, "logical" = FALSE, "complex" = 0+0i)


## maybe we should have this in base, maybe via an .Internal(paste0(.)) -> do_paste(.. op=2)
paste0 <- function(...) paste(..., sep = '')

## For %*% (M = Matrix; v = vector (double or integer {complex maybe?}):
.M.v <- function(x, y) { dim(y) <- c(length(y), 1L); callGeneric(x, y) }
.v.M <- function(x, y) { dim(x) <- c(1L, length(x)); callGeneric(x, y) }

.M.DN <- function(x) if(!is.null(dn <- dimnames(x))) dn else list(NULL,NULL)

.if.NULL <- function(x, orElse) if(!is.null(x)) x else orElse

.has.DN <- ## has non-trivial Dimnames slot?
    function(x) !identical(list(NULL,NULL), x@Dimnames)

.bail.out.1 <- function(fun, cl) {
    stop(gettextf('not-yet-implemented method for %s(<%s>).\n ->>  Ask the package authors to implement the missing feature.', fun, cl),
	 call. = FALSE)
}
.bail.out.2 <- function(fun, cl1, cl2) {
    stop(gettextf('not-yet-implemented method for %s(<%s>, <%s>).\n ->>  Ask the package authors to implement the missing feature.',
		  fun, cl1, cl2), call. = FALSE)
}

Matrix.msg <- function(..., .M.level = 1) {
    if(!is.null(v <- getOption("Matrix.verbose")) && v >= .M.level)
        message(...)
}

## we can set this to FALSE and possibly measure speedup:
.copyClass.check <- TRUE

## This should be done in C and be exported by 'methods':  [FIXME - ask JMC ]
copyClass <- function(x, newCl, sNames =
		      intersect(slotNames(newCl), slotNames(x)),
                      check = .copyClass.check)
{
    r <- new(newCl)
    ## Equivalent of
    ##   for(n in sNames) slot(r, n, check=check) <- slot(x, n)  :
    if(check) for(n in sNames) slot(r, n) <- slot(x, n)
    else for(n in sNames) # don't check, be fast
	.Call("R_set_slot", r, n, slot(x,n), PACKAGE = "methods")
    r
}

MatrixClass <- function(cl, cld = getClassDef(cl),
			...Matrix = TRUE, dropVirtual = TRUE, ...)
{
    ## Purpose: return the (maybe super-)class of class 'cl'   from "Matrix",
    ##		returning  character(0) if there is none.
    ## ----------------------------------------------------------------------
    ## Arguments: cl: string, class name
    ##		 cld: its class definition
    ##	   ...Matrix: if TRUE, the result must be of pattern "...Matrix"
    ##	      ..... : other arguments are passed to .selectSuperClasses()
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 24 Mar 2009

    ## stopifnot(is.character(cl))
    ## Hmm, packageSlot(cl)  *can* be misleading --> use  cld@package  first:
    if(is.null(pkg <- cld@package)) {
	if(is.null(pkg <- packageSlot(cl))) return(character())
	## else we use 'pkg'
    }
    if(identical(pkg, "Matrix") &&
       (!...Matrix || identical(1L, grep("^...Matrix$", cl))))
	cl
    else { ## possibly recursively
	r <- .selectSuperClasses(cld@contains, dropVirtual = dropVirtual,
				 namesOnly = TRUE, ...)
	if(length(r))
	    Recall(r[1], ...Matrix = ...Matrix, dropVirtual = dropVirtual)
	else r
    }
}

attrSlotNames <- function(m) {
    ## slotnames of Matrix objects which are *not* directly content related
    sn <- slotNames(m); sn[!(sn %in% c("x","i","j","p"))]
}

##' @param m
##' @return the slots of 'm' which are "attributes" of some kind.
attrSlots <- function(m) sapply(attrSlotNames(m), function(sn) slot(m, sn),
				simplify = FALSE)

attr.all_Mat <- function(target, current, check.attributes = TRUE, ...) {
    msg <- if(check.attributes)
	attr.all.equal(attrSlots(target), attrSlots(current), ...)
    if((c1 <- class(target)) != (c2 <- class(current)))

	## list(): so we can easily check for this
	list(c(msg, paste("class(target) is ", class(target), ", current is ",
				 class(current), sep = "")))
    else msg
}



## chol() via "dpoMatrix"
cholMat <- function(x, pivot = FALSE, ...) {
    ## This will only be called for *dense* matrices
    px <- as(x, if(length(x@x) < prod(dim(x))) ## packed
	     "dppMatrix" else "dpoMatrix")
    if (isTRUE(validObject(px, test=TRUE))) chol(px, pivot, ...)
    else stop("'x' is not positive definite -- chol() undefined.")
}

##  sign( <permutation> ) == determinant( <pMatrix>)

signPerm <- function(p)
{
    ## Purpose: sign(<permutation>) via the cycles
    ## ----------------------------------------------------------------------
    ## Arguments: a permutation of 1:n
    ## ----------------------------------------------------------------------
    ## Author: Peter Dalgaard, 14 Apr 2008 // speedup: Martin Maechler 2008-04-16

    n <- length(p)
    x <- integer(n)
    ii <- seq_len(n)
    for (i in ii) {
	z <- ii[!x][1]             # index of first unmarked x[] entry
	if (is.na(z)) break
	repeat { ## mark x[] <- i  for those in i-th cycle
	    x[z] <- i
	    z <- p[z]
	    if (x[z]) break
	}
    }
    ## Now, table(x) gives the cycle lengths,
    ## where  split(seq_len(n), x)  would give the cycles list
    ## tabulate(x, i - 1L) is quite a bit faster than the equivalent
    ## table(x)
    clen <- tabulate(x, i - 1L)
    ## The sign is -1 (<==>  permutation is odd)  iff
    ## the cycle factorization contains an odd number of even-length cycles:
    1L - (sum(clen %% 2 == 0) %% 2L)*2L
}


detSparseLU <- function(x, logarithm = TRUE, ...) {
    ## Purpose: Compute determinant() from  lu.x = lu(x)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 15 Apr 2008

    if(any(x@Dim == 0)) return(mkDet(numeric(0)))
    ll <- tryCatch(lu(x), error = function(e)e)

    if(inherits(ll, "error")) {
	## LU-decomposition failed:
        if(length(grep("singular", ll$message, fixed=TRUE)))
            ## <== Matrix singular and we behave as if "==>" was sure :
	    return(mkDet(ldet=if(any(is.na(x))) NaN else -Inf,
			 logarithm=logarithm, sig = 1L))
        else stop(ll$message, call. = FALSE)
    }
    ## else
    stopifnot(all(c("L","U") %in% slotNames(ll))) # ensure we have *sparse* LU
    r <- mkDet(diag(ll@U), logarithm)
    ## Det(x) == Det(P L U Q) == Det(P) * 1 * Det(U) * Det(Q); where Det(P), Det(Q) in {-1,1}
    r$sign <- r$sign * signPerm(ll@p + 1L) * signPerm(ll@q + 1L)
    r
}


## Log(Determinant) from diagonal ... used several times

mkDet <- function(d, logarithm = TRUE, ldet = sum(log(abs(d))),
                  sig = -1L+2L*as.integer(prod(sign(d)) >= 0))
{		# sig: -1 or +1 (not 0 !)
    modulus <- if (logarithm) ldet else exp(ldet)
    attr(modulus, "logarithm") <- logarithm
    val <- list(modulus = modulus, sign = sig)
    class(val) <- "det"
    val
}

dimCheck <- function(a, b) {
    da <- dim(a)
    db <- dim(b)
    if(any(da != db))
	stop(gettextf("Matrices must have same dimensions in %s",
		      deparse(sys.call(sys.parent()))),
	     call. = FALSE)
    da
}

mmultCheck <- function(a, b) {
    ca <- dim(a)[2]
    rb <- dim(b)[1]
    if(ca != rb)
	stop(gettextf("non-conformable matrix dimensions in %s",
		      deparse(sys.call(sys.parent()))),
	     call. = FALSE)
    ca
}

dimNamesCheck <- function(a, b) {
    ## Constructs "sensical" dimnames for something like  a + b ;
    ## assume dimCheck() has happened before
    nullDN <- list(NULL,NULL)
    h.a <- !identical(nullDN, dna <- dimnames(a))
    h.b <- !identical(nullDN, dnb <- dimnames(b))
    if(h.a || h.b) {
	if (!h.b) dna
	else if(!h.a) dnb
	else { ## both have non-trivial dimnames
	    r <- dna # "default" result
	    for(j in 1:2) if(!is.null(dn <- dnb[[j]])) {
		if(is.null(r[[j]]))
		    r[[j]] <- dn
		else if(!identical(r[[j]], dn))
		    warning(gettextf("dimnames [%d] mismatch in %s", j,
				     deparse(sys.call(sys.parent()))),
			    call. = FALSE)
	    }
	    r
	}
    }
    else
	nullDN
}

rowCheck <- function(a, b) {
    da <- dim(a)
    db <- dim(b)
    if(da[1] != db[1])
	stop(gettextf("Matrices must have same number of rows in %s",
		      deparse(sys.call(sys.parent()))),
	     call. = FALSE)
    ## return the common nrow()
    da[1]
}

colCheck <- function(a, b) {
    da <- dim(a)
    db <- dim(b)
    if(da[2] != db[2])
	stop(gettextf("Matrices must have same number of columns in %s",
		      deparse(sys.call(sys.parent()))),
	     call. = FALSE)
    ## return the common ncol()
    da[2]
}

## used for is.na(<nsparse>)  but not only:
is.na_nsp <- function(x) {
    d <- x@Dim
    dn <- x@Dimnames
    new(if(d[1] == d[2] && identical(dn[[1]], dn[[2]]))
	"nsCMatrix" else "ngCMatrix",
        Dim = d, Dimnames = dn, p = rep.int(0L, d[2]+1L))
}

## Note: !isPacked(.)  i.e. `full' still contains
## ----  "*sy" and "*tr" which have "undefined" lower or upper part
isPacked <- function(x)
{
    ## Is 'x' a packed (dense) matrix ?
    is(x, "denseMatrix") &&
    ## unneeded(!): any("x" == slotNames(x)) &&
    length(x@x) < prod(dim(x))
}

emptyColnames <- function(x, msg.if.not.empty = FALSE)
{
    ## Useful for compact printing of (parts) of sparse matrices
    ## possibly	 dimnames(x) "==" NULL :
    dn <- dimnames(x)
    d <- dim(x)
    if(msg.if.not.empty && is.list(dn) && length(dn) >= 2 &&
       is.character(cn <- dn[[2]]) && any(cn != "")) {
	lc <- length(cn)
	message(sprintf("   [[ suppressing %d column names %s%s ]]", d[2],
			paste(sQuote(cn[1:min(3, lc)]), collapse = ", "),
			if(lc > 3) " ..." else ""))
    }
    dimnames(x) <- list(dn[[1]], rep("", d[2]))
    x
}

idiag <- function(n, p=n)
{
    ## Purpose: diag() returning  *integer*
    ## --------------------------------------------------------
    ## Author: Martin Maechler, Date:  8 Dec 2007, 23:13
    r <- matrix(0L, n,p)
    if ((m <- min(n, p)) > 0)
	r[1 + 0:(m - 1) * (n + 1)] <- 1L
    r
}

ldiag <- function(n, p=n)
{
    ## Purpose: diag() returning  *logical*
    r <- matrix(FALSE, n,p)
    if ((m <- min(n, p)) > 0)
	r[1 + 0:(m - 1) * (n + 1)] <- TRUE
    r
}

## The indices of the diagonal entries of an  n x n matrix,  n >= 1
## i.e. indDiag(n) === which(diag(n) == 1)
indDiag <- function(n) cumsum(c(1L, rep.int(n+1L, n-1)))

### TODO:  write in C and port to base (or 'utils') R
### -----
### "Theory" behind this: /u/maechler/R/MM/MISC/lower-tri-w.o-matrix.R
indTri <- function(n, upper = TRUE, diag = FALSE) {
    ## Indices of strict upper/lower triangular part
    ## == which(upper.tri(diag(n), diag=diag) or
    ##	  which(lower.tri(diag(n), diag=diag) -- but
    ## more efficiently for largish 'n'
    stopifnot(length(n) == 1, n == (n. <- as.integer(n)), (n <- n.) >= 0)
    if(n <= 2) {
        if(n == 0) return(integer(0))
        if(n == 1) return(if(diag) 1L else integer(0))
        ## else n == 2
        v <- if(upper) 3L else 2L
	return(if(diag) c(1L, v, 4L) else v)
    }

    ## n >= 3 [also for n == 2 && diag (==TRUE)] :

    ## First, compute the 'diff(.)' of the result [fast, using integers]
    n. <- if(diag) n else n - 1L
    n1 <- n. - 1L
    ## all '1' but a few
    r <- rep.int(1L, choose(n.+1, 2) - 1)
    tt <- if(diag) 2L else 3L
    r[cumsum(if(upper) 1:n1 else n.:2)] <- if(upper) n:tt else tt:n
    ## now have differences; revert to "original":
    cumsum(c(if(diag) 1L else if(upper) n+1L else 2L, r))
}


prTriang <- function(x, digits = getOption("digits"),
                     maxp = getOption("max.print"),
		     justify = "none", right = TRUE)
{
    ## modeled along stats:::print.dist
    upper <- x@uplo == "U"

    m <- as(x, "matrix")
    cf <- format(m, digits = digits, justify = justify)
    if(upper)
	cf[row(cf) > col(cf)] <- "."
    else
	cf[row(cf) < col(cf)] <- "."
    print(cf, quote = FALSE, right = right, max = maxp)
    invisible(x)
}

prMatrix <- function(x, digits = getOption("digits"),
                     maxp = getOption("max.print")) {
    d <- dim(x)
    cl <- class(x)
    cat(sprintf('%d x %d Matrix of class "%s"\n', d[1], d[2], cl))
    if(prod(d) <= maxp) {
	if(is(x, "triangularMatrix"))
	    prTriang(x, digits = digits, maxp = maxp)
	else {
            print(as(x, "matrix"), digits = digits, max = maxp)
	}
    }
    else { ## d[1] > maxp / d[2] >= nr :
	m <- as(x, "matrix")
	nr <- maxp %/% d[2]
	n2 <- ceiling(nr / 2)
	print(head(m, max(1, n2)))
	cat("\n ..........\n\n")
	print(tail(m, max(1, nr - n2)))
    }
    ## DEBUG: cat("str(.):\n") ; str(x)
    invisible(x)# as print() S3 methods do
}

nonFALSE <- function(x) {
    ## typically used for lMatrices:  (TRUE,NA,FALSE) |-> (TRUE,FALSE)
    if(any(ix <- is.na(x))) x[ix] <- TRUE
    x
}

nz.NA <- function(x, na.value) {
    ## Non-Zeros of x
    ## na.value: TRUE: NA's give TRUE, they are not 0
    ##             NA: NA's are not known ==> result := NA
    ##          FALSE: NA's give FALSE, could be 0
    stopifnot(is.logical(na.value), length(na.value) == 1)
    if(is.na(na.value)) x != 0
    else  if(na.value)	isN0(x)
    else		x != 0 & !is.na(x)
}

### This assumes that e.g. the i-slot in Csparse is *not* over-allocated:
nnzSparse <- function(x, cl = class(x), cld = getClassDef(cl))
{
    ## Purpose: number of *stored* / structural non-zeros {NA's counted too}
    ## ----------------------------------------------------------------------
    ## Arguments: x sparseMatrix
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, 18 Apr 2008
    if(extends(cld, "CsparseMatrix") || extends(cld, "TsparseMatrix"))
	length(x@i)
    else if(extends(cld, "RsparseMatrix"))
	length(x@j)
    else if(extends(cld, "pMatrix"))	# is "sparse" too
	x@Dim[1]
    else stop("'x' must be sparseMatrix")
}


## Number of "structural" non-zeros --- this is  nnzmax() in Matlab
##        of effectively  non-zero values =      nnz()     "   "

## Our nnzero() is like Matlab's nnz() -- but more sophisticated because of NAs
## This is now exported!
nnzero <- function(x, na.counted = NA) {
    ## na.counted: TRUE: NA's are counted, they are not 0
    ##		     NA: NA's are not known (0 or not) ==>  result := NA
    ##		  FALSE: NA's are omitted before counting
    cl <- class(x)
    ## speedup:
    cld <- getClassDef(cl)
    if(!extends(cld, "Matrix"))
	sum(nz.NA(x, na.counted))
    else { ## Matrix
	d <- x@Dim
	if(any(d == 0)) return(0L)
	n <- d[1]
	iSym <- extends(cld, "symmetricMatrix")
	if(extends(cld, "pMatrix"))	# is "sparse" too
	    n
	else if(extends(cld, "diagonalMatrix"))
	    sum(nz.NA(diag(x), na.counted))
	else if(extends(cld, "sparseMatrix")) {
	    nn <- switch(.sp.class(cl),
                         "CsparseMatrix" = x@p[d[2]+1L],# == length(x@i) only if not over-alloc.
                         "TsparseMatrix" = length(x@i),
                         "RsparseMatrix" = x@p[n+1L])
	    if(!extends(cld, "nMatrix")) # <==> has 'x' slot : consider NAs in it:
		nn <- sum(nz.NA(if(nn < length(x@x)) x@x[seq_len(nn)] else x@x,
				na.counted))

	    if(iSym)
		nn+nn - sum(nz.NA(diag(x), na.counted))
	    else if(extends(cld, "triangularMatrix") && x@diag == "U")
		nn + n else nn
	}
	else {
	    ## dense, not diagonal: Can use 'x' slot;
	    if(iSym || extends(cld, "triangularMatrix")) {
		## now !iSym  <==> "triangularMatrix"
		upper <- (x@uplo == "U")
		if(length(x@x) < n*n) { ## packed symmetric | triangular
		    if(iSym) {
			## indices of *diagonal* entries for packed :
			iDiag <- cumsum(if(upper) 1:n else c(1L, if(n > 1)n:2))
			## symmetric packed: count off-diagonals *twice*
			2L* sum(nz.NA(x@x[-iDiag], na.counted)) +
			    sum(nz.NA(x@x[ iDiag], na.counted))
		    }
		    else ## triangular packed
			sum(nz.NA(x@x, na.counted))
		}
		else {
		    ## not packed, but may have "arbitrary"
		    ## entries in the non-relevant upper/lower triangle
		    s <- sum(nz.NA(x@x[indTri(n, upper=upper)], na.counted))
		    (if(iSym) 2L * s else s) +
			(if(!iSym && x@diag == "U")
			 n else sum(nz.NA(x@x[indDiag(n)], na.counted)))
		}
	    }
	    else { ## dense general <--> .geMatrix
		sum(nz.NA(x@x, na.counted))
	    }
	}
    }
}

## For sparseness handling, return a
## 2-column (i,j) matrix of 0-based indices of non-zero entries:

non0.i <- function(M, cM = class(M), uniqT=TRUE) {
    if(extends(cM, "TsparseMatrix")) {
	if(uniqT && is_not_uniqT(M))
	    .Call(compressed_non_0_ij, as(M,"CsparseMatrix"), TRUE)
	else cbind(M@i, M@j)
    } else if(extends(cM, "pMatrix")) {
	cbind(seq_len(nrow(M)), M@perm) - 1L
    } else { ## C* or R*
	isC <- extends(cM, "CsparseMatrix")
	.Call(compressed_non_0_ij, M, isC)
    }
}

non0ind <- function(x, classDef.x = getClassDef(class(x)),
		    uniqT = TRUE, xtendSymm = TRUE)
{
    if(is.numeric(x))
	return(if((n <- length(x))) (0:(n-1))[isN0(x)] else integer(0))
    ## else
    stopifnot(extends(classDef.x, "sparseMatrix"))

    ij <- non0.i(x, classDef.x, uniqT=uniqT)
    if(xtendSymm && extends(classDef.x, "symmetricMatrix")) { # also get "other" triangle
	notdiag <- ij[,1] != ij[,2]# but not the diagonals again
	rbind(ij, ij[notdiag, 2:1])
    }
    else if(extends(classDef.x, "triangularMatrix")) { # check for "U" diag
	if(x@diag == "U") {
	    i <- seq_len(dim(x)[1]) - 1L
	    rbind(ij, cbind(i,i))
	} else ij
    }
    else
	ij
}

if(FALSE) { ## -- now have  .Call(m_encodeInd, ...) etc :

## nr= nrow: since  i in {0,1,.., nrow-1}  these are 1L "decimal" encodings:
## Further, these map to and from the usual "Fortran-indexing" (but 0-based)
## __ care against integer overflow __
encodeInd  <- function(ij, di) {
    stopifnot(length(di) == 2)
    nr <- di[1L]
    if(prod(di) >= .Machine$integer.max) nr <- as.double(nr)
    ij[,1] + ij[,2] * nr
}
encodeInd2 <- function(i,j, di) {
    stopifnot(length(di) == 2)
    nr <- di[1L]
    if(prod(di) >= .Machine$integer.max) nr <- as.double(nr)
    i +  j * nr
}
}## no more needed

## 'code' is 0-based; as.integer(.) because encode*() may produce double
decodeInd <- function(code, nr) cbind(as.integer(code %% nr),
				      as.integer(code %/% nr))

complementInd <- function(ij, dim)
{
    ## Purpose: Compute the complement of the 2-column 0-based ij-matrix
    ##		but as 1-based indices
    n <- prod(dim)
    if(n == 0) return(integer(0))
    seq_len(n)[-(1L + .Call(m_encodeInd, ij, dim))]
}

unionInd <- function(ij1, ij2) unique(rbind(ij1, ij2))

intersectInd <- function(ij1, ij2, di) {
    ## from 2-column (i,j) matrices where i in {0,.., nrow-1},
    ## return only the *common* entries
    decodeInd(intersect(.Call(m_encodeInd, ij1, di),
			.Call(m_encodeInd, ij2, di)), nr=di[1])
}

WhichintersectInd <- function(ij1, ij2, di) {
    ## from 2-column (i,j) matrices where i \in {0,.., nrow-1},
    ## find *where*  common entries are in ij1 & ij2
    m1 <- match(.Call(m_encodeInd, ij1, di), .Call(m_encodeInd, ij2, di))
    ni <- !is.na(m1)
    list(which(ni), m1[ni])
}


### There is a test on this in ../tests/dgTMatrix.R !

uniqTsparse <- function(x, class.x = c(class(x))) {
    ## Purpose: produce a *unique* triplet representation:
    ##		by having (i,j) sorted and unique
    ## -----------------------------------------------------------
    ## The following is not quite efficient {but easy to program,
    ## and as() are based on C code  (all of them?)
    ##
    ## FIXME: Do it fast for the case where 'x' is already 'uniq'

    switch(class.x,
	   "dgTMatrix" = as(as(x, "dgCMatrix"), "dgTMatrix"),
	   "dsTMatrix" = as(as(x, "dsCMatrix"), "dsTMatrix"),
	   "dtTMatrix" = as(as(x, "dtCMatrix"), "dtTMatrix"),
	   ## do we need this for "logical" ones, there's no sum() there!
	   "lgTMatrix" = as(as(x, "lgCMatrix"), "lgTMatrix"),
	   "lsTMatrix" = as(as(x, "lsCMatrix"), "lsTMatrix"),
	   "ltTMatrix" = as(as(x, "ltCMatrix"), "ltTMatrix"),
	   ## do we need this for "logical" ones, there's no sum() there!
	   "ngTMatrix" = as(as(x, "ngCMatrix"), "ngTMatrix"),
	   "nsTMatrix" = as(as(x, "nsCMatrix"), "nsTMatrix"),
	   "ntTMatrix" = as(as(x, "ntCMatrix"), "ntTMatrix"),
	   ## otherwise:
	   stop("not yet implemented for class ", class.x))
}

## Note: maybe, using
## ----    xj <- .Call(Matrix_expand_pointers, x@p)
## would be slightly more efficient than as( <dgC> , "dgTMatrix")
## but really efficient would be to use only one .Call(.) for uniq(.) !

## Eearlier was:
## drop0 <- function(x, clx = c(class(x)), tol = 0) {
drop0 <- function(x, tol = 0, is.Csparse = NA) {
    .Call(Csparse_drop,
	  if(isTRUE(is.Csparse) || is.na(is.Csparse) && is(x, "CsparseMatrix")) x else
          as(x, "CsparseMatrix"),
	  tol)
}

uniq <- function(x) {
    if(is(x, "TsparseMatrix")) uniqTsparse(x) else
    if(is(x, "sparseMatrix")) drop0(x) else x
}

asTuniq <- function(x) {
    if(is(x, "TsparseMatrix")) uniqTsparse(x) else as(x,"TsparseMatrix")
}

## is 'x' a uniq Tsparse Matrix ?
is_not_uniqT <- function(x, di = dim(x))
    is.unsorted(x@j) || any(duplicated(.Call(m_encodeInd2, x@i, x@j, di)))

## is 'x' a TsparseMatrix with no duplicated entries (to be *added* for uniq):
is_duplicatedT <- function(x, di = dim(x))
    any(duplicated(.Call(m_encodeInd2, x@i, x@j, di)))


if(FALSE) ## try an "efficient" version
uniq_gT <- function(x)
{
    ## Purpose: produce a *unique* triplet representation:
    ##		by having (i,j) sorted and unique
    ## ------------------------------------------------------------------
    ## Arguments: a "gT" Matrix
    stopifnot(is(x, "gTMatrix"))
    if((n <- length(x@i)) == 0) return(x)
    ii <- order(x@i, x@j)
    if(any(ii != 1:n)) {
	x@i <- x@i[ii]
	x@j <- x@j[ii]
	x@x <- x@x[ii]
    }
    ij <- x@i + nrow(x) * x@j
    if(any(dup <- duplicated(ij))) {

    }
    ### We should use a .Call() based utility for this!

}

t_geMatrix <- function(x) {
    x@x <- as.vector(t(array(x@x, dim = x@Dim))) # no dimnames here
    x@Dim <- x@Dim[2:1]
    x@Dimnames <- x@Dimnames[2:1]
    ## FIXME: how to set factors?
    x
}

## t( [dl]trMatrix ) and  t( [dl]syMatrix ) :
t_trMatrix <- function(x) {
    x@x <- as.vector(t(as(x, "matrix")))
    x@Dim <- x@Dim[2:1]
    x@Dimnames <- x@Dimnames[2:1]
    x@uplo <- if (x@uplo == "U") "L" else "U"
    # and keep x@diag
    x
}

fixupDense <- function(m, from, cldm = getClassDef(class(m))) {
    if(extends(cldm, "triangularMatrix")) {
	m@uplo <- from@uplo
	m@diag <- from@diag
    } else if(extends(cldm, "symmetricMatrix")) {
	m@uplo <- from@uplo
    }
    m
}

## -> ./ldenseMatrix.R :
l2d_Matrix <- function(from, cl = MatrixClass(class(from)), cld = getClassDef(cl)) {
    ## stopifnot(is(from, "lMatrix"))
    fixupDense(new(sub("^l", "d", cl),
		   x = as.double(from@x),
		   Dim = from@Dim, Dimnames = from@Dimnames),
	       from, cld)
    ## FIXME: treat 'factors' smartly {not for triangular!}
}

## -> ./ndenseMatrix.R :
n2d_Matrix <- function(from, cl = MatrixClass(class(from)), cld = getClassDef(cl)) {
    ## stopifnot(is(from, "nMatrix"))
    fixupDense(new(sub("^n", "d", cl), x = as.double(from@x),
		   Dim = from@Dim, Dimnames = from@Dimnames),
	       from, cld)
    ## FIXME: treat 'factors' smartly {not for triangular!}
}
n2l_Matrix <- function(from, cl = MatrixClass(class(from)), cld = getClassDef(cl)) {
    fixupDense(new(sub("^n", "l", cl),
		   x = from@x, Dim = from@Dim, Dimnames = from@Dimnames),
	       from, cld)
    ## FIXME: treat 'factors' smartly {not for triangular!}
}
## -> ./ddenseMatrix.R :
d2l_Matrix <- function(from, cl = MatrixClass(class(from)), cld = getClassDef(cl)) {
    fixupDense(new(sub("^d", "l", cl), x = as.logical(from@x),
                   Dim = from@Dim, Dimnames = from@Dimnames),
	       from, cld)
    ## FIXME: treat 'factors' smartly {not for triangular!}
}

n2l_spMatrix <- function(from) {
    ## stopifnot(is(from, "nMatrix"))
    new(sub("^n", "l", MatrixClass(class(from))),
        ##x = as.double(from@x),
        Dim = from@Dim, Dimnames = from@Dimnames)
}

tT2gT <- function(x, cl = class(x), toClass, cld = getClassDef(cl)) {
    ## coerce *tTMatrix to *gTMatrix {triangular -> general}
    d <- x@Dim
    if(uDiag <- x@diag == "U")	     # unit diagonal, need to add '1's
        uDiag <- (n <- d[1]) > 0
    if(missing(toClass)) {
        do.n <- extends(cld, "nMatrix")
        toKind <- if(do.n) "n" else substr(MatrixClass(cl), 1,1) # "d" | "l"|"i"|"z"
        toClass <- paste(toKind, "gTMatrix", sep='')
    } else {
        do.n <- extends(toClass, "nMatrix")
        toKind <- if(do.n) "n" else substr(toClass, 1,1)
    }

    if(do.n) ## no 'x' slot
	new(toClass, # == "ngTMatrix"
            Dim = d, Dimnames = x@Dimnames,
	    i = c(x@i, if(uDiag) 0:(n-1)),
	    j = c(x@j, if(uDiag) 0:(n-1)))
    else
	new(toClass, Dim = d, Dimnames = x@Dimnames,
	    i = c(x@i, if(uDiag) 0:(n-1)),
	    j = c(x@j, if(uDiag) 0:(n-1)),
	    x = c(x@x, if(uDiag) rep.int(if(toKind == "l") TRUE else 1, n)))
}
## __TODO__
## Hack for the above, possibly considerably faster:
## Just *modify* the 'x' object , using attr(x, "class') <- toClass


## Fast very special one
## .gT2tC <- function(x, uplo, diag) .Call(Tsparse_to_tCsparse, x, uplo, diag)

gT2tT <- function(x, uplo, diag, cl = class(x), toClass,
		  do.n = extends(toClass, "nMatrix"))
{
    ## coerce *gTMatrix to *tTMatrix {general -> triangular}
    i <- x@i
    j <- x@j
    sel <-
	if(uplo == "U") {
	    if(diag == "U") i < j else i <= j
	} else {
	    if(diag == "U") i > j else i >= j
	}
    i <- i[sel]
    j <- j[sel]
    if(do.n) ## no 'x' slot
	new("ntTMatrix", i = i, j = j, uplo = uplo, diag = diag,
	    Dim = x@Dim, Dimnames = x@Dimnames)
    else
	new(toClass, i = i, j = j, uplo = uplo, diag = diag,
	    x = x@x[sel], Dim = x@Dim, Dimnames = x@Dimnames)
}

check.gT2tT <- function(from, cl = MatrixClass(class(from)),
			toClass, do.n = extends(toClass, "nMatrix")) {
    if(isTr <- isTriangular(from)) {
        force(cl)
	gT2tT(from, uplo = .if.NULL(attr(isTr, "kind"), "U"),
	      diag = "N", ## improve: also test for unit diagonal
	      cl = cl, toClass = toClass, do.n = do.n)
    } else stop("not a triangular matrix")
}

if(FALSE)# unused
l2d_meth <- function(x) {
    cl <- MatrixClass(class(x))
    as(callGeneric(as(x, sub("^l", "d", cl))), cl)
}

## return "d" or "l" or "n" or "z"
.M.kind <- function(x, clx = class(x)) {
    ## 'clx': class() *or* class definition of x
    if(is.matrix(x) || is.atomic(x)) { ## 'old style' matrix or vector
	if     (is.numeric(x)) "d"
	else if(is.logical(x)) "l" ## FIXME ? "n" if no NA ??
	else if(is.complex(x)) "z"
	else stop("not yet implemented for matrix w/ typeof ", typeof(x))
    }
    else .M.kindC(clx)
}

## the same as .M.kind, but also knows "i"
.V.kind <- function(x, clx = class(x)) {
    ## 'clx': class() *or* class definition of x
    if(is.matrix(x) || is.atomic(x)) { ## 'old style' matrix or vector
	if     (is.integer(x)) "i"
	else if(is.numeric(x)) "d"
	else if(is.logical(x)) "l" ## FIXME ? "n" if no NA ??
	else if(is.complex(x)) "z"
	else stop("not yet implemented for matrix w/ typeof ", typeof(x))
    }
    else .M.kindC(clx)
}

.M.kindC <- function(clx) { ## 'clx': class() *or* classdefinition
    if(is.character(clx))		# < speedup: get it once
        clx <- getClassDef(clx)
    if(extends(clx, "sparseVector")) ## shortcut
	substr(as.character(clx@className), 1,1)
    else if(extends(clx, "dMatrix")) "d"
    else if(extends(clx, "nMatrix")) "n"
    else if(extends(clx, "lMatrix")) "l"
    else if(extends(clx, "pMatrix")) "n" # permutation -> pattern
    else if(extends(clx, "zMatrix")) "z"
    else if(extends(clx, "iMatrix")) "i"
    else stop(" not yet be implemented for ", clx@className)
}


## typically used as .type.kind[.M.kind(x)]:
.type.kind <- c("d" = "double",
		"i" = "integer",
		"l" = "logical",
		"n" = "logical",
		"z" = "complex")

.M.shape <- function(x, clx = class(x)) {
    ## 'clx': class() *or* class definition of x
    if(is.matrix(x)) { ## 'old style matrix'
	if     (isDiagonal  (x)) "d"
	else if(isTriangular(x)) "t"
	else if(isSymmetric (x)) "s"
	else "g" # general
    }
    else {
	if(is.character(clx)) # < speedup: get it once
	    clx <- getClassDef(clx)
	if(extends(clx, "diagonalMatrix"))  "d"
	else if(extends(clx, "triangularMatrix"))"t"
	else if(extends(clx, "symmetricMatrix")) "s"
	else "g"
    }
}

## a faster simpler version [for sparse matrices, i.e., never diagonal]
.M.shapeC <- function(x, clx = class(x)) {
    if(is.character(clx)) # < speedup: get it once
	clx <- getClassDef(clx)
    if	   (extends(clx, "triangularMatrix")) "t"
    else if(extends(clx, "symmetricMatrix"))  "s" else "g"
}


class2 <- function(cl, kind = "l", do.sub = TRUE) {
    ## Find "corresponding" class; since pos.def. matrices have no pendant:
    cl <- MatrixClass(cl)
    if(cl %in% c("dpoMatrix","corMatrix"))
	paste(kind, "syMatrix", sep='')
    else if(cl == "dppMatrix")
	paste(kind, "spMatrix", sep='')
    else if(do.sub) sub("^[a-z]", kind, cl)
    else cl
}

## see also as_smartClass() below
geClass <- function(x) {
    if     (is(x, "dMatrix")) "dgeMatrix"
    else if(is(x, "lMatrix")) "lgeMatrix"
    else if(is(x, "nMatrix") || is(x, "pMatrix")) "ngeMatrix"
    else if(is(x, "zMatrix")) "zgeMatrix"
    else stop("general Matrix class not yet implemented for ",
	      class(x))
}

.dense.prefixes <- c("d" = "tr", ## map diagonal to triangular
                     "t" = "tr",
                     "s" = "sy",
                     "g" = "ge")

.sparse.prefixes <- c("d" = "t", ## map diagonal to triangular
                      "t" = "t",
                      "s" = "s",
                      "g" = "g")

## Used, e.g. after subsetting: Try to use specific class -- if feasible :
as_dense <- function(x, cld = if(isS4(x)) getClassDef(class(x))) {
    as(x, paste(.M.kind(x, cld), .dense.prefixes[.M.shape(x, cld)], "Matrix", sep=''))
}

## This is "general" but slower than the next definition
if(FALSE)
.sp.class <- function(x) { ## find and return the "sparseness class"
    if(!is.character(x)) x <- class(x)
    for(cl in paste(c("C","T","R"), "sparseMatrix", sep=''))
	if(extends(x, cl))
	    return(cl)
    ## else (should rarely happen)
    NA_character_
}

.sp.class <- function(x) { ## find and return the "sparseness class"
    x <- if(!is.character(x)) MatrixClass(class(x)) else MatrixClass(x)
    if(any((ch <- substr(x,3,3)) == c("C","T","R")))
        return(paste(ch, "sparseMatrix", sep=''))
    ## else
    NA_character_
}


### Goal: Eventually get rid of these --- want to foster coercions
### ----  *to* virtual classes whenever possible, e.g. as(*, "CsparseMatrix")
## 2007-12: better goal: use them only for "matrix" [maybe speed them up later]

## Here, getting the class definition and passing it, should be faster
as_Csparse <- function(x, cld = if(isS4(x)) getClassDef(class(x))) {
    as(x, paste(.M.kind(x, cld),
                .sparse.prefixes[.M.shape(x, cld)], "CMatrix", sep=''))
}

if(FALSE) # replaced by .Call(dense_to_Csparse, *) which is perfect for "matrix"
as_Csparse2 <- function(x, cld = if(isS4(x)) getClassDef(class(x))) {
    ## Csparse + U2N when needed
    sh <- .M.shape(x, cld)
    x <- as(x, paste(.M.kind(x, cld), .sparse.prefixes[sh], "CMatrix", sep=''))
    if(sh == "t") .Call(Csparse_diagU2N, x) else x
}

## *do* use this where applicable
as_Csp2 <- function(x) {
    ## Csparse + U2N when needed
    x <- as(x, "CsparseMatrix")
    if(is(x, "triangularMatrix")) .Call(Csparse_diagU2N, x) else x
}


## 'cl'   : class() *or* class definition of from
as_gCsimpl2 <- function(from, cl = class(from))
    as(from, paste(.M.kind(from, cl), "gCMatrix", sep=''))
## to be used directly in setAs(.) needs one-argument-only  (from) :
as_gCsimpl <- function(from) as(from, paste(.M.kind(from), "gCMatrix", sep=''))

## slightly smarter:
as_Sp <- function(from, shape, cl = class(from)) {
    if(is.character(cl)) cl <- getClassDef(cl)
    as(from, paste(.M.kind(from, cl),
		   shape,
		   if(extends(cl, "TsparseMatrix")) "TMatrix" else "CMatrix",
		   sep=''))
}
## These are used in ./sparseMatrix.R:
as_gSparse <- function(from) as_Sp(from, "g", getClassDef(class(from)))
as_sSparse <- function(from) as_Sp(from, "s", getClassDef(class(from)))
as_tSparse <- function(from) as_Sp(from, "t", getClassDef(class(from)))

as_geSimpl2 <- function(from, cl = class(from))
    as(from, paste(.M.kind(from, cl), "geMatrix", sep=''))
## to be used directly in setAs(.) needs one-argument-only  (from) :
as_geSimpl <- function(from) as(from, paste(.M.kind(from), "geMatrix", sep=''))

## Smarter, (but sometimes too smart!) compared to geClass() above:
as_smartClass <- function(x, cl, cld = getClassDef(cl)) {
    if(missing(cl)) return(as_geSimpl(x))
    ## else
    if(extends(cld, "diagonalMatrix")  && isDiagonal(x))
        ## diagonal* result:
	as(x, cl)
    else if(extends(cld, "symmetricMatrix") &&  isSymmetric(x)) {
        ## symmetric* result:
        kind <- .M.kind(x, cld)
	as(x, class2(cl, kind, do.sub= kind != "d"))
    } else if(extends(cld, "triangularMatrix") && isTriangular(x))
	as(x, cl)
    else ## revert to
	as_geSimpl2(x, cld)
}

as_CspClass <- function(x, cl) {
    ## NOTE: diagonal is *not* sparse:
    cld <- getClassDef(cl)
    ##(extends(cld, "diagonalMatrix") && isDiagonal(x)) ||
    if (extends(cld, "symmetricMatrix") && isSymmetric(x))
        forceSymmetric(as(x,"CsparseMatrix"))
    else if (extends(cld, "triangularMatrix") && (iT <- isTriangular(x)))
	as(x, cl)
    else if(is(x, "CsparseMatrix")) x
    else as(x, paste(.M.kind(x, cld), "gCMatrix", sep=''))
}

asTri <- function(from, newclass) {
    ## TODO: also check for unit-diagonal: 'diag = "U"'
    isTri <- isTriangular(from)
    if(isTri)
	new(newclass, x = from@x, Dim = from@Dim,
	    Dimnames = from@Dimnames, uplo = attr(isTri, "kind"))
    else stop("not a triangular matrix")
}



try_as <- function(x, classes, tryAnyway = FALSE) {
    if(!tryAnyway && !is(x, "Matrix"))
	return(x)
    ## else
    ok <- canCoerce(x, classes[1])
    while(!ok && length(classes <- classes[-1])) {
	ok <- canCoerce(x, classes[1])
    }
    if(ok) as(x, classes[1]) else x
}


## For *dense* matrices
isTriMat <- function(object, upper = NA) {
    ## pretest: is it square?
    d <- dim(object)
    if(d[1] != d[2]) return(FALSE)
    TRUE.U <- structure(TRUE, kind = "U")
    if(d[1] == 0) return(TRUE.U)
    ## else slower test
    TRUE.L <- structure(TRUE, kind = "L")
    if(!is.matrix(object))
	object <- as(object,"matrix")
    if(is.na(upper)) {
	if(all0(object[lower.tri(object)]))
	    TRUE.U
	else if(all0(object[upper.tri(object)]))
	    TRUE.L
	else FALSE
    } else if(upper)
	if(all0(object[lower.tri(object)])) TRUE.U else FALSE
    else ## upper is FALSE
	if(all0(object[upper.tri(object)])) TRUE.L else FALSE
}

## For Tsparse matrices:
isTriT <- function(object, upper = NA) {
    ## pretest: is it square?
    d <- dim(object)
    if(d[1] != d[2]) return(FALSE)
    ## else
    TRUE.U <- structure(TRUE, kind = "U")
    if(d[1] == 0) return(TRUE.U)
    TRUE.L <- structure(TRUE, kind = "L")
    if(is.na(upper)) {
	if(all(object@i <= object@j))
	    TRUE.U
	else if(all(object@i >= object@j))
	    TRUE.L
	else FALSE
    } else if(upper) {
	if(all(object@i <= object@j)) TRUE.U else FALSE
    } else { ## 'lower'
	if(all(object@i >= object@j)) TRUE.L else FALSE
    }
}

## For Csparse matrices
isTriC <- function(object, upper = NA) {
    ## pretest: is it square?
    d <- dim(object)
    if(d[1] != d[2]) return(FALSE)
    ## else
    TRUE.U <- structure(TRUE, kind = "U")
    if(d[1] == 0) return(TRUE.U)
    TRUE.L <- structure(TRUE, kind = "L")
    ni <- 1:d[2]
    ## the row indices split according to column:
    ilist <- split(object@i, factor(rep.int(ni, diff(object@p)), levels= ni))
    lil <- unlist(lapply(ilist, length), use.names = FALSE)
    if(any(lil == 0)) {
	pos <- lil > 0
	if(!any(pos)) ## matrix of all 0's
	    return(TRUE.U)
	ilist <- ilist[pos]
	ni <- ni[pos]
    }
    ni0 <- ni - 1L # '0-based ni'
    if(is.na(upper)) {
	if(all(sapply(ilist, max, USE.NAMES = FALSE) <= ni0))
	    TRUE.U
	else if(all(sapply(ilist, min, USE.NAMES = FALSE) >= ni0))
	    TRUE.L
	else FALSE
    } else if(upper) {
	if(all(sapply(ilist, max, USE.NAMES = FALSE) <= ni0))
	    TRUE.U else FALSE
    } else { ## 'lower'
	if(all(sapply(ilist, min, USE.NAMES = FALSE) >= ni0))
	    TRUE.L else FALSE
    }
}

.is.diagonal <- function(object) {
    ## "matrix" or "denseMatrix" (but not "diagonalMatrix")
    d <- dim(object)
    if(d[1] != (n <- d[2])) FALSE
    else if(is.matrix(object))
        ## requires that "vector-indexing" works for 'object' :
        all0(object[rep(c(FALSE, rep.int(TRUE,n)), length = n^2)])
    else ## "denseMatrix" -- packed or unpacked
        if(is(object, "generalMatrix")) # "dge", "lge", ...
            all0(object@x[rep(c(FALSE, rep.int(TRUE,n)), length = n^2)])
        else { ## "dense" but not {diag, general}, i.e. triangular or symmetric:
            ## -> has 'uplo'  differentiate between packed and unpacked

### .......... FIXME ...............

            packed <- isPacked(object)
            if(object@uplo == "U") {
            } else { ## uplo == "L"
            }

### very cheap workaround
	    all0(as.matrix(object)[rep(c(FALSE, rep.int(TRUE,n)), length = n^2)])
        }
}


## Purpose: Transform a *unit diagonal* sparse triangular matrix
##	into one with explicit diagonal entries '1'

.diagU2N <- function(x, cl, checkDense = FALSE)
{
    ## fast "no-test" version --- we *KNOW* 'x' is 'triangularMatrix'
    if(extends(cl, "CsparseMatrix"))
	.Call(Csparse_diagU2N, x)
    else if(extends(cl, "TsparseMatrix"))
	.Call(Tsparse_diagU2N, x)
    else {
	kind <- .M.kind(x, cl)
        if(checkDense && extends(cl,"denseMatrix")) {
	    ## For denseMatrix, 'diag = "U"'
	    ## means the 'x' slot can have wrong values which are
	    ## documented to never be accessed
	    if(isPacked(x)) { ## unpack
		stop("not yet implemented for packed class ", class(x))
		## FIXME
	    }
	    ## okay: now have  'x' slot of length n x n
	    n <- dim(x)[1]
	    if(n > 0)
		x@x[1L+ (0:(n-1L))*(n+1L)] <-
		    if(kind == "d") 1. else TRUE # even for "n..Matrix"
	    x@diag <- "N"
	    x
        }
        else { ## not dense, not [CT]sparseMatrix  ==>  Rsparse*
	    .Call(Tsparse_diagU2N,
		  as(as(x, paste(kind, "Matrix", sep='')), "TsparseMatrix"))
	    ## leave it as T* - the caller can always coerce to C* if needed
        }
    }
}

diagU2N <- function(x, cl = getClassDef(class(x)))
{
    if(extends(cl, "triangularMatrix") && x@diag == "U")
	.diagU2N(x, cl)
    else x
}

diagN2U <- function(x, cl = getClassDef(class(x)))
{
    if(extends(cl, "triangularMatrix") && x@diag == "N")
	.Call(Csparse_diagN2U, as(x, "CsparseMatrix")) else x
}

.as.dgC.0.factors <- function(x) {
    if(!is(x, "dgCMatrix"))
	as(x, "dgCMatrix") # will not have 'factors'
    else ## dgCMatrix
	if(!length(x@factors)) x else { x@factors <- list() ; x }
}


### Fast, much simplified version of tapply()
tapply1 <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
    sapply(unname(split(X, INDEX)), FUN, ...,
	   simplify = simplify, USE.NAMES = FALSE)
}

## tapply.x <- function (X, n, INDEX, FUN = NULL, ..., simplify = TRUE) {
##     tapply1(X, factor(INDEX, 0:(n-1)), FUN = FUN, ..., simplify = simplify)
## }

### MM: Unfortunately, these are still pretty slow for large sparse ...

sparsapply <- function(x, MARGIN, FUN, sparseResult = TRUE, ...)
{
    ## Purpose: "Sparse Apply": better utility than tapply1() for colSums() etc :
    ##    NOTE: Only applicable sum()-like where the "zeros do not count"
    ## ----------------------------------------------------------------------
    ## Arguments: x: sparseMatrix;  others as in *apply()
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 16 May 2007
    stopifnot(MARGIN %in% 1:2)
    xi <- if(MARGIN == 1) x@i else x@j
    ui <- unique(xi)
    n <- x@Dim[MARGIN]
    ## FIXME: Here we assume 'FUN' to return  'numeric' !
    r <- if(sparseResult) new("dsparseVector", length = n) else numeric(n)
    r[ui + 1L] <- sapply(ui, function(i) FUN(x@x[xi == i], ...))
    r
}

sp.colMeans <- function(x, na.rm = FALSE, dims = 1, sparseResult = FALSE)
{
    nr <- nrow(x)
    if(na.rm) ## use less than nrow(.) in case of NAs
	nr <- nr - sparsapply(x, 2, function(u) sum(is.na(u)),
			      sparseResult=sparseResult)
    sparsapply(x, 2, sum, sparseResult=sparseResult, na.rm=na.rm) / nr
}

sp.rowMeans <- function(x, na.rm = FALSE, dims = 1, sparseResult = FALSE)
{
    nc <- ncol(x)
    if(na.rm) ## use less than ncol(.) in case of NAs
	nc <- nc - sparsapply(x, 1, function(u) sum(is.na(u)),
			      sparseResult=sparseResult)
    sparsapply(x, 1, sum, sparseResult=sparseResult, na.rm=na.rm) / nc
}

all0Matrix <- function(n,m) {
    ## an  all-0 matrix	 -- chose what Matrix() also gives -- "most efficiently"
    n <- as.integer(n)
    m <- as.integer(m)
    new(if(n == m) "dsCMatrix" else "dgCMatrix",
	Dim = c(n,m),
	p = rep.int(0L, m+1L))
}

setZero <- function(x) {
    ## all-0 matrix  from x  which must inherit from 'Matrix'
    d <- x@Dim
    new(if(d[1] == d[2]) "dsCMatrix" else "dgCMatrix",
	Dim = d, Dimnames = x@Dimnames, p = rep.int(0L, d[2]+1L))
}

.M.vectorSub <- function(x,i) {
    if(any(as.logical(i)) || prod(dim(x)) == 0)
	## FIXME: for *large sparse*, use sparseVector !
	as.vector(x)[i]
    else ## save memory (for large sparse M):
	as.vector(x[1,1])[FALSE]
}
