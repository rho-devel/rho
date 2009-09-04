####  Utilities  for  Sparse Model Matrices

## The "first" version {no longer used}:
fac2sparse <- function(from, to = c("d","i","l","n","z"), drop.unused.levels = TRUE)
{
    ## factor(-like) --> sparseMatrix {also works for integer, character}
    fact <- if (drop.unused.levels) factor(from) else as.factor(from)
    levs <- levels(fact)
    n <- length(fact)
    to <- match.arg(to)
    ## MM: using new() and then assigning slots has efficiency "advantage"
    ##     of *not* validity checking
    res <- new(paste0(to, "gCMatrix"))
    res@i <- as.integer(fact) - 1L # 0-based
    res@p <- 0:n
    res@Dim <- c(length(levs), n)
    res@Dimnames <- list(levs, NULL)
    if(to != "n")
	res@x <- rep.int(switch(to,
				"d" = 1., "i" = 1L, "l" = TRUE, "z" = 1+0i),
			 n)
    res
}

## This version can deal with NA's [maybe slightly less efficient (how much?)] :
fac2sparse <- function(from, to = c("d","i","l","n","z"),
                       drop.unused.levels = TRUE)
{
    ## factor(-like) --> sparseMatrix {also works for integer, character}
    fact <- if (drop.unused.levels) factor(from) else as.factor(from)
    levs <- levels(fact)
    n <- length(fact)
    to <- match.arg(to)
    i <- as.integer(fact) - 1L                  # 0-based indices
    df <- data.frame(i = i, j = seq_len(n) - 1L)[!is.na(i),]
    if(to != "n")
	df$x <- rep.int(switch(to,
			       "d" = 1., "i" = 1L, "l" = TRUE, "z" = 1+0i),
			nrow(df))
    as(do.call("new", c(list(Class = paste0(to, "gTMatrix"),
			     Dim = c(length(levs), n),
			     Dimnames = list(levs, names(fact))),
			df)),
       "CsparseMatrix")
}

setAs("factor", "sparseMatrix", function(from) fac2sparse(from, to = "d"))

##' fac2Sparse() := fac2sparse w/ contrasts
##'
##' @param from factor of which we want the "contrasted" (indicator)
##'   design matrix
##' @param to character string specifying the response type
##' @param drop.unused.level logical indicating if non-present factor
##'   levels should be dropped, via  factor(from)
##' @param factorPatt12  logical vector fp[] of length 2
##'   fp[1] : give contrasted t(X);  fp[2] : give "dummy" t(X)
##' @param contrasts.arg character string or NULL or (coercable to)
##'		sparseMatrix, specifying the contrast
##'
##' @return a list of length two, each with the corresponding t(model matrix),
##'	when the corresponding factorPatt12 is true.
fac2Sparse <- function(from, to = c("d","i","l","n","z"),
		       drop.unused.levels = TRUE,
		       factorPatt12, contrasts.arg = NULL)
{
    stopifnot(is.logical(factorPatt12), length(factorPatt12) == 2)
    if(any(factorPatt12))
	m <- fac2sparse(from, to=to,
			drop.unused.levels=drop.unused.levels)
    ##
    ## code '2' : keep dummy, i.e. no contrasts :
    ans <- list(NULL, if(factorPatt12[2]) m)
    ##
    if(factorPatt12[1]) {
	## *do* use contrasts.arg
	if(is.null(contrasts.arg))
	    contrasts.arg <- getOption("contrasts")[if(is.ordered(from))
						    "ordered" else "unordered"]
	ans[[1]] <-
	    crossprod(if(is.character(contrasts.arg)) {
		stopifnot(is.function(FUN <- get(contrasts.arg)))
		## calling  contr.*() with correct level names directly :
		FUN(rownames(m), sparse = TRUE)
	    } else as(contrasts.arg, "sparseMatrix"), m)
    }
    ans
}

if(getRversion() < "2.10.0" || R.version$`svn rev` < 48913) {
### Define contr.sum() etc  with 'sparse' argument :

.sparse.array <- function(x, dim, dimnames) {
    dim <- as.integer(dim)
    new("dgCMatrix", Dim = dim, p = rep.int(0L, dim[2]+1L), Dimnames = dimnames)
}

contr.helmert <-
    function (n, contrasts=TRUE, sparse=FALSE)
{
    if (length(n) <= 1) {
	if(is.numeric(n) && length(n) == 1 && n > 1) levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    llev <- length(levels <- as.character(levels))
    if(sparse) array <- .sparse.array
    if (contrasts) {
	cont <- array(-1, c(llev, llev-1L), list(levels, NULL))
	cont[col(cont) <= row(cont) - 2] <- 0
	cont[col(cont) == row(cont) - 1] <- 1L:(llev-1)
    } else {
	cont <- array(0, c(llev, llev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.treatment <-
    function(n, base = 1, contrasts = TRUE, sparse = FALSE)
{
    if(is.numeric(n) && length(n) == 1) {
	if(n > 1) levels <- as.character(seq_len(n))
	else stop("not enough degrees of freedom to define contrasts")
    } else {
        levels <- as.character(n)
	n <- length(n)
    }

    if(sparse) array <- .sparse.array
    contr <- array(0, c(n, n), list(levels, levels))
    diag(contr) <- 1
    if(contrasts) {
	if(n < 2)
	    stop(gettextf("contrasts not defined for %d degrees of freedom",
                          n - 1), domain = NA)
	if (base < 1 | base > n)
	    stop("baseline group number out of range")
	contr <- contr[, -base, drop = FALSE]
    }
    contr
}

contr.sum <-
    function (n, contrasts=TRUE, sparse=FALSE)
{
    if (length(n) <= 1) {
	if (is.numeric(n) && length(n) == 1 && n > 1)
	    levels <- seq_len(n)
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    llev <- length(levels <- as.character(levels))
    if(sparse) array <- .sparse.array
    if (contrasts) {
	cont <- array(0, c(llev, llev - 1L), list(levels, NULL))
	cont[col(cont) == row(cont)] <- 1
	cont[llev, ] <- -1
    } else {
	cont <- array(0, c(llev, llev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.SAS <- function(n, contrasts = TRUE, sparse=FALSE)
{
    contr.treatment(n,
                    base = if (is.numeric(n) && length(n) == 1) n else length(n),
                    contrasts, sparse=sparse)
}

contr.poly <- function (n, scores = 1L:n, contrasts = TRUE, sparse = FALSE) {
    ## this is non-sense anyway

    ## need for 'scores' default :
    if (is.numeric(n) && length(n) == 1)
	levs <- seq_len(n)
    else {
	levs <- n
	n <- length(levs)
    }
    m <- stats::contr.poly(levs, scores=scores, contrasts=contrasts)
    if(sparse) as(m, "sparseMatrix") else m
}

`contrasts<-` <- function(x, how.many, value)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if(!is.factor(x))
	stop("contrasts apply only to factors")
    if(nlevels(x) < 2L)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if(is.function(value)) value <- value(nlevels(x))
    if((is.n <- is.numeric(value)) || is(value, "Matrix")) {
	## also work for "sparseMatrix"
	if(is.n) value <- as.matrix(value)
	nlevs <- nlevels(x)
	if(nrow(value) != nlevs)
	    stop("wrong number of contrast matrix rows")
	n1 <- if(missing(how.many)) nlevs - 1L else how.many
	nc <- ncol(value)
	rownames(value) <- levels(x)
	if(nc < n1) {
	    if(!is.n) value <- as.matrix(value) ## for now ..
	    cm <- qr(cbind(1,value))
	    if(cm$rank != nc+1) stop("singular contrast matrix")
	    cm <- qr.qy(cm, diag(nlevs))[, 2L:nlevs]
	    cm[,1L:nc] <- value
	    dimnames(cm) <- list(levels(x),NULL)
	    if(!is.null(nmcol <- dimnames(value)[[2L]]))
		dimnames(cm)[[2L]] <- c(nmcol, rep.int("", n1-nc))
	} else cm <- value[, 1L:n1, drop=FALSE]
    }
    else if(is.character(value)) cm <- value
    else if(is.null(value)) cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
}

} else { ## make codoc() happy [needed as long as we *document* these (support R <= 2.9.x) ]
contr.helmert <- function (n, contrasts=TRUE, sparse=FALSE)
 stats::contr.helmert(n, contrasts=contrasts, sparse=sparse)

contr.treatment <- function(n, base = 1, contrasts = TRUE, sparse = FALSE)
 stats::contr.treatment(n, base=base, contrasts=contrasts, sparse=sparse)

contr.sum <- function (n, contrasts=TRUE, sparse=FALSE)
 stats::contr.sum(n, contrasts=contrasts, sparse=sparse)

contr.SAS <- function(n, contrasts = TRUE, sparse=FALSE)
 stats::contr.SAS(n, contrasts=contrasts, sparse=sparse)

contr.poly <- function (n, scores = 1L:n, contrasts = TRUE, sparse = FALSE)
 stats::contr.poly(n, scores=scores, contrasts=contrasts, sparse=sparse)

`contrasts<-` <- function(x, how.many, value)
 stats::`contrasts<-`(x, how.many=how.many, value=value)

} ## end if {define  contr.*()  for pre-2.10.0 R)


## Goal: an  model.sparseMatrix()
##      model.matrix(object, data = environment(object),
##                   contrasts.arg = NULL, xlev = NULL, ...)
## "FIXME": Rather should have
##    model.matrix(.......,  sparse = TRUE) ...
##
##  Cut'n'paste from model.matrix() ... just replacing small part at end:
sparse.model.matrix <- function(object, data = environment(object),
                                contrasts.arg = NULL, xlev = NULL, ...)
{
    t <- if(missing(data)) terms(object) else terms(object, data=data)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
	reorder <- match(sapply(attr(t,"variables"),deparse,
                                width.cutoff=500)[-1L],
                         names(data))
	if (any(is.na(reorder)))
	    stop("model frame and formula mismatch in model.matrix()")
	if(!identical(reorder, seq_len(ncol(data))))
	    data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) {      # otherwise no rhs terms, so skip all this
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        ## turn any character columns into factors
        for(i in namD)
            if(is.character(data[[i]])) {
                data[[i]] <- factor(data[[i]])
                warning(gettextf("variable '%s' converted to a factor", i),
                        domain = NA)
            }
        isF <- sapply(data, function(x) is.factor(x) || is.logical(x) )
        isF[int] <- FALSE
        isOF <- sapply(data, is.ordered)
        for(nn in namD[isF])            # drop response
            if(is.null(attr(data[[nn]], "contrasts")))
                contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        ## it might be safer to have numerical contrasts:
        ##	  get(contr.funs[1 + isOF[nn]])(nlevels(data[[nn]]))
        if (!is.null(contrasts.arg) && is.list(contrasts.arg)) {
            if (is.null(namC <- names(contrasts.arg)))
                stop("invalid 'contrasts.arg' argument")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD)))
                    warning(gettextf("variable '%s' is absent, its contrast will be ignored", nn),
                            domain = NA)
                else {
                    ca <- contrasts.arg[[nn]]
## FIXME: work for *sparse* ca
                    if(is.matrix(ca)) contrasts(data[[ni]], ncol(ca)) <- ca
                    else contrasts(data[[ni]]) <- contrasts.arg[[nn]]
                }
            }
        }
    } else {               # internal model.matrix needs some variable
        isF <-  FALSE
        data <- list(x=rep(0, nrow(data)))
    }
    ## <Sparse> src/library/stats/R/models.R has
    ##    ans <- .Internal(model.matrix(t, data))
    ans <- model.spmatrix(t, data)
    ##     ==============
    ## </Sparse>

    cons <- if(any(isF))
	lapply(data[isF], function(x) attr(x,  "contrasts"))
    else NULL
    attr(ans, "contrasts") <- cons
    ans
}


sparse2int <- function(X, Y, do.names = TRUE)
{
    ## Produce the t(Z); Z = "design matrix" of (X : Y), where
    ##             --- t(Z) : aka rowwise -version : "r"

    if(do.names) {
        dnx <- dimnames(X)
        dny <- dimnames(Y)
    }
    dimnames(Y) <- dimnames(X) <- list(NULL,NULL)
    nx <- nrow(X)
    ny <- nrow(Y)

    ## X, Y either are numeric matrices {maybe 1-column}
    ##      or "as(<factor>, sparseM)"

    r <-
	if((nX <- is.numeric(X)) | (nY <- is.numeric(Y))) {
	    if(nX) {
		if (nY || nx > 1) { # both numeric, or X >=2 "columns"
                    (if(ny == 1) X else X[rep.int(seq_len(nx),  ny)   , ]) *
                    (if(nx == 1) Y else Y[rep    (seq_len(ny),each=nx), ])
		}
                else { ## numeric X (1 "column"),  sparseMatrix Y
		    r <- Y
                    dp <- Y@p[-1] - Y@p[-(Y@Dim[2]+1L)]
                    stopifnot(all(dp %in% 0:1)) # just for now - drop! - FIXME
                    ## if(nx == 1)
                    ## FIXME: similar trick would be applicable for nx > 2
                    r@x <- X[dp == 1L] * Y@x
		    r
		}
	    }
	    else { ## sparseMatrix X, numeric Y
                if(ny == 1) {
                    ## FIXME: similar trick would be applicable for ny > 2
                    r <- X
                    dp <- X@p[-1] - X@p[-(X@Dim[2]+1L)]
                    stopifnot(all(dp %in% 0:1)) # just for now - drop! - FIXME
                    r@x <- Y[dp == 1L] * X@x
                    r
                }
                else { ## ny > 1 -- *larger* matrix
                    X[rep.int(seq_len(nx),  ny)   , ] *
                    (if(nx == 1) Y else Y[rep(seq_len(ny),each=nx), ])
                }
	    }
	}
	else { ## X & Y are both sparseMatrix
            (if(ny == 1) X else X[rep.int(seq_len(nx), ny)     , ]) *
            (if(nx == 1) Y else Y[rep    (seq_len(ny),each=nx) , ])
	}

    if(do.names) {
        ## FIXME: This names business needs a good solution..
        ##        but maybe "up in the caller"
        if(!is.null(dim(r)) &&
           !is.null(nX <- dnx[[1]]) &&
           !is.null(nY <- dny[[1]]))
            rownames(r) <- outer(nX, nY, paste, sep = ":")
    }
    r
}

##' @param rList: list(.) of (transposed) single-factor model matrices,
##'	belonging to, say, factors  a, b, c,...
##' @param do.names

##' @return the model matrix corresponding to a:b:...
sparseInt.r <- function(rList, do.names = TRUE) {
    m <- length(rList)
    if(m == 1) {
	rList[[1]]
    } else {
	## recurse
	sparse2int(sparseInt.r(rList[-m], do.names=do.names),
		   rList[[m]], do.names=do.names)
    }
}


## not used currently
is.model.frame <- function(x)
{
  ## Purpose: check if x is a "valid" model.frame
  ## ------------------------------------------------------------
  ## Author: Martin Maechler, Date: 3 Jul 2009
    is.data.frame(x) &&
    !is.null(tms <- attr(x, "terms")) &&
    inherits(tms, "terms") && ## is.terms() would be better
    inherits(tms, "formula") &&
    is.matrix(attr(tms, "factors")) &&
    is.language(vv <- attr(tms, "variables")) &&
    vv[[1]] == as.symbol("list") &&
    all((vars <- sapply(as.list(vv[-1]), as.character)) %in% colnames(x))
    ## and we could go on testing vars
}


##' Create a sparse model matrix from a model frame.
##'          Assumption:  at least one component is a factor or sparse  << ? FIXME ?
##' -- This version uses  'rBind' and returns  X' i.e. t(X) :
##' @param trms a "terms" object
##' @param mf a data frame, typically resulting from  model.frame()
##' @param transpose logical indicating if  X' = t(X) {is faster!} or X
##' 	should be returned
##' @param drop.unused.levels logical indicating if unused factor levels
##'	should be dropped

##' @return sparse matrix (class "dgCMatrix")
model.spmatrix <- function(trms, mf, transpose=FALSE,
                           drop.unused.levels = TRUE, row.names=TRUE,
                           verbose = getOption('verbose'))

{
    ## Author: Martin Maechler, Date:  7 Jul 2009

    ## mf is a model frame or a "simple" data.frame [after reorder !]
    stopifnot(is.data.frame(mf))
    n <- nrow(mf)
    if(row.names)
        rnames <- row.names(mf)
    ## mf:  make into list, dropping all attributes (but the names)
    fnames <- names(mf <- unclass(mf))
    attributes(mf) <- list(names = fnames)

    factorPattern <- attr(trms, "factors")
    d <- dim(factorPattern)
    nVar <- d[1]
    nTrm <- d[2]
    n.fP <- dimnames(factorPattern)
    fnames <- n.fP[[1]] # == names of variables {incl. "F(var)"} in the model
    Names  <- n.fP[[2]] # == colnames == names of terms:  "a", "b:c", ...

    ## all the "variables in the model" are also in "mf", including "sin(x)";
    ## actually, ..../src/main/model.c even assumes
    stopifnot((m <- length(mf)) >= nVar)
    if(m > nVar) mf <- mf[seq_len(nVar)]
    stopifnot(fnames == names(mf))
    if(nVar == 0) nVar <- 1L # (as in ~/R/D/r-devel/R/src/main/model.c)
    ## Note: "character" variables have been changed to factor in the caller;
    ##     hence: both factor and *logical*  should be dealt as factor :
    indF <- which(is.f <- sapply(mf, function(.) is.factor(.) | is.logical(.)))

    hasInt <- attr(trms, "intercept") == 1
    ## the degree of interaction:
    intOrder <- attr(trms, "order")
    ##
    if(!hasInt && length(indF)) {
        ## change the '1' of the first factor into a '2' :
        if(any(i1 <- factorPattern[indF, ] == 1))
            ## replace at the first '1' location:
            factorPattern[indF,][which.max(i1)] <- 2L
        else {}
            ## nothing to do
    }
    ## Convert "factors" to "Rowwise- sparseMatrix ("dummy"-matrix) -----------
    ## Result: a list of sparse model matrices for the "factor"s :
    f.matr <- structure(vector("list", length = length(indF)),
                        names = fnames[indF])
    i.f <- 0
    ## ---- For each variable in the model -------------------
    for(i in 1:nVar) {
	nam <- fnames[i]
	f <- mf[[i]]
	if(is.f[i]) {
	    fp <- factorPattern[i,]   ## == factorPattern[nam,]
	    f.matr[[(i.f <- i.f + 1)]] <- # a list of 2
		lapply(fac2Sparse(f, to = "d",
				  drop.unused.levels=drop.unused.levels,
				  factorPatt12 = 1:2 %in% fp,
				  contrasts.arg = attr(f, "contrasts")),
		       function(s) {
			   if(is.null(s)) return(s)
			   ## else
			   rownames(s) <-
			       paste0(nam, if(is.null(rownames(s)))
				     ## for some contr.*(), have lost rownames; hmm..
				     seq_len(nrow(s)) else rownames(s))

			   s
		       })
	} else { ## continuous variable --> "matrix" - for all of them
	    nr <- if(is.matrix(f)) nrow(f <- t(f)) else (dim(f) <- c(1L, length(f)))[1]
	    if(is.null(rownames(f)))
		rownames(f) <- if(nr == 1) nam else paste0(nam, seq_len(nr))
	    mf[[i]] <- f
	}
    }

    result <- structure(vector("list", length = nTrm), names = Names)
    getR <- function(N) # using 'nm'
        if(!is.null(r <- f.matr[[N]])) r[[factorPattern[N, nm]]] else mf[[N]]
    for(j in seq_len(nTrm)) { ## j-th term
	nm <- Names[j]
        nmSplits <- strsplit(nm, ":", fixed=TRUE)[[1]]
        result[[j]] <- sparseInt.r(lapply(nmSplits, getR), do.names=TRUE)
    }

    attributes(trms) <- NULL       # only need formula
    ## r :=  t(X)
    r <- structure(do.call("rBind",
			   c(if(hasInt) list("(Intercept)" = 1), result)),
		   ## extra attributes added to the sparse Matrix:
                   ##  [do *NOT* cobble slots!]
		   termNames = Names, ## = names(result)
		   df = sapply(result, ncol),
		   call = match.call(),
		   formula = trms)
    if(row.names)
        colnames(r) <- rnames
    if(!transpose) t(r) else r
}## model.spmatrix()



### Keep this namespace-hidden: Would need to return a classed object

## FIXME: still test this function for both methods, since currently
## ----- both  dgCMatrix_cholsol and  dgCMatrix_qrsol are only called from here!
lm.fit.sparse <- function(x, y, offset = NULL, method = c("qr", "cholesky"),
                          tol = 1e-7, singular.ok = TRUE, order = NULL,
                          transpose = FALSE) ## NB: meaning of 'transpose'
                                        # is changed from original

### Fit a linear model, __ given __ a sparse model matrix 'x'
### using a sparse QR or a sparse Cholesky factorization
{
    cld <- getClass(class(x))
    stopifnot(extends(cld, "dsparseMatrix"))
## or     if(!is(x, "dsparseMatrix")) x <- as(x, "dsparseMatrix")
    yy <- as.numeric(y)
    if (!is.null(offset)) {
	stopifnot(length(offset) == length(y))
	yy <- yy - as.numeric(offset)
    }
    method <- match.arg(method)
    order <- {
        if(is.null(order)) ## recommended default depends on method :
            if(method == "qr") 3L else 1L
        else as.integer(order) }

    if(transpose) x <- t(x)
    ans <- switch(method,
		  cholesky =
		  .Call(dgCMatrix_cholsol,# has AS_CHM_SP(x)
			as(x, "CsparseMatrix"), yy),
		  qr =
		  .Call(dgCMatrix_qrsol, # has AS_CSP(): must be dgC or dtC:
			if(cld@className %in% c("dtCMatrix", "dgCMatrix")) x
			else as(x, "dgCMatrix"),
			yy, order),
		  ## otherwise:
		  stop("unknown method ", dQuote(method))
		  )
    ans
}

