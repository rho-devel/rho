#### $Id: agnes.q 6015 2012-01-18 11:03:38Z maechler $
agnes <- function(x, diss = inherits(x, "dist"), metric = "euclidean",
		  stand = FALSE, method = "average", par.method,
                  keep.diss = n < 100, keep.data = !diss)
{
    METHODS <- c("average", "single","complete", "ward","weighted", "flexible")
    ## hclust has more;  1    2         3           4       5         6
    meth <- pmatch(method, METHODS)
    if(is.na(meth)) stop("invalid clustering method")
    if(meth == -1) stop("ambiguous clustering method")
    method <- METHODS[meth]
    if(method == "flexible") {
        ## Lance-Williams formula (but *constant* coefficients):
        par.method <- as.numeric(par.method) # or barf
        stopifnot((np <- length(par.method)) >= 1)
        if(np == 1)## default (a1= a, a2= a, b= 1-2a, c = 0)
            par.method <- c(par.method, par.method, 1-2*par.method, 0)
        else if(np == 3)
            par.method <- c(par.method, 0)
        else if(np != 4)
            stop("'par.method' must be of length 1, 3, or 4")
        attr(method,"par") <- par.method
    } else par.method <- double(1)

    if((diss <- as.logical(diss))) {
	## check type of input vector
	if(any(is.na(x))) stop(..msg$error["NAdiss"])
	if(data.class(x) != "dissimilarity") { # try to convert to
	    if(!is.null(dim(x))) {
		x <- as.dist(x) # or give an error
	    } else {
		## possibly convert input *vector*
		if(!is.numeric(x) || is.na(n <- sizeDiss(x)))
		    stop(..msg$error["non.diss"])
		attr(x, "Size") <- n
	    }
	    class(x) <- dissiCl
	    if(is.null(attr(x,"Metric"))) attr(x, "Metric") <- "unspecified"
	}
	n <- attr(x, "Size")
	dv <- x[lower.to.upper.tri.inds(n)]
	## prepare arguments for the Fortran call
	dv <- c(0., dv)# "double", 1st elem. "only for Fortran" (?)
	jp <- 1
	mdata <- FALSE
	ndyst <- 0
	x2 <- double(1)
    }
    else {
	## check input matrix and standardize, if necessary
	x <- data.matrix(x)
	if(!is.numeric(x)) stop("x is not a numeric dataframe or matrix.")
	x2 <- if(stand) scale(x, scale = apply(x, 2, meanabsdev)) else x
        storage.mode(x2) <- "double"
	ndyst <- if(metric == "manhattan") 2 else 1
	n <- nrow(x2)
	jp <- ncol(x2)
	if((mdata <- any(inax <- is.na(x2)))) { # TRUE if x[] has any NAs
	    jtmd <- as.integer(ifelse(apply(inax, 2, any), -1, 1))
	    ## VALue for MISsing DATa
	    valmisdat <- 1.1* max(abs(range(x2, na.rm=TRUE)))
	    x2[inax] <- valmisdat
	    valmd <- rep(valmisdat, jp)
	}
	dv <- double(1 + (n * (n - 1))/2)
    }
    if(n <= 1) stop("need at least 2 objects to cluster")
    C.keep.diss <- keep.diss && !diss
    res <- .C(twins,
		    as.integer(n),
		    as.integer(jp),
		    x2,
		    dv,
		    dis = double(if(C.keep.diss) length(dv) else 1),
		    jdyss = if(C.keep.diss) diss + 10L else as.integer(diss),
		    if(mdata) valmd else double(1),
		    if(mdata) jtmd else integer(jp),
		    as.integer(ndyst),
		    1L,# jalg = 1 <==> AGNES
		    meth,# integer
		    integer(n),
		    ner = integer(n),
		    ban = double(n),
		    ac = as.double(0), ## as.double(trace.lev),# in / out
                    par.method,
		    merge = matrix(0L, n - 1, 2), # integer
                    DUP = FALSE)
    if(!diss) {
	##give warning if some dissimilarities are missing.
	if(res$jdyss == -1)
	    stop("No clustering performed, NA-values in the dissimilarity matrix.\n" )
        if(keep.diss) {
            ## adapt Fortran output to S:
            ## convert lower matrix,read by rows, to upper matrix, read by rows.
            disv <- res$dis[-1]
            disv[disv == -1] <- NA
            disv <- disv[upper.to.lower.tri.inds(n)]
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- dimnames(x)[[1]]
        }
	##add labels to Fortran output
	if(length(dimnames(x)[[1]]) != 0)
	    order.lab <- dimnames(x)[[1]][res$ner]
    }
    else {
        if(keep.diss) disv <- x
	##add labels to Fortran output
	if(length(attr(x, "Labels")) != 0)
	    order.lab <- attr(x, "Labels")[res$ner]
    }
    clustering <- list(order = res$ner, height = res$ban[-1], ac = res$ac,
		       merge = res$merge, diss = if(keep.diss)disv,
                       call = match.call(), method = METHODS[meth])
    if(exists("order.lab"))
	clustering$order.lab <- order.lab
    if(keep.data && !diss) {
	if(mdata) x2[x2 == valmisdat] <- NA
	clustering$data <- x2
    }
    class(clustering) <- c("agnes", "twins")
    clustering
}

summary.agnes <- function(object, ...)
{
    class(object) <- "summary.agnes"
    object
}

print.agnes <- function(x, ...)
{
    cat("Call:	", deparse(x$call),
	"\nAgglomerative coefficient: ", format(x$ac, ...),
	"\nOrder of objects:\n")
    print(if(length(x$order.lab) != 0) x$order.lab else x$order,
	  quote = FALSE, ...)
    cat("Height (summary):\n");		print(summary(x$height), ...)
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}

print.summary.agnes <- function(x, ...)
{
    ## a bit more than print.agnes() ..
    cat("Object of class 'agnes' from call:\n", deparse(x$call),
	"\nAgglomerative coefficient: ", format(x$ac, ...),
	"\nOrder of objects:\n")
    print(if(length(x$order.lab) != 0) x$order.lab else x$order,
	  quote = FALSE, ...)
    cat("Merge:\n");			print(x$merge, ...)
    cat("Height:\n");			print(x$height, ...)
    if(!is.null(x$diss)) { ## Dissimilarities:
	cat("\n");			print(summary(x$diss, ...))
    }
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}

as.dendrogram.twins <- function(object, ...) ## ... : really only 'hang'
    as.dendrogram(as.hclust(object), ...)
