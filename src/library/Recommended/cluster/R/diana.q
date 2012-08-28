### $Id: diana.q 6015 2012-01-18 11:03:38Z maechler $

diana <- function(x, diss = inherits(x, "dist"),
		  metric = "euclidean", stand = FALSE,
		  ##_not_yet stop.at.k = FALSE,
                  keep.diss = n < 100, keep.data = !diss, trace.lev = 0)
{
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
	n <- as.integer(attr(x, "Size"))
	dv <- x[lower.to.upper.tri.inds(n)]
	## prepare arguments for the Fortran call
	dv <- c(0., dv)# double
	jp <- 1L
	mdata <- FALSE
	ndyst <- 0
	x2 <- double(1)
    }
    else {
	## check input matrix and standardize, if necessary
	x <- data.matrix(x)
	if(!is.numeric(x)) stop("x is not a numeric dataframe or matrix.")
	x2 <- if(stand) scale(x, scale = apply(x, 2, meanabsdev)) else x
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
    stopifnot(length(trace.lev <- as.integer(trace.lev)) == 1)
##_not_yet     stopifnot(is.logical(stop.at.k) ||
##_not_yet               (is.numeric(stop.at.k) && 1 <= stop.at.k && stop.at.k <= n))
    C.keep.diss <- keep.diss && !diss
    res <- .C(twins,
		    n,
		    jp,
		    as.double(x2),
		    dv,
		    dis = double(if(C.keep.diss) length(dv) else 1),
		    jdyss = if(C.keep.diss) diss + 10L else as.integer(diss),
		    if(mdata)valmd else double(1),
		    if(mdata) jtmd else integer(jp),
		    as.integer(ndyst),
		    2L,# jalg = 2 <==> DIANA
                    0L, ##_not_yet as.integer(stop.at.k),# default: 0 do *not* stop early,
		    integer(n),
		    ner = integer(n),
		    ban = double(n),
		    dc = as.double(trace.lev),# in / out
		    double(1),
		    merge = matrix(0L, n - 1, 2), # integer
		    DUP = FALSE)
    if(!diss) {
	## give warning if some dissimilarities are missing.
	if(res$jdyss == -1)
	    stop("No clustering performed, NA's in dissimilarity matrix.\n")
        if(keep.diss) {
            ## adapt Fortran output to S:
            ## convert lower matrix, read by rows, to upper matrix, read by rows.
            disv <- res$dis[-1]
            disv[disv == -1] <- NA
            disv <- disv[upper.to.lower.tri.inds(n)]
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- dimnames(x)[[1]]
        }
	## add labels to Fortran output
	if(length(dimnames(x)[[1]]) != 0)
	    order.lab <- dimnames(x)[[1]][res$ner]
    }
    else {
        if(keep.diss) disv <- x
	## add labels to Fortran output
	if(length(attr(x, "Labels")) != 0)
	    order.lab <- attr(x, "Labels")[res$ner]
    }
    clustering <- list(order = res$ner, height = res$ban[-1], dc = res$dc,
		       merge = res$merge, diss = if(keep.diss)disv,
                       call = match.call())
    if(exists("order.lab"))
	clustering$order.lab <- order.lab
    if(keep.data && !diss) {
	if(mdata) x2[x2 == valmisdat] <- NA
	clustering$data <- x2
    }
    class(clustering) <- c("diana", "twins")
    clustering
}

print.diana <- function(x, ...)
{
    cat("Merge:\n")
    print(x$merge, ...)
    cat("Order of objects:\n")
    print(if (length(x$order.lab) != 0) x$order.lab else x$order,
	  quote = FALSE, ...)
    cat("Height:\n")
    print(x$height, ...)
    cat("Divisive coefficient:\n")
    print(x$dc, ...)
    cat("\nAvailable components:\n")
    print(names(x), ...)
    invisible(x)
}

summary.diana <- function(object, ...)
{
    class(object) <- "summary.diana"
    object
}

print.summary.diana <- function(x, ...)
{
    cat("Merge:\n");			print(x$merge, ...)
    cat("Order of objects:\n")
    print(if(length(x$order.lab)) x$order.lab else x$order, quote = FALSE, ...)
    cat("Height:\n");			print(x$height, ...)
    cat("Divisive coefficient:\n");	print(x$dc, ...)
    if(!is.null(x$diss)) { ## Dissimilarities:
	cat("\n");			print(summary(x$diss, ...))
    }
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}
