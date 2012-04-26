### $Id: diana.q 5598 2010-06-24 10:20:48Z maechler $

diana <- function(x, diss = inherits(x, "dist"),
		  metric = "euclidean", stand = FALSE,
                  keep.diss = n < 100, keep.data = !diss)
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
	jp <- as.integer(1)
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
    res <- .Fortran(twins,
		    n,
		    jp,
		    as.double(x2),
		    dv,
		    dis = double(if(keep.diss) length(dv) else 1),
		    ok = as.integer(if(keep.diss) diss + 10 else diss),
		    if(mdata)valmd else double(1),
		    if(mdata) jtmd else integer(jp),
		    as.integer(ndyst),
		    as.integer(2),# jalg = 2 <==> DIANA
		    as.integer(0),# ~ method
		    integer(n),
		    ner = integer(n),
		    ban = double(n),
		    dc = double(1),# care! as.double() is copy-less from 2.6.0
		    double(1),
		    merge = matrix(0:0, n - 1, 2), # integer
		    DUP = FALSE)
    if(!diss) {
	## give warning if some dissimilarities are missing.
	if(res$ok == -1)
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
