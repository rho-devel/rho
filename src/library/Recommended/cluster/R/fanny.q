#### $Id: fanny.q 5977 2011-12-21 09:42:55Z maechler $
fanny <- function(x, k, diss = inherits(x, "dist"), memb.exp = 2,
                  metric = c("euclidean", "manhattan", "SqEuclidean"),
                  stand = FALSE, iniMem.p = NULL, cluster.only = FALSE,
                  keep.diss = !diss && !cluster.only && n < 100,
                  keep.data = !diss && !cluster.only,
                  maxit = 500, tol = 1e-15, trace.lev = 0)
{
    if((diss <- as.logical(diss))) {
	## check type of input vector
	if(any(is.na(x))) stop(..msg$error["NAdiss"])
	if(data.class(x) != "dissimilarity") { # try to convert to
	    if(!is.null(dim(x))) {
		x <- as.dist(x)         # or give an error
	    } else {
		## possibly convert input *vector*
		if(!is.numeric(x) || is.na(n <- sizeDiss(x)))
		    stop(..msg$error["non.diss"])
		attr(x, "Size") <- n
	    }
	    class(x) <- dissiCl
	    if(is.null(attr(x,"Metric"))) attr(x, "Metric") <- "unspecified"
	}
	## prepare arguments for the Fortran call
	n <- attr(x, "Size")
	dv <- as.double(c(x, 0))# add extra one
	jp <- 1
	mdata <- FALSE
	ndyst <- 0L
	x2 <- double(n)
	jdyss <- 1
    }
    else {
	## check input matrix and standardize, if necessary
	x <- data.matrix(x)
	if(!is.numeric(x)) stop("x is not a numeric dataframe or matrix.")
	x2 <- if(stand) scale(x, scale = apply(x, 2, meanabsdev)) else x
	metric <- match.arg(metric)
	## put info about metric, size and NAs in arguments for the Fortran call
        ndyst <- which(metric == eval(formals()$metric))# 1, 2, or 3
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
	jdyss <- 0
    }
    if((k <- as.integer(k)) < 1 || k > n%/%2 - 1)
	stop("'k' (number of clusters) must be in {1,2, .., n/2 -1}")
    if(length(memb.exp) != 1 || (memb.exp <- as.double(memb.exp)) < 1
       || memb.exp == Inf)
        stop("'memb.exp' must be a finite number > 1")
    if((maxit <- as.integer(maxit)[1]) < 0)
        stop("'maxit' must be non-negative integer")
    computeP <- is.null(iniMem.p) # default: determine initial membership in C
    if(computeP)# default: determine initial membership in C
        iniMem.p <- matrix(0., n, k)# all 0 -> will be used as 'code'
    else {
        dm <- dim(iniMem.p)
        if(length(dm) !=2 || !all(dm == c(n,k)) ||
           !is.numeric(iniMem.p) || any(iniMem.p < 0) ||
           !isTRUE(all.equal(unname(rowSums(iniMem.p)), rep(1, n))))
            stop("'iniMem.p' must be a nonnegative n * k matrix with rowSums == 1")
        if(!is.double(iniMem.p)) storage.mode(iniMem.p) <- "double"
    }
    stopifnot(length(cluster.only) == 1)
    stopifnot(length(trace.lev) == 1)

    ## call Fortran routine
    storage.mode(x2) <- "double"
    res <- .C(cl_fanny,
              as.integer(n),
              as.integer(jp),
              k,
              x2,
              dis = dv,
              ok = as.integer(jdyss),
              if(mdata)valmd else double(1),
              if(mdata) jtmd else integer(jp),
              ndyst,
              integer(n), # nsend
              integer(n), # nelem
              integer(n), # negbr
              double(n),  # syl
              p = iniMem.p,
              dp = matrix(0., n, k),# < must all be 0 on entry!
              avsil = double(k),# 'pt'
              integer(k), # nfuzz
              double(k),  # esp
              double(k),  # ef
              double(n),  # dvec
              ttsil = as.double(0),
              obj = as.double(c(cluster.only, trace.lev, computeP, 0)),# in & out!
              clu = integer(n),
              silinf = if(cluster.only) 0. else matrix(0., n, 4),
              memb.exp = memb.exp,# = 'r'
              tol = as.double(tol),
              maxit = maxit)

    if(!(converged <- res$maxit > 0)) {
        warning(sprintf(
            "FANNY algorithm has not converged in 'maxit' = %d iterations",
                        maxit))
    }

    if(!cluster.only) sildim <- res$silinf[, 4]
    if(diss) {
	if(keep.diss) disv <- x
        labs <- attr(x, "Labels")
    }
    else {
	## give warning if some dissimilarities are missing.
	if(res$ok == -1)
	    stop("No clustering performed, NA-values in the dissimilarity matrix.")
        labs <- dimnames(x)[[1]]
        if(keep.diss) {
            disv <- res$dis[ - (1 + (n * (n - 1))/2)] # drop the extra one
            disv[disv == -1] <- NA
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- labs
        }
    }
    ## add labels, dimnames, etc  to Fortran output:
    if(length(labs) != 0) {
        if(!cluster.only) sildim <- labs[sildim]
        dimnames(res$p) <- list(labs, NULL)
        names(res$clu) <- labs
    }
    coeff <- if(memb.exp == 2) res$obj[3:4] else {
        ## usual partition coefficient with " ^ 2 " :
        cf <- sum(res$p ^ 2) / n
        c(cf, (k * cf - 1)/(k - 1))
    }
    names(coeff) <- c("dunn_coeff", "normalized")
    if(abs(coeff["normalized"]) < 1e-7)
        warning("the memberships are all very close to 1/k. Maybe decrease 'memb.exp' ?")
    k.crisp <- res$obj[1]
    res$obj <- c("objective" = res$obj[2])

    r <- list(membership = res$p, coeff = coeff, memb.exp = memb.exp,
              clustering = res$clu, k.crisp = k.crisp,
              # 'obj*': also containing iterations for back compatibility:
              objective = c(res$obj, "tolerance" = res$tol),
              convergence = c(iterations = res$maxit, converged = converged, maxit = maxit),
              diss = if(keep.diss) disv,
              call = match.call())
    if(k != 1 && !cluster.only) {
	dimnames(res$silinf) <- list(sildim,
				     c("cluster", "neighbor", "sil_width", ""))
	r$silinfo <- list(widths = res$silinf[, -4],
                          clus.avg.widths = res$avsil[1:k],
                          avg.width = res$ttsil)
    }
    if(keep.data && !diss) {
	if(mdata) x2[x2 == valmisdat] <- NA
	r$data <- x2
    }
    class(r) <- c("fanny", "partition")
    r
}

## non-exported:
.print.fanny <- function(x, digits = getOption("digits"), ...) {
    cat("Fuzzy Clustering object of class 'fanny' :")
    print(formatC(cbind(" " = c("m.ship.expon." = x$memb.exp,
			x$objective[c("objective", "tolerance")],
			x$convergence, "n" = nrow(x$membership))),
		  digits = digits),
	  quote = FALSE, ...)
    k <- ncol(x$membership)
    cat("Membership coefficients (in %, rounded):\n"); print(round(100 * x$membership), ...)
    cat("Fuzzyness coefficients:\n");	print(x$coeff, digits = digits, ...)
    cat("Closest hard clustering:\n");	print(x$clustering, ...)
    if(x$k.crisp < k)
	cat(sprintf("k_crisp (= %d) < k !!\n", x$k.crisp))
}

print.fanny <- function(x, digits = getOption("digits"), ...)
{
    .print.fanny(x, digits = digits, ...)
    cat("\nAvailable components:\n")
    print(names(x), ...)
    invisible(x)
}

summary.fanny <- function(object, ...)
{
    class(object) <- "summary.fanny"
    object
}

print.summary.fanny <- function(x, digits = getOption("digits"), ...)
{
    .print.fanny(x, digits = digits, ...)
    if(length(x$silinfo) != 0) {
	cat("\nSilhouette plot information:\n")
	print(x$silinfo[[1]], ...)
	cat("Average silhouette width per cluster:\n")
	print(x$silinfo[[2]], ...)
	cat("Average silhouette width of total data set:\n")
	print(x$silinfo[[3]], ...)
    }
    if(!is.null(x$diss)) { ## Dissimilarities:
	cat("\n");			print(summary(x$diss, ...))
    }
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}

## FIXME: Export and document these! -----------------------

## Convert crisp clustering vector to fuzzy membership matrix
as.membership <- function(clustering, keep.names = TRUE) {
    stopifnot(is.numeric(clustering), clustering == round(clustering))
    n <- length(clustering)
    k <- length(u <- sort(unique(clustering)))
    r <- matrix(0L, n, k)
    if(k == 0 || n == 0) return(r)
    if(keep.names)
	dimnames(r) <- list(names(clustering), NULL)
    if(any(u != 1:k)) clustering <- match(clustering, u)
    r[cbind(1:n, clustering)] <- 1L
    r
}

## "Generalized Inverse" transformation:
## Convert fuzzy membership matrix to closest crisp clustering vector
toCrisp <- function(m)
{
    dm <- dim(m)
    if(length(dm) != 2 || !is.numeric(m) || any(m < 0) ||
       !isTRUE(all.equal(unname(rowSums(m)), rep(1, dm[1]))))
        stop("'m', a membership matrix, must be nonnegative with rowSums == 1")
    apply(m, 1, which.max)
}
