#### CLARA := Clustering LARge Applications
####
#### Note that the algorithm is O(n), but O(ns^2) where ns == sampsize

clara <- function(x, k, metric = "euclidean", stand = FALSE,
		  samples = 5, sampsize = min(n, 40 + 2 * k), trace = 0,
                  medoids.x = TRUE, keep.data = medoids.x, rngR = FALSE,
                  pamLike = FALSE)
{
    ## check type of input matrix and values of input numbers
    if(inherits(x, "dist"))# catch user error
	stop("'x' is a \"dist\" object, but should be a data matrix or frame")
    x <- data.matrix(x)
    if(!is.numeric(x)) stop("x is not a numeric dataframe or matrix.")
    n <- nrow(x)
    if((k <- as.integer(k)) < 1 || k > n - 1)
	stop("The number of cluster should be at least 1 and at most n-1." )
    if((sampsize <- as.integer(sampsize)) < max(2,k+1))
	stop(gettextf("'sampsize' should be at least %d = max(2, 1+ number of clusters)",
                      max(2,k+1)), domain=NA)
    if(n < sampsize)
	stop(gettextf("'sampsize' = %d should not be larger than the number of objects, %d",
                      sampsize, n), domain=NA)
    if((samples <- as.integer(samples)) < 1)
	stop("'samples' should be at least 1")

    jp <- ncol(x)
    namx <- dimnames(x)[[1]]
    ## standardize, if necessary {careful not to copy unnecessarily}:
    if(medoids.x) ## need to save original 'x'
        ox <- x
    else if(keep.data)
        stop("when 'medoids.x' is FALSE, 'keep.data' must be too")
    if(stand)
        x <- scale(x, scale = apply(x, 2, meanabsdev))
    if(keep.data)
        data <- x
    ## put info about metric, size and NAs in arguments for the .C call

    if((mdata <- any(inax <- is.na(x)))) { # TRUE if x[] has any NAs
	jtmd <- as.integer(ifelse(apply(inax, 2, any), -1, 1))
	## VALue for MISsing DATa
	valmisdat <- 1.1* max(abs(range(x, na.rm=TRUE)))
	x[inax] <- valmisdat
    } else rm(inax) # save space

    doDUP <- nzchar(dup <- Sys.getenv("R_cluster_clara_dup")) && isTRUE(as.logical(dup))
    if((trace <- as.integer(trace)))
	cat(sprintf("calling .C(cl_clara, ..., DUP = %s):\n", doDUP))
    res <- .C(cl_clara,
	      n,
	      jp,
	      k,
	      clu = as.double(x),
	      nran  = samples,
	      nsam  = sampsize,
	      dis   = double(1 + (sampsize * (sampsize - 1))/2),
	      mdata = as.integer(mdata),
	      valmd = if(mdata) rep(valmisdat, jp) else -1.,
	      jtmd  = if(mdata) jtmd else integer(1),
	      ndyst = as.integer(if(metric == "manhattan") 2 else 1),
              as.logical(rngR[1]),
              as.logical(pamLike[1]),
	      integer(sampsize),# = nrepr
	      integer(sampsize),# = nsel
	      sample= integer(sampsize),# = nbest
	      integer(k),		# = nr
	      imed = integer(k),	# = nrx
	      double(k),		# = radus
	      double(k),		# = ttd
	      double(k),		# = ratt
	      avdis  = double(k),	# = ttbes
	      maxdis = double(k),	# = rdbes
	      ratdis = double(k),	# = rabes
	      size  = integer(k),	# = mtt
	      obj   = double(1),
	      avsil = double(k),
	      ttsil = double(1),
	      silinf = matrix(0, sampsize, 4),
	      jstop = integer(1),
	      trace = trace,
	      tmp  = double (3 * sampsize),
	      itmp = integer(6 * sampsize),
	      DUP = doDUP)
    ## give a warning when errors occured
    if(res$jstop) {
	if(mdata && any(aNA <- apply(inax,1, all))) {
	    i <- which(aNA)
	    nNA <- length(i)
	    pasteC <- function(...) paste(..., collapse= ",")
	    stop(ngettext(nNA,
			  sprintf("Observation %d has *only* NAs --> omit it for clustering",
				  i[1]),
			  ## nNA > 1 :
			  paste(if(nNA < 13) sprintf("Observations %s", pasteC(i))
			  else sprintf("%d observations (%s ...)", nNA, pasteC(i[1:12])),
				"\thave *only* NAs --> na.omit() them for clustering!",
				sep = "\n")), domain = NA)
	} ## else
	if(res$jstop == 1)
	    stop("Each of the random samples contains objects between which\n",
		 " no distance can be computed.")
	if(res$jstop == 2)
	    stop("For each of the ", samples,
		 " samples, at least one object was found which\n could not",
		 " be assigned to a cluster (because of missing values).")
	## else {cannot happen}
	stop("invalid 'jstop' from .C(cl_clara,.): ", res$jstop)
    }
    ## 'res$clu' is still large; cut down ASAP
    res$clu <- as.integer(res$clu[1:n])
    sildim <- res$silinf[, 4]
    ## adapt C output to S:
    ## convert lower matrix, read by rows, to upper matrix, read by rows.
    disv <- res$dis[-1]
    disv[disv == -1] <- NA
    disv <- disv[upper.to.lower.tri.inds(sampsize)]
    class(disv) <- dissiCl
    attr(disv, "Size") <- sampsize
    attr(disv, "Metric") <- metric
    attr(disv, "Labels") <- namx[res$sample]
    res$med <- if(medoids.x) ox[res$imed, , drop = FALSE]
    ## add labels to C output
    if(!is.null(namx)) {
	sildim <- namx[sildim]
	res$sample <- namx[res$sample]
	names(res$clu) <- namx
    }
    r <- list(sample = res$sample, medoids = res$med, i.med = res$imed,
	      clustering = res$clu, objective = res$obj,
	      clusinfo = cbind(size = res$size, "max_diss" = res$maxdis,
	      "av_diss" = res$avdis, isolation = res$ratdis),
	      diss = disv, call = match.call())
    ## add dimnames to C output
    if(k > 1) {
	dimnames(res$silinf) <- list(sildim,
				     c("cluster", "neighbor", "sil_width", ""))
	r$silinfo <- list(widths = res$silinf[, -4],
			  clus.avg.widths = res$avsil,
			  avg.width = res$ttsil)
    }
    if(keep.data) r$data <- data
    class(r) <- c("clara", "partition")
    r
}

print.clara <- function(x, ...)
{
    cat("Call:	", deparse(x$call),
	"\nMedoids:\n");		print(x$medoids, ...)
    cat("Objective function:\t ", format(x$objective, ...),"\n",
	"Clustering vector: \t", sep=""); str(x$clustering, vec.len = 7)
    cat("Cluster sizes:	    \t", x$clusinfo[,1],
	"\nBest sample:\n");		print(x$sample, quote = FALSE, ...)
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}

summary.clara <- function(object, ...)
{
    class(object) <- "summary.clara"
    object
}

print.summary.clara <- function(x, ...)
{
    cat("Object of class 'clara' from call:\n", deparse(x$call),
	"\nMedoids:\n");		print(x$medoids, ...)
    cat("Objective function:\t ", format(x$objective, ...),
	"\nNumerical information per cluster:\n")
    print(x$clusinfo, ...)
    if(has.sil <- !is.null(x$silinfo)) {
	cat("Average silhouette width per cluster:\n")
	print(x$silinfo[[2]], ...)
	cat("Average silhouette width of best sample:",
	    format(x$silinfo[[3]], ...), "\n")
    }
    cat("\nBest sample:\n");		print(x$sample, quote = FALSE, ...)
    cat("Clustering vector:\n");	print(x$clustering, ...)
    if(has.sil) {
	cat("\nSilhouette plot information for best sample:\n")
	print(x$silinfo[[1]], ...)
    }
    if(!is.null(x$diss)) { ## Dissimilarities:
	cat("\n");			print(summary(x$diss, ...))
    }
    cat("\nAvailable components:\n");	print(names(x), ...)
    invisible(x)
}

