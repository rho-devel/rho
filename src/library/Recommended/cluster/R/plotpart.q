### $Id: plotpart.q 5974 2011-12-20 17:47:08Z maechler $
plot.partition <-
function(x, ask = FALSE, which.plots = NULL,
	 nmax.lab = 40, max.strlen = 5, data = x$data, dist = NULL,
	 stand = FALSE, lines = 2,
	 shade = FALSE, color = FALSE, labels = 0, plotchar = TRUE,
	 span = TRUE, xlim = NULL, ylim = NULL, main = NULL, ...)
{
    if(is.null(x$data))# data not kept
	x$data <- data
    if(is.null(x$data) && !is.null(dist))
	x$diss <- dist
    if(is.null(which.plots) && !ask)
	which.plots <- {
	    if(is.null(x$data) && (is.null(x$diss) || inherits(x, "clara")))
		2 ## no clusplot
	    else 1:2
	}
    if(ask && is.null(which.plots)) { ## Use 'menu' ..
	tmenu <- paste("plot ", ## choices :
		       c("All", "Clusplot", "Silhouette Plot"))
	do.all <- FALSE
	repeat {
	    if(!do.all)
		pick <- menu(tmenu, title =
			     "\nMake a plot selection (or 0 to exit):\n") + 1
	    switch(pick,
		   return(invisible())# 0 -> exit loop
		   ,
		   do.all <- TRUE# 1 : All
		   ,
		   clusplot(x, stand = stand, lines = lines,
			    shade = shade, color = color, labels = labels,
			    plotchar = plotchar, span = span,
			    xlim = xlim, ylim = ylim, main = main, ...)
		   ,
		   plot(silhouette(x), nmax.lab, max.strlen, main = main)
		   )
	    if(do.all) { pick <- pick + 1; do.all <- pick <= length(tmenu) + 1}
	}
	invisible()
    }
    else {
	ask <- prod(par("mfcol")) < length(which.plots) && dev.interactive()
	if(ask) { op <- par(ask = TRUE); on.exit(par(op)) }
	for(i in which.plots)
	switch(i,
	       clusplot(x, stand = stand, lines = lines,
			shade = shade, color = color, labels = labels,
			plotchar = plotchar, span = span,
			xlim = xlim, ylim = ylim, main = main, ...)
	       ,
	       plot(silhouette(x), nmax.lab, max.strlen, main = main)
	       ) ## and return() whatever  *plot(..) returns
    }
}

clusplot <- function(x, ...) UseMethod("clusplot")


##' @title Make/Check the (n x 2) matrix needed for clusplot.default():
##' @param x  numeric matrix or dissimilarity matrix (-> clusplot.default())
##' @param diss	 logical indicating if 'x' is dissimilarity matrix
##' @return x1 : (n x 2) numeric matrix;
##'	var.dec: the "variance explained"
##' @author Martin Maechler
mkCheckX <- function(x, diss) {
    if(diss) {
	if(any(is.na(x)))
	    stop("NA-values are not allowed in dist-like 'x'.")
	if(inherits(x, "dist")) {
	    n <- attr(x, "Size")
	    labs <- attr(x, "Labels")
	}
	else { # x (num.vector or square matrix) must be transformed into diss.
	    siz <- sizeDiss(x)
	    if(is.na(siz)) {
		if((n <- nrow(x)) != ncol(x))
		    stop("Distances must be result of dist or a square matrix.")
		if(all.equal(x, t(x)) != TRUE)
		    stop("the square matrix is not symmetric.")
		labs <- dimnames(x)[[1]]
	    }
	    else {
		if(!is.vector(x)) {
		    labs <- attr(x, "Labels") # possibly NULL
		    x <- as.matrix(x)
		    if((n <- nrow(x)) == ncol(x) && all.equal(x, t(x)) == TRUE) {
			labs <- dimnames(x)[[1]]
		    }
		    else {
			## Hmm, when does this ever happen :
			## numeric, not-dist, non-vector, not symmetric matrix ?
			warning(">>>>> funny case in clusplot.default() -- please report!\n")
			## if(n != sizeDiss(x)) ...
			if(is.null(labs))
			    labs <- 1:sizeDiss(x)
			attr(x, "Size") <- sizeDiss(x)
		    }
		}
		else {
		    attr(x, "Size") <- n <- siz
		    labs <- 1:n
		}
	    }
	}
	if(is.null(labs)) labs <- 1:n

	x1 <- cmdscale(x, k = 2, eig = TRUE, add = TRUE)
	if(x1$ac < 0)
	    x1 <- cmdscale(x, k = 2, eig = TRUE)
	var.dec <- x1$GOF[2]		# always in [0,1]
	x1 <- x1$points
    }
    else { ## Not (diss)
	if(!is.matrix(x)) stop("x is not a data matrix")
	if(any(is.na(x))) {
	    y <- is.na(x)
	    if(any(apply(y, 1, all)))
		stop("one or more objects contain only missing values")
	    if(any(apply(y, 2, all)))
		stop("one or more variables contain only missing values")
	    x <- apply(x, 2, function(x)
		   { x[is.na(x)] <- median(x, na.rm = TRUE); x } )
	    message("Missing values were displaced by the median of the corresponding variable(s)")
	}

	n <- nrow(x)
	labs <- dimnames(x)[[1]]
	if(is.null(labs)) labs <- 1:n

	x1 <- if(ncol(x) == 1) {
	    hulp <- rep(0, length(x))
	    var.dec <- 1
	    matrix(c(t(x), hulp), ncol = 2)
	}
	else {
	    prim.pr <- princomp(x, scores = TRUE, cor = ncol(x) != 2)
	    var.dec <- cumsum(prim.pr$sdev^2/sum(prim.pr$ sdev^2))[2]
	    prim.pr$scores[, 1:2]
	}
    }
    list(x = x1, var.dec = var.dec, labs = labs)
}

clusplot.default <-
function(x, clus, diss = FALSE, s.x.2d = mkCheckX(x, diss),
         stand = FALSE, lines = 2,
	 shade = FALSE, color = FALSE, labels = 0, plotchar = TRUE,
	 col.p = "dark green", # was 5 (= shaded col)
	 col.txt = col.p, col.clus = if(color) c(2, 4, 6, 3) else 5,
	 cex = 1, cex.txt = cex,
	 span = TRUE, add = FALSE,  xlim = NULL, ylim = NULL,
	 main = paste("CLUSPLOT(", deparse(substitute(x)),")"),
	 sub = paste("These two components explain",
	       round(100 * var.dec, digits = 2), "% of the point variability."),
	 xlab = "Component 1", ylab = "Component 2",
	 verbose = getOption("verbose"),
	 ...)
{
    force(main)
    if(is.data.frame(x))
	x <- data.matrix(x)
    if(!is.numeric(x))
	stop("x is not numeric")
    stopifnot(is.list(s.x.2d),
	      c("x","labs","var.dec") %in% names(s.x.2d),
              (n <- nrow(x1 <- s.x.2d[["x"]])) > 0)
    labels1 <- s.x.2d[["labs"]]
    var.dec <- s.x.2d[["var.dec"]]
    ## --- The 2D space is setup and points are in x1[,]  (n x 2) ---

    clus <- as.vector(clus)
    if(length(clus) != n)
	stop("The clustering vector is of incorrect length")
    clus <- as.factor(clus)
    if(any(is.na(clus)))
	stop("NA-values are not allowed in clustering vector")
    if(stand)
	x1 <- scale(x1)

    levclus <- levels(clus)
    nC <- length(levclus) # the number of clusters

    d.x <- diff(range(x1[, 1]))
    d.y <- diff(range(x1[, 2]))
    z <- A <- vector("list", nC)
    loc <- matrix(0, nrow = nC, ncol = 2)
    d2 <- verhoud <- numeric(nC)
    ## num1 .. num6 : all used only once -- there are more constants anyway
    num3 <- 90
    num6 <- 70

    for(i in 1:nC) { ##-------------  i-th cluster  --------------
	x <- x1[clus == levclus[i],, drop = FALSE ]
	aantal <- nrow(x) # number of observations in cluster [i]
	cov <- var(if(aantal == 1) {
		     if(verbose)
			 cat("cluster",i," has only one observation ..\n")
		     rbind(x, c(0, 0))
		   } else x)
	x.1 <- range(x[, 1])
	y.1 <- range(x[, 2])
	notrank2 <- qr(cov, tol = 0.001)$rank != 2
	if(!span && notrank2) {
	    d2[i] <- 1
	    if((abs(diff(x.1)) > d.x/70) ||
	       (abs(diff(y.1)) > d.y/50)) {
		loc[i, ] <- c(x.1[1] + diff(x.1)/2, y.1[1] + diff(y.1)/2)
		a <- sqrt((loc[i, 1] - x.1[1])^2 +
			  (loc[i, 2] - y.1[1])^2)
		a <- a + 0.05 * a
		num2 <- 40
		if(abs(diff(x.1)) > d.x/70 ) {
		    ind1 <- which.max(x[,1])
		    ind2 <- which.min(x[,1])
		    q <- atan((x[ind1, 2] - x[ind2, 2])/
			      (x[ind1, 1] - x[ind2, 1]))
		    b <-
			if(d.y == 0)
			    1
			else if(abs(diff(y.1)) > d.y/50)
			    diff(y.1)/10 ## num1 <- 10
			else d.y/num2
		}
		else {
		    b <- if(d.x == 0) 1 else d.x/num2
		    q <- pi/2
		}
		D <- diag(c(a^2, b^2))
		R <- rbind(c(cos(q), -sin(q)),
			   c(sin(q),  cos(q)))
		A[[i]] <- (R %*% D) %*% t(R)
	    }
	    else {
		a <- d.x/num3
		b <- d.y/num6
		if(a == 0) a <- 1
		if(b == 0) b <- 1
		A[[i]] <- diag(c(a^2, b^2))
		loc[i, ] <- x[1, ]
	    }
	    oppervlak <- pi * a * b
	}
	else if(span && notrank2) {
	    d2[i] <- 1
	    if(sum(x[, 1] != x[1, 1]) != 0 ||
	       sum(x[, 2] != x[1, 2]) != 0) {
		loc[i, ] <- c(x.1[1] + diff(x.1)/2,
			      y.1[1] + diff(y.1)/2)
		a <- sqrt((loc[i, 1] - x.1[1])^2 +
			  (loc[i, 2] - y.1[1])^2)
		if(any(x[, 1] != x[1, 1])) {
		    ind1 <- which.max(x[,1])
		    ind2 <- which.min(x[,1])
		    q <- atan((x[ind1, 2] - x[ind2, 2])/
			      (x[ind1, 1] - x[ind2, 1]))
		}
		else {
		    q <- pi/2
		}
		b <- 1e-7
		D <- diag(c(a^2, b^2))
		R <- rbind(c(cos(q), -sin(q)),
			   c(sin(q),  cos(q)))
		A[[i]] <- (R %*% D) %*% t(R)
	    }
	    else {
		a <- d.x/num3
		b <- d.y/num6
		if(a == 0) a <- 1
		if(b == 0) b <- 1
		A[[i]] <- diag(c(a^2, b^2))
		loc[i, ] <- x[1, ]
	    }
	    oppervlak <- pi * a * b

	}
	else { ## rank2
	    if(!span) {
		loc[i, ] <- colMeans(x)
		d2[i] <- max(mahalanobis(x, loc[i, ], cov))
		## * (1+ 0.01)^2  --- dropped factor for back-compatibility
	    }
	    else { ## span and rank2
		if(verbose)
		    cat("span & rank2 : calling \"spannel\" ..\n")
		k <- 2L
		res <- .C(spannel,
			  aantal,
			  ndep= k,
			  dat = cbind(1., x),
			  sqdist = double(aantal),
			  l1 = double((k+1) ^ 2),
			  double(k),
			  double(k),
			  prob = double(aantal),
			  double(k+1),
			  eps = (0.01),## convergence tol.
			  maxit = 5000L,
			  ierr = integer(1))
		if(res$ierr != 0)
		    ## MM : exactmve not available here !
		    warning("Error in Fortran routine for the spanning ellipsoid,",
			    "\n rank problem??")

		cov <- cov.wt(x, res$prob)
		loc[i, ] <- cov$center
		## NB: cov.wt() in R has extra wt[] scaling; revert here:
		cov <- cov$cov * (1 - sum(cov$wt^2))
		d2[i] <- weighted.mean(res$sqdist, res$prob)

		if(verbose)
		    cat("ellipse( A= (", format(cov[1,]),"*", format(cov[2,2]),
			"),\n\td2=", format(d2[i]),
			", loc[]=", format(loc[i, ]), ")\n")
	    }
	    A[[i]] <- cov
	    ## oppervlak (flam.)  =  area (Engl.)
	    oppervlak <- pi * d2[i] * sqrt(cov[1, 1] * cov[2, 2] - cov[1, 2]^2)
	}

	z[[i]] <- ellipsoidPoints(A[[i]], d2[i], loc[i, ], n.half= 201)
	verhoud[i] <- aantal/oppervlak
    } ## end for( i-th cluster )

    x.range <- do.call(range, lapply(z, `[`, i=TRUE, j = 1))
    y.range <- do.call(range, lapply(z, `[`, i=TRUE, j = 2))
    verhouding <- sum(verhoud[verhoud < 1e7])
    if(verhouding == 0) verhouding <- 1
    ## num4 <- 37 ; num5 <- 3 --- but '41' is another constant
    density <- 3 + (verhoud * 37)/verhouding
    density[density > 41] <- 41
    if (span) {
	if (d.x == 0) ## diff(range(x[,1]) == 0 : x-coords all the same
	    x.range <- x1[1, 1] + c(-1,1)
	if (d.y == 0) ## diff(range(x[,2]) == 0 : y-coords all the same
	    y.range <- x1[1, 2] + c(-1,1)
    }
    if(is.null(xlim)) xlim <- x.range
    if(is.null(ylim)) ylim <- y.range
    if(length(col.p) < n) col.p <- rep(col.p, length= n)

    ## --- Now plotting starts ---

    ## "Main plot" --
    if(!add) {
	plot(x1, xlim = xlim, ylim = ylim,
	     xlab = xlab, ylab = ylab, main = main,
	     type = if(plotchar) "n" else "p", # if(plotchar) add points later
	     col = col.p, cex = cex, ...)
	if(!is.null(sub) && !is.na(sub) && nchar(sub) > 0)
	    title(sub = sub, adj = 0)
    }
    if(color) {
	if(length(col.clus) < min(4,nC))
	    stop("'col.clus' should have length 4 when color is TRUE")
	i.verh <- order(verhoud)
	jInd <- if(nC > 4) pam(verhoud[i.verh], 4)$clustering else 1:nC
	for(i in 1:nC) {
	    k <- i.verh[i]
	    polygon(z[[k]], density = if(shade) density[k] else 0,
		    col = col.clus[jInd[i]], ...)
	}
	col.clus <- col.clus[jInd][order(i.verh)]
    }
    else {
	for(i in 1:nC)
	    polygon(z[[i]], density = if(shade) density[i] else 0,
		    col = col.clus, ...)
    }

    ## points after polygon in order to write ON TOP:
    if(plotchar) {
	karakter <- 1:19
	for(i in 1:nC) {
	    iC <- clus == levclus[i]
	    points(x1[iC, , drop = FALSE], cex = cex,
		   pch = karakter[1+(i-1) %% 19], col = col.p[iC], ...)
	}
    }

    if(nC > 1 && (lines == 1 || lines == 2)) {
	## Draw lines between all pairs of the	nC  cluster (centers)

	## utilities for computing ellipse intersections:
	clas.snijpunt <- function(x, loc, m, n, p)
	{
	    if (    !is.na(xm <- x[1,m]) && loc[n, m] <= xm && xm <= loc[p, m]) x[1, ]
	    else if(!is.na(xm <- x[2,m]) && loc[n, m] <= xm && xm <= loc[p, m]) x[2, ]
	    else NA
	}
	coord.snijp1 <- function(x, gemid)
	    x[2, 2] - 2 * x[1, 2] * gemid + x[1, 1] * gemid^2
	coord.snijp2 <- function(x, d2, y)
	    ((x[1, 1] * x[2, 2] - x[1, 2]^2) * d2)/y
	coord.snijp3 <- function(xx, y, gemid)
	{
	    sy <- sqrt(y)
	    sy <- c(sy, -sy)
	    cbind(xx[1] + sy,
		  xx[2] + gemid*sy)
	}

	afstand <- matrix(0, ncol = nC, nrow = nC)
	for(i in 1:(nC - 1)) {
	    for(j in (i + 1):nC) {
		gemid <- (loc[j, 2] - loc[i, 2])/(loc[j, 1] - loc[i, 1])
		s0 <- coord.snijp1(A[[i]], gemid)
		b0 <- coord.snijp2(A[[i]], d2[i], s0)
		snijp.1 <- coord.snijp3(loc[i,], y=b0, gemid)
		s1 <- coord.snijp1(A[[j]], gemid)
		b1 <- coord.snijp2(A[[j]], d2[j], s1)
		snijp.2 <- coord.snijp3(loc[j,], y=b1, gemid)
		if(loc[i, 1] != loc[j, 1]) {
		    if(loc[i, 1] < loc[j, 1]) {
			punt.1 <- clas.snijpunt(snijp.1, loc, 1, i, j)
			punt.2 <- clas.snijpunt(snijp.2, loc, 1, i, j)
		    }
		    else {
			punt.1 <- clas.snijpunt(snijp.1, loc, 1, j, i)
			punt.2 <- clas.snijpunt(snijp.2, loc, 1, j, i)
		    }
		}
		else {
		    if(loc[i, 2] < loc[j, 2]) {
			punt.1 <- clas.snijpunt(snijp.1, loc, 2, i, j)
			punt.2 <- clas.snijpunt(snijp.2, loc, 2, i, j)
		    }
		    else {
			punt.1 <- clas.snijpunt(snijp.1, loc, 2, j, i)
			punt.2 <- clas.snijpunt(snijp.2, loc, 2, j, i)
		    }
		}
		if(is.na(punt.1[1]) || is.na(punt.2[1]) ||
		   (sqrt((punt.1[1] - loc[i, 1])^2 +
			 (punt.1[2] - loc[i, 2])^2) +
		    sqrt((punt.2[1] - loc[j, 1])^2 +
			 (punt.2[2] - loc[j, 2])^2)) >
		   sqrt((loc[j, 1] - loc[i, 1])^2 +
			(loc[j, 2] - loc[i, 2])^2))
		{
		    afstand[i, j] <- NA
		}
		else if(lines == 1) {
		    afstand[i, j] <- sqrt((loc[i, 1] - loc[j, 1])^2 +
					  (loc[i, 2] - loc[j, 2])^2)
		    segments(loc[i, 1], loc[i, 2],
			     loc[j, 1], loc[j, 2], col = 6, ...)
		}
		else { ## lines == 2
		    afstand[i, j] <- sqrt((punt.1[1] - punt.2[1])^2 +
					  (punt.1[2] - punt.2[2])^2)
		    segments(punt.1[1], punt.1[2],
			     punt.2[1], punt.2[2], col = 6, ...)
		}
	    }
	}
	afstand <- t(afstand) + afstand
    }
    else afstand <- NULL

    if(labels) {
	if(labels == 1) {
	    for(i in 1:nC) { ## add cluster border points
		m <- nrow(z[[i]])
		ni <- length(ii <- seq(1, m, by = max(1, m %/% 40)))
		x1 <- rbind(x1, z[[i]][ii, ])
		labels1 <- c(labels1, rep(levclus[i], ni))
		## identify() only allows one color:
		##col.txt <- c(col.txt, rep(col.clus[if(color) i else 1], ni))
	    }
	    identify(x1, labels = labels1, col = col.txt[1])
	}
	else {
### FIXME --- 'cex.txt' but also allow to specify 'cex' (for the points) ???
	    Stext <- function(xy, labs, ...) {
		## FIXME: these displacements are not quite ok!
		xy[, 1] <- xy[, 1] + diff(x.range)/130
		xy[, 2] <- xy[, 2] + diff(y.range)/50
		text(xy, labels = labs, ...)
	    }
	    if(labels == 3 || labels == 2)
		Stext(x1, labels1, col = col.txt, cex = cex.txt, ...)
	    if(labels %in% c(2,4,5)) {
		maxima <- t(sapply(z, `[`, i=201, j=1:2))
		Stext(maxima, levclus, font = 4, col = col.clus, cex = cex, ...)
	    }
	    if(labels == 5)
		identify(x1, labels = labels1, col = col.txt[1])
	}
    }
    density[density == 41] <- NA
    invisible(list(Distances = afstand, Shading = density))
}

clusplot.partition <- function(x, main = NULL, dist = NULL, ...)
{
    if(is.null(main) && !is.null(x$call))
	main <- paste("clusplot(",format(x$call),")", sep="")
    if(length(x$data) != 0 &&
       (!any(is.na(x$data)) || data.class(x) == "clara"))
	clusplot.default(x$data, x$clustering, diss = FALSE, main = main, ...)
    else if(!is.null(dist))
	clusplot.default(dist, x$clustering, diss = TRUE, main = main, ...)
    else if(!is.null(x$diss))
	clusplot.default(x$diss, x$clustering, diss = TRUE, main = main, ...)
    else { ## try to find "x$diss" by looking at the pam() call:
	if(!is.null(x$call)) {
	    xD <- try(eval(x$call[[2]], envir = parent.frame()))
	    if(inherits(xD, "try-error") || !inherits(xD, "dist"))
		stop("no diss nor data found, nor the original argument of ",
		     deparse(x$call))
	    ## else
	    ## warning("both 'x$diss' and 'dist' are empty; ",
	    ##	       "trying to find the first argument of ", deparse(x$call))
	    clusplot.default(xD, x$clustering, diss = TRUE, main = main, ...)
	}
	else stop("no diss nor data found for clusplot()'")
    }
}
