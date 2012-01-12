### $Id: plothier.q 5642 2010-11-03 09:33:50Z maechler $

pltree <- function(x, ...) UseMethod("pltree")

## note: pltree() can have an 'xlab' in "..." (plot.hclust has an explicit one)
pltree.twins <- function(x, main = paste("Dendrogram of ", deparse(x$call)),
			 labels = NULL, ylab = "Height", ...)
{

    plot(as.hclust(x), labels = labels,
##-     if(is.null(labels) && length(x$order.lab) != 0)
##- 	labels <- x$order.lab[sort.list(x$order)]
##-
##-     ## calling plot.hclust() via generic :
##-     plot(structure(list(merge = x$merge, order = x$order,
##-                         height = sort(x$height), labels = labels,
##-                         call = x$call, method = x$method),
##-                    class = "hclust"),
         main = main, ylab = ylab, ...)
}

bannerplot <-
function(x, w = rev(x$height), fromLeft = TRUE, main=NULL, sub=NULL,
         xlab = "Height", adj = 0, col = c(2, 0), border = 0,
	 axes = TRUE, frame.plot = axes, rev.xax = !fromLeft, xax.pretty = TRUE,
	 labels = NULL, nmax.lab = 35, max.strlen = 5,
	 yax.do = axes && length(x$order) <= nmax.lab,
	 yaxRight = fromLeft, y.mar = 2.4 + max.strlen / 2.5, ...)
{
    m <- max(w)
    if(axes) {
	if(xax.pretty) {
	    at.vals <- if(!is.logical(xax.pretty))
		pretty(c(0,w), n = xax.pretty) else pretty(c(0,w))
	    n <- length(at.vals <- at.vals[at.vals <= m])
	    if(at.vals[n] * 1.01 < m) {
		lab.vals <- c(at.vals, signif(m, 3))
		at.vals <-  c(at.vals, m)
	    } else lab.vals <- at.vals
	} else { # old default for plot.agnes() and plot.diana()
	    ss <- seq(0, floor(m), length = 11)# => intervals = 1/10 {total}
	    at.vals  <- c(ss, m)
	    lab.vals <- round(at.vals, 2)
	}
    }
    if(fromLeft) {
	w <- rbind(w, m - w)
	if(missing(col)) col <- rev(col)
    } else { ## from Right
	w <- rbind(m - w, w)
	if(axes && rev.xax) {
	    at.vals <- m - rev(at.vals)## == c(0, ss + m - floor(m))
	    lab.vals <- rev(lab.vals)
	}
    }
    if(yax.do) {
	ax <- if(yaxRight)
	    list(side = 4, pos = m)
	else list(side = 2, pos = 0)
	if((pm <- par("mar"))[ax$side] < y.mar) {
	    ## need space besides y axis for labeling
	    pm[ax$side] <- y.mar
	    op <- par(mar = pm)
	    on.exit(par(op))
	}
    }
    barplot(w, xlab = xlab, horiz = TRUE, space = 0, axes = FALSE,
	    col = col, border = border, mgp = c(2.5, 1, 0), ...)
    if(frame.plot && (border == 0 || border == par("bg")))
	rect(0, 0, m, ncol(w))

    title(main = main, sub = sub, adj = adj)
    if(axes) {
	axis(1, at = at.vals, labels = lab.vals, ...)
	if(yax.do) {
	    if(is.null(labels))
		labels <- rev(if (length(x$order.lab) != 0)
			      substring(x$order.lab, 1,max.strlen) else x$order)
	    axis(ax$side, at = 0:(length(x$order) - 1), las = 1,
		 labels = labels, pos = ax$pos, mgp = c(3, 1.25, 0), ...)
	}
    }
    invisible()
}

## plot.diana() [further down] & plot.agnes() are  almost identical;
## --  made  bannerplot() a stand-alone function
## --> maybe *merge* these two into one plot.twins()
plot.agnes <-
function(x, ask = FALSE, which.plots = NULL, main = NULL,
	 sub = paste("Agglomerative Coefficient = ", round(x$ac, digits = 2)),
         adj = 0, nmax.lab = 35, max.strlen = 5, xax.pretty = TRUE, ...)
{
    if(is.null(main)) {
	## Different default for banner & pltree:
	cl <- deparse(x$call)
	main1 <- paste("Banner of ", cl)
	main2 <- paste("Dendrogram of ", cl)
    }
    else { # same title for both
	main1 <- main2 <- main
    }

    if(is.null(which.plots) && !ask)
	which.plots <- 1:2
    if(ask && is.null(which.plots)) { ## Use 'menu' ..
	tmenu <- paste("plot ", ## choices :
		       c("All", "Banner", "Clustering Tree"))
	do.all <- FALSE
	repeat {
	    if(!do.all)
		pick <- menu(tmenu, title =
			     "\nMake a plot selection (or 0 to exit):\n") + 1
	    switch(pick,
		   return(invisible()), # 0 -> exit loop
		   do.all <- TRUE,# 1 : All
		   bannerplot(x, fromLeft = TRUE,
			      main = main1, sub = sub, adj = adj,
			      xax.pretty = 10,
			      nmax.lab= nmax.lab, max.strlen= max.strlen, ...),
		   pltree (x, main = main2, sub = sub, ...) # 3
		   )
	    if(do.all) { pick <- pick + 1; do.all <- pick <= length(tmenu) + 1}
	}
    }
    else {
	ask <- prod(par("mfcol")) < length(which.plots) && dev.interactive()
	if(ask) {
	    op <- par(ask = TRUE)
	    on.exit(par(op))
	}
	for(i in which.plots)
	switch(i,
	       bannerplot(x, fromLeft = TRUE,
			  main = main1, sub = sub, adj = adj,
			  xax.pretty = 10,
			  nmax.lab = nmax.lab, max.strlen = max.strlen, ...),
	       pltree (x, main = main2, sub = sub, ...)
	       )
    }
    invisible()
}

plot.diana <-
function(x, ask = FALSE, which.plots = NULL, main = NULL,
	 sub  = paste("Divisive Coefficient = ", round(x$dc, digits = 2)),
	 adj = 0, nmax.lab = 35, max.strlen = 5, xax.pretty = TRUE, ...)
{
    if(is.null(main)) {
	## Different default for banner & pltree:
	cl <- deparse(x$call)
	main1 <- paste("Banner of ", cl)
	main2 <- paste("Dendrogram of ", cl)
    }
    else { # same title for both
	main1 <- main2 <- main
    }

    if(is.null(which.plots) && !ask)
	which.plots <- 1:2
    if(ask && is.null(which.plots)) { ## Use 'menu' ..
	tmenu <- paste("plot ", ## choices :
		       c("All", "Banner", "Clustering Tree"))
	do.all <- FALSE
	repeat {
	    if(!do.all)
		pick <- menu(tmenu, title =
			     "\nMake a plot selection (or 0 to exit):\n") + 1
	    switch(pick,
		   return(invisible()), # 0 -> exit loop
		   do.all <- TRUE,# 1 : All
		   bannerplot(x, fromLeft = FALSE,
			      main = main1, sub = sub, adj = adj,
			      xax.pretty = 10,
			      nmax.lab= nmax.lab, max.strlen= max.strlen, ...),
		   pltree (x, main = main2, sub = sub, ...)
		   )
	    if(do.all) { pick <- pick + 1; do.all <- pick <= length(tmenu) + 1}
	}
    }
    else {
	ask <- prod(par("mfcol")) < length(which.plots) && dev.interactive()
	if(ask) {
	    op <- par(ask = TRUE)
	    on.exit(par(op))
	}
	for(i in which.plots)
	switch(i,
	       bannerplot(x, fromLeft = FALSE, main = main1, sub = sub,
			  adj = adj, xax.pretty = 10,
			  nmax.lab = nmax.lab, max.strlen = max.strlen, ...),# 1
	       pltree (x, main = main2, sub = sub, ...) # i = 2
	       )
    }
    invisible()
}

plot.mona <- function(x, main = paste("Banner of ", deparse(x$call)),
		      sub = NULL, xlab = "Separation step",
		      col = c(2,0), axes = TRUE, adj = 0,
		      nmax.lab = 35, max.strlen = 5, ...)
{
    w <- rev(x$step)
    m <- max(w)
    if(any(i0 <- w == 0))
	w[i0] <- m <- m+1
    bannerplot(x[c("order","order.lab")], w = w, fromLeft = TRUE,
	       yaxRight = FALSE, col = col, main = main, sub = sub, xlab = xlab,
	       adj= adj, axes= axes, nmax.lab= nmax.lab, max.strlen= max.strlen,
	       xax.pretty = m+1, ...)
    names <- paste(" ", rev(x$variable))
    is.na(names) <- i0
    text(w, 1:length(names) - 0.5, names, adj = 0, col = col[1], ...)
}
