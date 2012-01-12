
## Moving from Bioconductor package geneplotter, in conjunction with
## the move of smoothScatter to package graphics.

## .smoothScatterCalcDensity() is also in graphics, but not exported.
## We will copy/paste rather than export it (and so not rely on
## graphics for it).  The long-term plan is to reimplement the
## underlying density calculations, at which point this should be
## revamped.

.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x)
{
    if (!("KernSmooth" %in% loadedNamespaces())) {
        ns <- try(loadNamespace("KernSmooth"))
        if (isNamespace(ns))
            message("(loaded the KernSmooth namespace)")
        else stop("panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace")
    }
    if (length(nbin) == 1)
        nbin <- c(nbin, nbin)
    if (!is.numeric(nbin) || (length(nbin)!=2)) stop("'nbin' must be numeric of length 1 or 2")
    if (missing(bandwidth)) {
        bandwidth <- diff(apply(x, 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)) / 25
    } else {
        if(!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
    }
    bandwidth[bandwidth==0] <- 1
    ## create density map
    if(missing(range.x))
        rv <- KernSmooth::bkde2D(x, gridsize=nbin, bandwidth=bandwidth)
    else
        rv <- KernSmooth::bkde2D(x, gridsize=nbin, bandwidth=bandwidth, range.x=range.x) 
    rv$bandwidth <- bandwidth
    return(rv)
}


panel.smoothScatter <-
    function (x, y = NULL,
              nbin = 64,
              cuts = 255,
              bandwidth,
              colramp,
              nrpoints = 100,
              transformation = function(x) x^0.25,
              pch = ".",
              cex = 1, col="black",
              range.x,
              ...,
              raster = FALSE,
              subscripts,
              identifier = "smoothScatter")

    ## subscripts is ignored (and recomputed), but the formal argument
    ## is necessary to catch another version passed in as ...

    ## If time permits, replacing the call to panel.levelplot by
    ## panel.rect might make the code more transparent (which would
    ## also make the subscripts thing unnecessary).  The only drawback
    ## I can think of is that we will no longer get contourplots
    ## instead of levelplots, but I don't think this is the right
    ## place for that anyway.

{
    if (missing(colramp))
        colramp <- colorRampPalette(c("white", "#F7FBFF", "#DEEBF7",
                                      "#C6DBEF", "#9ECAE1", "#6BAED6",
                                      "#4292C6", "#2171B5", "#08519C",
                                      "#08306B"))
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (!is.numeric(nrpoints) | (nrpoints < 0) | (length(nrpoints) != 1))
        stop("'nrpoints' should be numeric scalar with value >= 0.")
    xy <- xy.coords(x, y)
    x <- cbind(xy$x, xy$y)[!(is.na(xy$x) | is.na(xy$y)), , drop = FALSE]
    if (nrow(x) < 1) return()
    map <- .smoothScatterCalcDensity(x, nbin, bandwidth, range.x)
    xm <- map$x1
    ym <- map$x2
    dens <- map$fhat
    dens <- array(transformation(dens), dim = dim(dens))
    PFUN <- if (raster) panel.levelplot.raster else panel.levelplot
    PFUN(x = rep(xm, length(ym)),
         y = rep(ym, each = length(xm)),
         z = as.numeric(dens),
         subscripts = TRUE,
         at = seq(from = 0, to = 1.01 * max(dens), length = cuts + 2),
         col.regions = colramp(cuts + 1),
         ...,
         identifier = identifier)
    if (nrpoints != 0)
    {
        stopifnot(length(xm) == nrow(dens), length(ym) == ncol(dens))
        ixm <- round((x[, 1] - xm[1])/(xm[length(xm)] - xm[1]) *
                     (length(xm) - 1))
        iym <- round((x[, 2] - ym[1])/(ym[length(ym)] - ym[1]) *
                     (length(ym) - 1))
        idens <- dens[1 + iym * length(xm) + ixm]
        nrpoints <- min(nrow(x), ceiling(nrpoints))
        sel <- order(idens, decreasing = FALSE)[1:nrpoints]
        panel.points(x[sel, 1:2], pch = pch, cex = cex, col = col,
                     identifier = identifier)
    }
}

