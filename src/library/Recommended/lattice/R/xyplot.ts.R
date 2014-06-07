
## overplot groups, but use default plot.* style rather than superpose.* style
panel.superpose.plain <-
    function(..., col = NA, col.line = plot.line$col, col.symbol = plot.symbol$col, 
             pch = plot.symbol$pch, cex = plot.symbol$cex, fill = plot.symbol$fill, 
             font = plot.symbol$font, fontface = plot.symbol$fontface, 
             fontfamily = plot.symbol$fontfamily, lty = plot.line$lty, 
             lwd = plot.line$lwd, alpha = plot.symbol$alpha)
{
    plot.line <- trellis.par.get("plot.line")
    plot.symbol <- trellis.par.get("plot.symbol")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
        if (missing(col.symbol)) 
            col.symbol <- col
    }
    panel.superpose(..., col.line = col.line, col.symbol = col.symbol,
                    pch = pch, cex = cex, fill = fill, font = font,
                    fontfamily = fontfamily, lty = lty, lwd = lwd, alpha = alpha)
}

xyplot.ts <-
    function(x, data = NULL,
             screens = if (superpose) 1 else colnames(x),
             ...,
             superpose = FALSE, ## not used directly
             cut = FALSE,
             type = "l",
             col = NULL,
             lty = NULL,
             lwd = NULL,
             pch = NULL,
             cex = NULL,
             fill = NULL,
             auto.key = superpose,
             panel = if (superpose) "panel.superpose"
                     else "panel.superpose.plain",
             par.settings = list(),
             layout = NULL, as.table = TRUE,
             xlab = "Time", ylab = NULL,
             default.scales = list(y = list(relation =
                 if (missing(cut)) "free" else "same")))
{
    ## fix up some default settings;
    ## these are too obscure to put in as default arguments
    if (NCOL(x) == 1) {
        ## only one series, so use the more standard "panel.superpose"
        if (missing(superpose)) superpose <- TRUE
        if (missing(auto.key)) auto.key <- FALSE
    }
    stopifnot(is.null(data))
    timex <- time(x)
    x <- as.matrix(x)
    if (is.null(colnames(x)))
        colnames(x) <- paste("V", seq_len(NCOL(x)), sep = "")
    cn <- colnames(x)

    ## set up shingle for cut-and-stack plot
    ## (may not work well with irregular series)
    time <- NULL
    if (is.numeric(cut)) cut <- list(number = cut)
    if (isTRUE(cut)) {
        ## calculate optimum aspect ratio using banking (as for aspect = "xy")
        timediff <- diff(timex)
        asp <- apply(x, 2, function(y)
                     banking(timediff, diff(y)) * diff(range(y)) / diff(range(timex))
                     )
        asp <- median(asp)
        ## work out aspect ratio of n panels in vertical layout on a square
        nasp <- 1 / (1:6)
        ## choose number of cuts so that the "xy" aspect matches layout
        number <- which.min(abs(1 - (asp * 1:6) / nasp))
        cut <- list(number = number)
        if (number == 1) cut <- FALSE
    }
    if (is.list(cut))
    {
        ecargs <- list(x = timex)
        ecargs <- modifyList(ecargs, cut)
        time <- do.call(equal.count, ecargs)
        default.scales <-
            modifyList(list(x = list(relation = "sliced")),
                       default.scales)
    }

    ## 'screens' defines which panels to draw series in
    ## if 'screens' was given as a readable value then always show strips
    screensgiven <- !missing(screens) && !is.numeric(screens)
    screens <- make.par.list(cn, screens, NROW(x), NCOL(x), 1)
    screens <- unlist(screens, use.names = FALSE)
    screens <- factor(screens, levels = unique(screens))
    screens <- rep(screens, length = NCOL(x))
    fac <- factor(rep(screens, each = NROW(x)))

    ## formula
    tt <- rep(timex, NCOL(x))
    fo <- if ((nlevels(fac) > 1) || screensgiven) {
        if (!is.null(time))
            x ~ tt | time * fac
        else
            x ~ tt | fac
    } else {
        if (!is.null(time))
            x ~ tt | time
        else
            x ~ tt
    }

    if (is.null(layout)) {
        npanels <- max(1, nlevels(fac)) * max(1, nlevels(time))
        nc <- ceiling(npanels/6)
        nr <- ceiling(npanels/nc)
        layout <- c(nc, nr)
    }

    ## set lines, not points, as default for key
    if (is.logical(auto.key) && auto.key) auto.key <- list()
    if (is.list(auto.key))
        auto.key <-
            modifyList(list(lines = TRUE, points = FALSE), auto.key)

    ## The original approach (e.g. in 0.18-4) was to update par.settings
    ## with the col, lty etc for each series, and let this be picked up by
    ## panel.superpose as well as auto.key.
    ##
    ## However, that causes problems: further drawing in the panels
    ## (e.g. with layer()) can appear to be broken because it uses the same
    ## par.settings, and this has a constant color for all superposed series
    ## by default.
    ##
    ## So now we avoid updating par.settings unless necessary for auto.key:

    needStyles <-
        (any(sapply(list(col, lty, lwd, pch, cex, fill), length) > 1))
    
    ## If all series are plotted in separate panels AND
    ## all have the same style, then we don't need 'groups'.
    ## (this avoids using spurious styles when 'panel' is overridden)
    needGroups <-
        ((length(unique(screens)) < NCOL(x)) ||
         (needStyles) ||
         (is.list(type) && (length(type) > 1)))

    if (needGroups)
        groups <- factor(col(x), labels = cn)
    else groups <- rep(factor(1), length(x))

    if (is.list(type))
        type <- make.par.list(cn, type, NROW(x), NCOL(x), "l")

    tmpcall <-
        quote(xyplot(fo, groups = groups,
               ..., panel = panel,
               type = type, distribute.type = is.list(type),
               auto.key = auto.key, par.settings = par.settings,
               layout = layout, as.table = as.table,
               xlab = xlab, ylab = ylab,
               default.scales = default.scales))

    ## Include style arguments (col, lty, etc) in the call only if specified.
    ## (Originally they were not, and were picked up only from par.settings)

    if (length(par.settings) > 0) {
        ## apply settings here before we look up plot.line etc
        opar <- trellis.par.get()
        trellis.par.set(par.settings)
        on.exit(trellis.par.set(opar))
    }
    plot.line <- trellis.par.get("plot.line")
    plot.symbol <- trellis.par.get("plot.symbol")

    unlistIfSimple <- function(z)
        if (all(sapply(z, length) == 1)) unlist(z) else z
    
    if (!is.null(col)) {
        col <- make.par.list(cn, col, NROW(x), NCOL(x), plot.line$col)
        tmpcall$col <- unlistIfSimple(col)
    }
    if (!is.null(lty)) {
        lty <- make.par.list(cn, lty, NROW(x), NCOL(x), plot.line$lty)
        tmpcall$lty <- unlistIfSimple(lty)
    }
    if (!is.null(lwd)) {
        lwd <- make.par.list(cn, lwd, NROW(x), NCOL(x), plot.line$lwd)
        tmpcall$lwd <- unlistIfSimple(lwd)
    }
    if (!is.null(pch)) {
        pch <- make.par.list(cn, pch, NROW(x), NCOL(x), plot.symbol$pch)
        tmpcall$pch <- unlistIfSimple(pch)
    }
    if (!is.null(cex)) {
        cex <- make.par.list(cn, cex, NROW(x), NCOL(x), plot.symbol$cex)
        tmpcall$cex <- unlistIfSimple(cex)
    }
    if (!is.null(fill)) {
        fill <- make.par.list(cn, fill, NROW(x), NCOL(x), plot.symbol$fill)
        tmpcall$fill <- unlistIfSimple(fill)
    }

    if (needStyles) {
        ## set 'superpose' styles to be picked up by auto.key
        if (identical(panel, "panel.superpose.plain")) {
            ## (one alternative would be to define a new simplePlainKey(),
            ##  but that would not work if the user called update(auto.key=))
            if (is.null(col)) col <- plot.line$col
            if (is.null(lty)) lty <- plot.line$lty
            if (is.null(lwd)) lwd <- plot.line$lwd
            if (is.null(pch)) pch <- plot.symbol$pch
            if (is.null(cex)) cex <- plot.symbol$cex
            if (is.null(fill)) fill <- plot.symbol$fill
        }
        if (!is.null(col)) {
            par.settings <-
                modifyList(list(superpose.line = list(col = unlist(col)),
                                superpose.symbol = list(col = unlist(col))),
                           par.settings)
        }
        if (!is.null(lty)) {
            par.settings <-
                modifyList(list(superpose.line = list(lty = unlist(lty))),
                           par.settings)
        }
        if (!is.null(lwd)) {
            par.settings <-
                modifyList(list(superpose.line = list(lwd = unlist(lwd))),
                           par.settings)
        }
        if (!is.null(pch)) {
            par.settings <-
                modifyList(list(superpose.symbol = list(pch = unlist(pch))),
                           par.settings)
        }
        if (!is.null(cex)) {
            par.settings <-
                modifyList(list(superpose.symbol = list(cex = unlist(cex))),
                           par.settings)
        }
        if (!is.null(fill)) {
            par.settings <-
                modifyList(list(superpose.symbol = list(fill = unlist(fill))),
                           par.settings)
        }
    }

    obj <- eval(tmpcall)

    obj$call <- sys.call(sys.parent()); obj$call[[1]] <- quote(xyplot)
    obj
}

## COPIED FROM ZOO
## http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/zoo/R/plot.zoo.R?rev=609&root=zoo&view=markup

make.par.list <- function(nams, x, n, m, def, recycle = sum(unnamed) > 0) {
    ## if nams are the names of our variables and x is a parameter
    ## specification such as list(a = c(1,2), c(3,4)) then
    ## create a new list which uses the named variables from x
    ## and assigns the unnamed in order.  For the remaining variables
    ## assign them the default value if recycle = FALSE or recycle the
    ## unnamed variables if recycle = TRUE.  The default value for
    ## recycle is TRUE if there is at least one unnamed variable
    ## in x and is false if there are only named variables in x.
    ## n is the length of the series and m is the total number of series
    ## It only needs to know whether m is 1 or greater than m.
    ## def is the default value used when recycle = FALSE
    ## recycle = TRUE means recycle unspecified values
    ## recycle = FALSE means replace values for unspecified series with def
    ## Within series recycling is done even if recycle=FALSE.

    ## Should we allow arbirary names in 1d case?
    ## if (m > 1) stopifnot(all(names(x) %in% c("", nams)))
    if (!is.list(x)) x <- if (m == 1) list(x) else as.list(x)
    y <- vector(mode = "list", length = length(nams))
    names(y) <- nams
    in.x <- nams %in% names(x)
    unnamed <- if (is.null(names(x))) rep(TRUE, length(x)) else names(x) == ""
    if (!recycle) y[] <- def
    y[in.x] <- x[nams[in.x]]
    if (recycle) {
        stopifnot(sum(unnamed) > 0)
        y[!in.x] <- rep(x[unnamed], length.out = sum(!in.x)) ## CHECK, this was: x[unnamed]
    } else {
        y[which(!in.x)[seq_len(sum(unnamed))]] <- x[unnamed]
    }
    lapply(y, function(y) if (length(y)==1) y else rep(y, length.out = n))
}
