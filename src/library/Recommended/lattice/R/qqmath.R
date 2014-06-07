
### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA




## FIXME: should be retired (quantile is good enough now, but check first)

fast.quantile <- 
    function(x,
             probs = seq(0, 1, 0.25),
             na.rm = FALSE,
             names = FALSE,
             type = 7, ...) 
{
    if (na.rm) 
        x <- x[!is.na(x)]
    else if (any(is.na(x))) 
        stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    if (any((p.ok <- !is.na(probs)) & (probs < 0 | probs > 1))) 
        stop("'probs' outside [0,1]")
    n <- length(x)
    if (na.p <- any(!p.ok)) {
        o.pr <- probs
        probs <- probs[p.ok]
    }
    np <- length(probs)
    if (n > 0 && np > 0) {
        if (type == 7) {
            index <- 1 + (n - 1) * probs
            lo <- floor(index)
            hi <- ceiling(index)
            x <- sort(x)
            i <- index > lo
            qs <- x[lo]
            i <- seq_along(i)[i & !is.na(i)]
            h <- (index - lo)[i]
            qs[i] <- ifelse(h == 0, qs[i], (1 - h) * qs[i] + 
                h * x[hi[i]])
        }
        else {
            if (type <= 3) {
                nppm <- if (type == 3) 
                  n * probs - 0.5
                else n * probs
                j <- floor(nppm)
                switch(type, h <- ifelse(nppm > j, 1, 0), h <- ifelse(nppm > 
                  j, 1, 0.5), h <- ifelse((nppm == j) & ((j%%2) == 
                  0), 0, 1))
            }
            else {
                switch(type - 3, {
                  a <- 0
                  b <- 1
                }, a <- b <- 0.5, a <- b <- 0, a <- b <- 1, a <- b <- 1/3, 
                  a <- b <- 3/8)
                fuzz <- 4 * .Machine$double.eps
                nppm <- a + probs * (n + 1 - a - b)
                j <- floor(nppm + fuzz)
                h <- nppm - j
                h <- ifelse(abs(h) < fuzz, 0, h)
            }
            x <- sort(x)
            x <- c(x[1], x[1], x, x[n], x[n])
            qs <- ifelse(h == 0, x[j + 2], ifelse(h == 1, x[j + 
                3], (1 - h) * x[j + 2] + h * x[j + 3]))
        }
    }
    else {
        qs <- rep(NA_real_, np)
    }
    if (names && np > 0) {
        dig <- max(2, getOption("digits"))
        names(qs) <- paste(if (np < 100) 
            formatC(100 * probs, format = "fg", width = 1, digits = dig)
        else format(100 * probs, trim = TRUE, digits = dig), 
            "%", sep = "")
    }
    if (na.p) {
        o.pr[p.ok] <- qs
        names(o.pr) <- rep("", length(o.pr))
        names(o.pr)[p.ok] <- names(qs)
        o.pr
    }
    else qs
}





prepanel.default.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL,
             subscripts, ...,
             tails.n = 0)
{
    if (!is.numeric(x)) x <- as.numeric(x) # FIXME: dates?
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    ## if plotting tails, do prepanel as for raw data:
    if (tails.n > 0)
        f.value <- NULL
    getxx <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            distribution(ppoints(nobs))
        else if (is.numeric(f.value))
            distribution(f.value)
        else
            distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            sort(x)
        else if (is.numeric(f.value))
            quantile(x, f.value,       # was fast.quantile 
                     names = FALSE,
                     type = qtype,
                     na.rm = TRUE)
        else
            quantile(x, f.value(nobs), # was fast.quantile
                     names = FALSE,
                     type = qtype,
                     na.rm = TRUE)
    }
    if (!nobs)
        prepanel.null()
    else if (!is.null(groups))
    {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        list(xlim = range(unlist(xxlist), finite = TRUE),
             ylim = range(unlist(yylist), finite = TRUE),
             dx = unlist(lapply(xxlist, diff)),
             dy = unlist(lapply(yylist, diff)))
    }
    else 
    {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        list(xlim = scale.limits(xx), # range(xx, finite = TRUE),
             ylim = scale.limits(yy), # range(yy, finite = TRUE),
             dx = diff(xx),
             dy = diff(yy))
    }
}





panel.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL, ...,
             tails.n = 0,
             identifier = "qqmath")
{
    x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    if (!is.null(groups))
        panel.superpose(x, y = NULL,
                        f.value = f.value,
                        distribution = distribution,
                        qtype = qtype,
                        groups = groups,
                        panel.groups = panel.qqmath,
                        ...,
                        tails.n = tails.n)
    else if (nobs)
    {
        if (is.null(f.value)) # exact data instead of quantiles
        {
            panel.xyplot(x = distribution(ppoints(nobs)),
                         y = sort(x),
                         ...,
                         identifier = identifier)
        }
        else
        {
            pp <- if (is.numeric(f.value)) f.value else f.value(nobs)
            if (tails.n > 0)
            {
                ## use exact data for tails of distribution
                tails.n <- min(tails.n, nobs %/% 2)
                ppd <- ppoints(nobs)
                ## omit probabilities within the exact tails
                pp <- pp[(pp > ppd[tails.n] &
                          pp < ppd[nobs + 1 - tails.n])]
                ## add on probs corresponding to exact tails
                pp <- c(head(ppd, tails.n), pp, tail(ppd, tails.n))
                ## must use a quantile type that recovers exact values:
                qtype <- 1
            }
            xx <- distribution(pp)
            yy <- quantile(x, pp, 
                                  names = FALSE,
                                  type = qtype,
                                  na.rm = TRUE)
            panel.xyplot(x = xx, y = yy, ...,
                         identifier = identifier)
        }
    }
}




qqmath <- function(x, data, ...) UseMethod("qqmath")



qqmath.numeric <-
    function(x, data = NULL, ylab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(qqmath)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$ylab <- ylab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::qqmath)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



qqmath.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = !is.null(groups),
             distribution = qnorm,
             f.value = NULL,
             auto.key = FALSE,
             aspect = "fill",
             panel = lattice.getOption("panel.qqmath"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.qqmath"),
             subscripts = !is.null(groups),
             subset = TRUE)
{
    formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(lattice.options))
    {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }
    
    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <-
        latticeParseFormula(formula, data, subset = subset,
                            groups = groups, multiple = allow.multiple,
                            outer = outer, subscripts = TRUE,
                            drop = drop.unused.levels)
    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr
    cond <- form$condition
    ## number.of.cond <- length(cond)
    x <- form$right
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
        ## number.of.cond <- 1
    }

    dist.name <- paste(deparse(substitute(distribution)), collapse = "")
    if (missing(xlab)) xlab <- dist.name
    if (missing(ylab)) ylab <- form$right.name

    ## create a skeleton trellis object with the
    ## less complicated components:
    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula, 
                       cond = cond,
                       aspect = aspect,
                       strip = strip,
                       panel = panel,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = dist.name,
                       ylab.default = form$right.name,
                       lattice.options = lattice.options),
                  dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(qqmath)

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:
    
    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limits))
    {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limits
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limits))
    {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limits
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log

    ## This is slightly weird because 'x' is eventually plotted in the
    ## Y-axis
    if (have.xlog)
    {
        warning("Can't have log X-scale")
        have.xlog <- FALSE
        foo$x.scales$log <- FALSE
    }
    if (have.ylog)
    {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)
        
        x <- log(x, ybase)
        if (have.ylim) ylim <- logLimits(ylim, ybase)
    }


    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(distribution = distribution,
               f.value = f.value),
          dots)

    if (subscripts)
    {
        foo$panel.args.common$groups <- groups
    }

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length))) 
        stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)


    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1)
    {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }
    cond.current.level <- rep(1, length(cond))
    for (packet.number in seq_len(npackets))
    {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <-
            list(x = x[id])
        if (subscripts)
            foo$panel.args[[packet.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    more.comp <-
        c(limits.and.aspect(default.prepanel,
                            prepanel = prepanel, 
                            have.xlim = have.xlim, xlim = xlim, 
                            have.ylim = have.ylim, ylim = ylim, 
                            x.relation = foo$x.scales$relation,
                            y.relation = foo$y.scales$relation,
                            panel.args.common = foo$panel.args.common,
                            panel.args = foo$panel.args,
                            aspect = aspect,
                            npackets = npackets,
                            x.axs = foo$x.scales$axs,
                            y.axs = foo$y.scales$axs),
          cond.orders(foo))
    foo[names(more.comp)] <- more.comp

    if (is.null(foo$legend) && needAutoKey(auto.key, groups))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      updateList(list(text = levels(as.factor(groups)),
                                      points = TRUE,
                                      rectangles = FALSE,
                                      lines = FALSE),
                                 if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <- 
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }
    class(foo) <- "trellis"
    foo
}














##############################################


panel.qqmathline <-
    function(x, y = x,
             distribution = qnorm,
             probs = c(0.25, 0.75),
             qtype = 7,
             groups = NULL, 
             ...,
             identifier = "qqmathline")
{
    y <- as.numeric(y)
    stopifnot(length(probs) == 2)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(y))
    if (!is.null(groups))
        panel.superpose(x = y, y = NULL,
                        distribution = distribution,
                        probs = probs,
                        qtype = qtype,
                        groups = groups,
                        panel.groups = panel.qqmathline,
                        ...)
    else if (nobs > 0)
    {
        yy <-
            quantile(y, probs, names = FALSE, # was fast.quantile 
                     type = qtype, na.rm = TRUE)
        xx <- distribution(probs)
        r <- diff(yy)/diff(xx)
        panel.abline(c( yy[1]-xx[1]*r , r), ...,
                     identifier = identifier)
    }
}


prepanel.qqmathline <-
    function(x, y = x,
             distribution = qnorm,
             probs = c(0.25, 0.75),
             qtype = 7,
             groups = NULL,
             subscripts = TRUE,
             ...)
{
    ans <-
        prepanel.default.qqmath(x,
                                distribution = distribution,
                                qtype = qtype,
                                groups = groups,
                                subscripts = subscripts,
                                ...)
    y <- as.numeric(y)
    stopifnot(length(probs) == 2)
    distribution <- getFunctionOrName(distribution)
    getdy <- function(x)
    {
        diff(quantile(x, probs, names = FALSE, # was fast.quantile 
                      type = qtype,
                      na.rm = TRUE))
    }
    dy <- 
        if (!is.null(groups)) sapply(split(y, groups[subscripts]), getdy)
        else getdy(y)
    if (!all(is.na(dy)))
    {
        ans$dy <- dy[!is.na(dy)]
        ans$dx <- rep(diff(distribution(probs)), length(ans$dy))
    }
    ans
}








