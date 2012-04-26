


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



prepanel.default.qq <-
    function(x, y, ...)
{
    ## if (!is.numeric(x)) x <- as.numeric(x)
    ## if (!is.numeric(y)) y <- as.numeric(y)
    list(xlim = scale.limits(c(x, y)), # range(x, y),
         ylim = scale.limits(c(x, y)), # range(x, y),
         dx = 1,
         dy = 1)
}




panel.qq <-
    function(...,
             identifier = "qq")
{
    reference.line <- trellis.par.get("reference.line")
    panel.abline(0,1,
                 col = reference.line$col,
                 lty = reference.line$lty,
                 lwd = reference.line$lwd,
                 identifier = paste(identifier, "abline", sep = "."))
    panel.xyplot(...,
                 identifier = identifier)

}



qq <- function(x, data, ...)  UseMethod("qq")


qq.formula <-
    function(x,
             data = NULL,
             aspect = "fill",
             panel = lattice.getOption("panel.qq"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             f.value = NULL,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             qtype = 7,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.qq"),
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
                            groups = groups, subscripts = TRUE,
                            drop = drop.unused.levels)

    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if(subscripts) subscr <- form$subscr
    cond <- form$condition
    y <- form$left
    x <- form$right
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }

    ##x <- as.numeric(x)
    y <- as.factorOrShingle(y)
    is.f.y <- is.factor(y)
    num.l.y <- nlevels(y)
    if (num.l.y!=2) stop("y must have exactly 2 levels")

    if(missing(xlab)) xlab <-
        if (is.f.y) unique(levels(y))[1]
        else paste("y:", as.character(unique(levels(y)[[1]])))
    
    if(missing(ylab)) ylab <-
        if (is.f.y) unique(levels(y))[2]
        else paste("y:", as.character(unique(levels(y)[[2]])))

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
                       xlab.default =
                       if (is.f.y) unique(levels(y))[1]
                       else paste("y:", as.character(unique(levels(y)[[1]]))),

                       ylab.default =
                       if (is.f.y) unique(levels(y))[y]
                       else paste("y:", as.character(unique(levels(y)[[2]]))),
                       lattice.options = lattice.options),
                  dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(qq)

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

    ## Step 4: Decide if log scales are being used: completed later

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog)
    {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)
        ## x <- log(x, xbase)  later, in panel.args
        if (have.xlim) xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog)
    {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)
        ## y <- log(y, ybase)
        if (have.ylim) ylim <- logLimits(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <- dots
    if (subscripts) foo$panel.args.common$groups <- groups

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

        if (any(id))
        {
            if (is.f.y)
            {
                tx <- x[id]
                ty <- as.numeric(y[id])
                x.val <- tx[ty==1]
                y.val <- tx[ty==2]
            }
            else {
                tx <- x[id]
                ty <- y[id]
                ly <- levels(y)
                x.val <- tx[ty>=ly[[1]][1] & ty <=ly[[1]][2]]
                y.val <- tx[ty>=ly[[2]][1] & ty <=ly[[2]][2]]
            }
            n <- max(length(x.val), length(y.val))
            ## changed from S-PLUS, where f.value = ppoints is default
            p  <-
                if (is.null(f.value))
                {
                    if (n == 1) 0.5 else ppoints(n, a = 1)
                }
                else if (is.numeric(f.value)) f.value
                else f.value(n)
            foo$panel.args[[packet.number]] <-
                list(x =
                     quantile(x = x.val, probs = p, # was fast.quantile 
                                   type = qtype, na.rm = TRUE),
                     y =
                     quantile(x = y.val, probs = p, # was fast.quantile 
                                   type = qtype, na.rm = TRUE))
        }
        else
            foo$panel.args[[packet.number]] <-
                list(x = numeric(0), y = numeric(0))

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

    class(foo) <- "trellis"
    foo
}



