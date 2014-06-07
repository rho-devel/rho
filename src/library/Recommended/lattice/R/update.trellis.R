


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

## retrieve trellis object saved during plotting

trellis.last.object <- function(..., prefix = lattice.getStatus("current.prefix"))
{
    ## if (warn && (!lattice.getStatus("current.plot.saved", prefix = prefix)))
    ##     warning("Requested object was not saved")
    if (!lattice.getStatus("current.plot.saved", prefix = prefix))
    {
        warning("Requested 'trellis' object was not saved")
        return(invisible(NULL))
    }
    ans <- lattice.getStatus("last.object", prefix = prefix)
    ## FIXME: remove, as should not be needed any more
    ## if (is.null(ans)) { 
    ##     warning("Could not retrieve saved trellis object")
    ##     return(invisible())
    ## }
    update(ans, ...)
}


 
## Not all arguments to xyplot etc can be supplied to
## update.trellis. Generally speaking, anything that needs to change
## the data within each panel is a no-no. Everything else is
## technically game, though implementation might be
## problematic. Here's a list of arguments that should work (list
## currently based on xyplot, may need to be updated later)


##             panel
##             aspect = "fill",
##             as.table = FALSE,
##             between,
##             key,
##             auto.key = FALSE,
##             layout,
##             main,
##             page,
##             par.strip.text,
##             prepanel,

## scales, one of the problematic ones

##             skip,
##             strip,
##             strip.left,
##             sub,
##             xlab,
##             xlim,
##             ylab,
##             ylim,
##             par.settings,



## ...,  these should probably be added to the list of common panel arguments



## There is also the possibility of some update arguments that may not
## necessarily be valid arguments to xyplot etc (although we might
## change that). Currently these would be the perm and index arguments
## controlling reordering of conditioning variables and their levels.




update.trellis <-
    function(object,
             panel,
             aspect,
             as.table,
             between,
             key,
             auto.key,
             legend,
             layout,
             main,
             page,
             par.strip.text,
             prepanel,
             scales, #one of the problematic ones
             skip,
             strip,
             strip.left,
             sub,
             xlab,
             ylab,
             xlab.top,
             ylab.right,
             xlim,
             ylim,
             xscale.components,
             yscale.components,
             axis,

             par.settings,
             plot.args,
             lattice.options,

             index.cond,
             perm.cond,

             ...)

{
    ## modify call to reflect update
    upcall <- match.call()
    nm <- names(upcall)
    if (!is.null(nm))
    {
        nm <- nm[nm != "" & nm != "object"]
        if (length(nm) == 0)
        {
            ## FIXME: drop this message before release
            ## cat("nothing to update with")
            return(object)
        }
        object$call[nm] <- upcall[nm]
    }
    have.xlim <- !missing(xlim)    ## needed later
    have.ylim <- !missing(ylim)
    have.axs <- FALSE

    ## deal with the non-problematic stuff first

    if (!missing(as.table))
    {
        if (is.logical(as.table)) object$as.table <- as.table
        else warning("Inappropriate value of 'as.table'")
    }
    if (!missing(between))
    {
        if ("x" %in% names(between)) object$x.between <- between$x
        if ("y" %in% names(between)) object$y.between <- between$y
    }
    if (!missing(layout))
    {
        object$layout <- layout
    }
    if (!missing(main)) object$main <- main
    if (!missing(sub)) object$sub <- sub
    if (!missing(xlab)) object$xlab <- xlab
    if (!missing(ylab)) object$ylab <- ylab
    if (!missing(xlab.top)) object$xlab.top <- xlab.top
    if (!missing(ylab.right)) object$ylab.right <- ylab.right
    if (!missing(page)) object$page <- page
    if (!missing(xscale.components)) object$xscale.components <- xscale.components
    if (!missing(yscale.components)) object$yscale.components <- yscale.components
    if (!missing(axis)) object$axis <- axis
    if (!missing(par.strip.text))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly

        if (is.list(par.strip.text))
            object$par.strip.text <- updateList(object$par.strip.text, par.strip.text)
        else warning("'par.strip.text' must be a list")
    }
    if (!missing(skip)) object$skip <- skip
    if (!missing(strip))
    {
        if (is.logical(strip)) {
            if (strip) object$strip <- strip.default
            else object$strip <- FALSE
        }
        else object$strip <- strip
    }
    if (!missing(strip.left))
    {
        if (is.logical(strip.left)) {
            if (strip.left) object$strip.left <- strip.custom(horizontal = FALSE)
            else object$strip.left <- FALSE
        }
        else object$strip.left <- strip.left
    }
    if (!missing(par.settings))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly
        if (is.list(par.settings))
            object$par.settings <- updateList(object$par.settings, par.settings)
        else warning("'par.settings' must be a list")
    }
    if (!missing(plot.args))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly
        if (is.list(plot.args))
            object$plot.args <- updateList(object$plot.args, plot.args)
        else warning("'plot.args' must be a list")
    }
    if (!missing(lattice.options))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly
        if (is.list(lattice.options))
            object$lattice.options <- updateList(object$lattice.options, lattice.options)
        else warning("'lattice.options' must be a list")
    }

    ## during construction of trellis objects, perm.cond and
    ## index.cond are calculated by the cond.orders function. We could
    ## do that here as well, but the perm.cond is really too trivial
    ## to bother. cond.orders() is called is index.cond is
    ## non-missing, and then it becomes important that perm.cond is
    ## processed first (in case it it non-missing as well).

    if (!missing(perm.cond))
    {
        if (is.null(perm.cond))
            object$perm.cond <- seq_len(length(object$condlevels))
        else if (all(sort(perm.cond) == object$perm.cond))
            object$perm.cond <- perm.cond
        else stop("Invalid value of 'perm.cond'")
    }
    if (!missing(index.cond))
    {
        object$index.cond <- index.cond
        cond.ord <- cond.orders(object)
        object[names(cond.ord)] <- cond.ord
    }
    dots <- list(...)
    if (length(dots) > 0)
    {
        ##print(dots) ## for debugging, remove later
        object$panel.args.common <- updateList(object$panel.args.common, dots)
    }
    if (!missing(panel))
    {
        panel <- getFunctionOrName(panel)
        ## if (as.character(object$call[[1]]) == "splom")
        if ("panel" %in% names(object$panel.args.common))
            object$panel.args.common$panel <- panel
        else object$panel <- panel
    }

    ## the slightly complicated stuff

    if (!missing(legend))
    {
        if (is.null(legend)) object$legend <- NULL
        else
        {
            ## Used to be:
            ## object$legend <- updateList(object$legend, legend)
            ## but that does not handle multiple components with same
            ## name, which can happen when there are multiple 'inside'
            ## components (lattice bug #2587).

            ## What should happen in that case?  If there is one or
            ## more 'inside' in old, but none in new, retain all in
            ## the old.  If there is one in the replacement, and one
            ## in the original, then the new should replace the old.
            ## If there are multiple, remove all in the old and retain
            ## all in the new. [One might argue that there should be
            ## the option of retaining the old, but in that case the
            ## user could add that manually to the new 'legend'. ]

            winside <- which(names(legend) == "inside")
            if (length(winside) ==  0L) # none in new
                object$legend <- updateList(object$legend, legend)
            else 
            {
                object$legend <- object$legend[ names(object$legend) != "inside" ]
                new.insides <- legend[winside]
                legend <- legend[-winside]
                object$legend <- updateList(object$legend, legend)
                object$legend <- c(object$legend, new.insides)
            }
        }
        
    }
    if (!missing(key))
    {
        object$legend <-
            updateList(object$legend, 
                       construct.legend(legend = NULL, key = key))
    }
    if (!missing(auto.key))
    {
        if (!is.null(object$legend))
            message("Note: 'auto.key' ignored since legend already present.\nUse 'update(..., legend = NULL)' to remove existing legend(s)")
        else 
        {
            groups <- object$panel.args.common$groups
            if (needAutoKey(auto.key, groups))
            {
                object$legend <-
                    list(list(fun = "drawSimpleKey",
                              args =
                              updateList(list(text = levels(as.factor(groups))), 
                                         if (is.list(auto.key)) auto.key else list())))
                object$legend[[1]]$x <- object$legend[[1]]$args$x
                object$legend[[1]]$y <- object$legend[[1]]$args$y
                object$legend[[1]]$corner <- object$legend[[1]]$args$corner
                names(object$legend) <- 
                    if (any(c("x", "y", "corner") %in% names(object$legend[[1]]$args)))
                        "inside"
                    else
                        "top"
                if (!is.null(object$legend[[1]]$args$space))
                    names(object$legend) <- object$legend[[1]]$args$space
            }
        }
    }

    relationChanged <- FALSE

    if (!missing(scales))
        ## FIXME: this needs special handling for cloud, but leave that for later
    {
        if (is.character(scales)) scales <- list(relation = scales)
        xscales <- scales$x
        yscales <- scales$y
        zscales <- scales$z
        scales$x <- NULL
        scales$y <- NULL
        scales$z <- NULL
        if (is.character(xscales)) xscales <- list(relation = xscales)
        if (is.character(yscales)) yscales <- list(relation = yscales)
        if (is.character(zscales)) zscales <- list(relation = zscales)

        if (!is.null(scales$log) || !is.null(xscales$log) || !is.null(yscales$log) || !is.null(zscales$log))
        {
            warning("log scales cannot be changed via 'update'")
            scales$log <- NULL
            xscales$log <- NULL
            yscales$log <- NULL
            zscales$log <- NULL
        }

        if (!is.null(scales$axs) || !is.null(xscales$axs) || !is.null(yscales$axs))
            have.axs <- TRUE

        if (is.logical(scales$alternating)) scales$alternating <- if (scales$alternating) c(1,2) else 1
        if (is.logical(xscales$alternating)) xscales$alternating <- if (xscales$alternating) c(1,2) else 1
        if (is.logical(yscales$alternating)) yscales$alternating <- if (yscales$alternating) c(1,2) else 1
        ## cannot possibly make sense for z

        rep2IfNotNULL <- function(x) 
        {
            if (is.null(x)) NULL else rep(x, length.out = 2)
        }
        for (nm in c("tck", "cex", "rot"))
        {
            scales[[nm]] <- rep2IfNotNULL(scales[[nm]])
            xscales[[nm]] <- rep2IfNotNULL(xscales[[nm]])
            yscales[[nm]] <- rep2IfNotNULL(yscales[[nm]])
            zscales[[nm]] <- rep2IfNotNULL(zscales[[nm]])
        }

        if (!is.null(scales$limits))
        {
            have.xlim <- TRUE
            have.ylim <- TRUE
            ##have.zlim <- TRUE
            xlim <- scales$limits
            ylim <- scales$limits
            ##zlim <- scales$limits
        }

        if (!is.null(xscales$limits))
        {
            have.xlim <- TRUE
            xlim <- xscales$limits
        }
        if (!is.null(yscales$limits))
        {
            have.ylim <- TRUE
            ylim <- yscales$limits
        }

        if (!is.null(scales$relation) || !is.null(xscales$relation) || !is.null(yscales$relation))
            relationChanged <- TRUE

        object$x.scales[names(scales)] <- scales
        object$y.scales[names(scales)] <- scales
        object$z.scales[names(scales)] <- scales

        object$x.scales[names(xscales)] <- xscales
        object$y.scales[names(yscales)] <- yscales
        object$z.scales[names(zscales)] <- zscales

        if (object$x.scales$relation == "same" && (is.list(object$x.scales$at) || is.list(object$x.scales$labels)))
            stop("the at and labels components of scales may not be lists when relation = same")
        if (object$y.scales$relation == "same" && (is.list(object$y.scales$at) || is.list(object$y.scales$labels)))
            stop("the at and labels components of scales may not be lists when relation = same")
    }

    ## difficult stuff

#             aspect
#             prepanel,
#             scales, #one of the problematic ones
#             xlim,
#             ylim,

    ## stuff that may need recalculation of limits and aspect ratio

    recalculateLimits <- have.xlim || have.ylim || relationChanged || have.axs

    if (!missing(aspect))
    {
        if (is.numeric(aspect))
        {
            object$aspect.ratio <- aspect
            object$aspect.fill <- FALSE
        }
        else if (is.character(aspect))
        {
            if (aspect == "fill") object$aspect.fill <- TRUE
            else if (aspect == "xy")
            {
                object$aspect.fill <- FALSE
                object$aspect.ratio <- "xy" ## guaranteed to be modified below
                recalculateLimits <- TRUE
            }
            else if (aspect == "iso")
            {
                object$aspect.fill <- FALSE
                object$aspect.ratio <- "iso" ## guaranteed to be modified below
                recalculateLimits <- TRUE
            }
            else warning(gettextf("Unrecognized value of 'aspect': '%s'", as.character(aspect)))
        }
        else warning("Invalid value of 'aspect'")
    }

    if (!missing(prepanel))
    {
        recalculateLimits <- TRUE
        ## prepanel <- getFunctionOrName(prepanel)
    }
    else prepanel <- object$prepanel

    if (recalculateLimits)
    {
        prepanel.def <- object$prepanel.default
        laa <- limits.and.aspect(prepanel.def,
                                 prepanel = prepanel,
                                 have.xlim = have.xlim,
                                 xlim = xlim,
                                 have.ylim = have.ylim,
                                 ylim = ylim,
                                 x.relation = object$x.scales$relation,
                                 y.relation = object$y.scales$relation,
                                 panel.args.common = object$panel.args.common,
                                 panel.args = object$panel.args,
                                 aspect = object$aspect.ratio,
                                 x.axs = object$x.scales$axs,
                                 y.axs = object$y.scales$axs)
        ##...)  ## extra arguments for prepanel (for qqmathline)
        object[names(laa)] <- laa
    }
    object
}



## `subsetting': shortcut to updating index.cond


"[.trellis" <- function(x, i, j, ..., drop = FALSE)
{
    ## call update.trellis with a suitable 'index.cond' argument
    ocall <- match.call()[-2] # removes 'x'
    ocall[[1]] <- quote(base::list)
    if (!missing(drop))
    {
        if (drop) warning("'drop=TRUE' ignored")
        ocall$drop <- NULL
    }
    indices <- rep(list(TRUE), length.out = length(x$condlevels))
    if (!missing(i)) {
        indices[[1]] <- i
        ocall$i <- NULL
    }
    if (!missing(j)) {
        indices[[2]] <- j
        ocall$j <- NULL
    }
    ## set missing args in ocall to TRUE before evaluating
    if (length(ocall) > 1)
    {
        emptyArgs <-
            sapply(as.list(ocall[-1]),
                   function(x) (typeof(x) == "symbol" &&
                                as.character(x) == ""))
        ocall[1L + which(emptyArgs)] <- quote(TRUE)
        dots <- eval.parent(ocall)
        indices[-c(1, 2)] <- dots
    }
    original.levs <- lapply(sapply(x$condlevels, length), seq)
    stopifnot(length(original.levs) == length(indices))
    current.levs <-
        mapply("[", original.levs, x$index.cond,
               SIMPLIFY = FALSE)
    new.levs <-
        mapply("[", current.levs, indices,
               SIMPLIFY = FALSE)
    if (any(sapply(new.levs, function(x) any(is.na(x)))))
        stop("Invalid indices")
    update(x, index.cond = new.levs)
}



## ## Old version.  Failed with

## ## bar <- function(i) { foo[,,,i] }
## ## bar(1)

## "[.trellis" <- function(x, i, j, ..., drop = FALSE)
## {
##     ## index.cond <-
##     ocall <- match.call()
##     tmp <- as.list(ocall)[-(1:2)]
##     isj <- "j" %in% names(tmp)
##     isi <- "i" %in% names(tmp)
##     if (drop)
##     {
##         warning("'drop=TRUE' ignored")
##         tmp$drop <- NULL
##     }
##     len <-
##         if (length(dim(x)) == 1) 1
##         else length(tmp) + (1 - isj) + (1 - isi)
##     indices <- rep(list(TRUE), length.out = len)
##     if (isi)
##     {
##         indices[[1]] <- tmp$i
##         tmp <- tmp[-1]
##     }
##     if (isj)
##     {
##         indices[[2]] <- tmp$j
##         tmp <- tmp[-1]
##     }
##     if (len > 2)
##     {
##         keep <-
##             sapply(tmp,
##                    function(x) 
##                    typeof(x) == "symbol" && as.character(x) == "")
##         tmp[keep] <- list(TRUE)
##         indices[-(1:2)] <- tmp
##     }
##     indices <- lapply(indices, eval)
##     original.levs <- lapply(sapply(x$condlevels, length), seq)
##     stopifnot(length(original.levs) == len)
##     current.levs <-
##         mapply("[", original.levs, x$index.cond,
##                SIMPLIFY = FALSE)
##     new.levs <-
##         mapply("[", current.levs, indices,
##                SIMPLIFY = FALSE)
##     if (any(sapply(new.levs, function(x) any(is.na(x)))))
##         stop("Invalid indices")
##     update(x, index.cond = new.levs)
## }





t.trellis <- function(x)
{
    stopifnot(length(dim(x)) == 2)
    update(x, perm.cond = rev(x$perm.cond))
}

