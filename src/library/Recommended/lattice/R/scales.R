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


## Can't think of a way to ensure partial matching except by doing it explicitly

complete_names <- function(x, template, allow.invalid = FALSE)
{
    pid <- pmatch(names(x), names(template), duplicates.ok = TRUE)
    if (allow.invalid) {
        x <- x[!is.na(pid)]
        pid <- pid[!is.na(pid)]
    } else {
        if (any(is.na(pid)))
            warning("Invalid or ambiguous component names: ",
                     paste(names(x)[which(is.na(pid))], collapse = ", ") )
    }
    if (any(duplicated(pid))) stop("Multiple matches to component name")
    names(x) <- names(template)[pid]
    x
}

    
## FIXME: how should we handle unrecognized arguments?

construct.scales <-
    function(draw = TRUE, axs = "r", tck = 1, tick.number = 5,
             at = FALSE, labels = FALSE, log = FALSE,
             alternating = TRUE, relation = "same",
             abbreviate = FALSE, minlength = 4,
             limits = NULL, format = NULL,
             equispaced.log = TRUE,

             lty = FALSE, lwd = FALSE, cex = FALSE, rot = FALSE,
             col = FALSE, col.line = col, alpha = FALSE, alpha.line = alpha,
             font = FALSE, fontfamily = FALSE, fontface = FALSE, lineheight = FALSE,

             ...,  ## NOTE: ... is currently ignored
             x = NULL, y = NULL)
{
    ## top-level values
    x.scales <- y.scales <-
        list(draw = draw, axs = axs, tck = tck, tick.number = tick.number,
             at = at, labels = labels, log = log,
             alternating = alternating, relation = relation,
             abbreviate = abbreviate, minlength = minlength,
             limits = limits, format = format, equispaced.log = equispaced.log,
             lty = lty, lwd = lwd, cex = cex, rot = rot,
             col = col, col.line = col.line, alpha = alpha, alpha.line = alpha.line,
             font = font, fontfamily = fontfamily, fontface = fontface, lineheight = lineheight)
    ## override by component-specific values
    if (!is.null(x))
    {
        if (is.character(x)) x <- list(relation = x)
        x <- complete_names(x, x.scales)
        x.scales[names(x)] <- x
    }
    if (!is.null(y))
    {
        if (is.character(y)) y <- list(relation = y)
        y <- complete_names(y, y.scales)
        y.scales[names(y)] <- y
    }
    if (is.logical(x.scales$alternating))
        x.scales$alternating <-
            if (x.scales$alternating) c(1,2)
            else 1
    if (is.logical(y.scales$alternating))
        y.scales$alternating <-
            if (y.scales$alternating) c(1,2)
            else 1
    for (nm in c("tck", "cex", "rot")) {
        x.scales[[nm]] <- rep(x.scales[[nm]], length.out = 2)
        y.scales[[nm]] <- rep(y.scales[[nm]], length.out = 2)
    }
    if (x.scales$relation == "same" && (is.list(x.scales$at) || is.list(x.scales$labels)))
        stop("the 'at' and 'labels' components of 'scales' may not be lists when 'relation = \"same\"'")
    if (y.scales$relation == "same" && (is.list(y.scales$at) || is.list(y.scales$labels)))
        stop("the 'at' and 'labels' components of 'scales' may not be lists when 'relation = \"same\"'")
    list(x.scales = x.scales, y.scales = y.scales)
}



construct.3d.scales <-
    function(draw = TRUE,
             axs = "r",
             tck = 1,
             lty = FALSE, lwd = FALSE,
             distance = c(1,1,1),
             tick.number = 5,
             cex = FALSE,
             rot = FALSE,
             at = FALSE,
             labels = FALSE,
             col = FALSE,
             col.line = col,
             alpha = FALSE,
             alpha.line = col,
             log = FALSE,
             font = FALSE,
             fontfamily = FALSE,
             fontface = FALSE,
             lineheight = FALSE,
             arrows = TRUE,
             relation = "same",
             format = NULL,
             abbreviate = FALSE,
             minlength = 4,
             ...,
             x = NULL,
             y = NULL,
             z = NULL)
{
    x.scales <- y.scales <- z.scales <- 
        list(draw = draw, axs = axs, tck = tck,
             lty = lty, lwd = lwd,
             tick.number = tick.number,
             cex = cex, rot = rot, font = font,
             fontfamily = fontfamily, fontface = fontface,
             lineheight = lineheight,
             at = at, labels = labels,
             col = col, col.line = col.line,
             alpha = alpha, alpha.line = alpha.line,
             log = log, arrows = arrows,
             relation = relation, format = format,
             abbreviate = abbreviate, minlength = minlength)
    distance <- rep(distance, length.out = 3)
    x.scales$distance <- distance[1]
    y.scales$distance <- distance[2]
    z.scales$distance <- distance[3]
    if (!is.null(x))
    {
        if (is.character(x)) x <- list(relation = x)
        x <- complete_names(x, x.scales)
        x.scales[names(x)] <- x
    }
    if (!is.null(y))
    {
        if (is.character(y)) y <- list(relation = y)
        y <- complete_names(y, y.scales)
        y.scales[names(y)] <- y
    }
    if (!is.null(z))
    {
        if (is.character(z)) z <- list(relation = z)
        z <- complete_names(z, z.scales)
        z.scales[names(z)] <- z
    }
    list(x.scales = x.scales, y.scales = y.scales, z.scales = z.scales)
}





## FIXME: the following function uses a very unstructured patchwork of
## logic.  Need to change it to use some sort of method dispatch to
## work with different types of limits (dates, factors, etc)



limitsFromLimitlist <-
    function(have.lim,
             lim,
             relation,
             limitlist,
             used.at,
             numlimitlist,
             axs,
             npackets)
    ## have.lim: logical, whether xlim/ylim was explicitly specified
    ## lim: the specified limit if have.lim = TRUE
    ## relation: same/free/sliced
    ## limitlist: list of limits from prepanel calculations, one for each panel
    ## numlimitlist: (optional) numeric locations for factors (lim
    ##                will be levels including unused ones)
    ## axs: "r", "i" etc, passed on to extend.limits

    ## return value depends on relation. (See limits.and.aspect below,
    ## where this is used, for partial enlightenment.)
{

    if (relation == "same")
    {
        ## The problem here is that we need to figure out the overall
        ## limit required from the limits of each panel. This could be
        ## a problem for two reasons. First, some panels could have no
        ## data in them, in which case the corresponding limits would
        ## be NA. Secondly, the limits could be either numeric or
        ## character vectors (the latter for factors). When relation =
        ## same, the type should be same across panels. When numeric,
        ## we just take range, leaving out NAs. But what about
        ## factors?  Is it OK to assume that all the non-NA vectors
        ## would be exactly the same ? They should be, since levels(x)
        ## would not change even if not all levels are
        ## represented. So, I'm just taking unique of all the vectors
        ## concatenated, excluding NA's

        ## Additional complication: Need to preserve class of limits,
        ## to be used later in tick location/label calculation. Not a
        ## problem in other cases, but here unlist-ing loses the
        ## class.


        #if (!have.lim)
        ## always calculate the limits from prepanel first:

        ## should check that all classes are the same. How ? What
        ## about NA's ? Arrgh!

        ## to handle NA's, how about:

        all.na <- unlist(lapply(limitlist, function(x) all(is.na(x))))
        class.lim <- ## retain non-NA limitlists only
            lapply(limitlist[!all.na], class)
        ## class.lim is a list now, may be length 0
        limits <- unlist(limitlist) ## loses the class attribute

        ## if (length(limits) > 0)
        if (sum(!is.na(limits)) > 0)
        {
            if (is.character(limits))
            {
                limits <- unique(limits[!is.na(limits)])
                slicelen <- diff(extend.limits(limits, axs = axs))
            }
            else ## if (is.numeric(limits)) # or dates etc
            {
                limits <-
                    extend.limits(range(as.numeric(limits), finite = TRUE),
                                  axs = axs)
                slicelen <- diff(range(limits, finite = TRUE))
            }

            ## hopefully put back appropriate class of limits:
            ## FIXME: date changes may have messed this up

            if (length(class.lim) > 0)
                class(limits) <-
                    if (all(class.lim[[1]] == "integer"))
                        "numeric" else class.lim[[1]]

            ## (have to handle "integer" specially, since variable
            ## specifications like 1:10 are rather common, and
            ## class() <- "integer" would turn the limits into
            ## integers)
        }
        else
        {
            limits <- c(0,1)
            slicelen <- 1
        }

        if (have.lim)
        {
            if (is.list(lim))
                stop("limits cannot be a list when relation = same")
            old.limits <- limits
            limits <- lim
            ## lim overrides prepanel except NAs
            if (!is.character(limits) && !is.character(old.limits)) {
                limits[is.na(limits)] <- old.limits[is.na(limits)]
            }
            slicelen <-
                ## this no longer works for dates (R 2.6)
##                 if (is.numeric(lim)) diff(range(lim))
##                 else length(lim) + 2
                if (is.character(limits)) length(limits) + 2
                else diff(range(as.numeric(limits)))
        }
        ans <- list(limits = limits, slicelen = slicelen)
    }
    else if (relation == "sliced")
    {
        if (have.lim)
        {
            if (is.list(lim))
            {
                limits <- rep(lim, length.out = npackets)
            }
            else warning("Explicitly specified limits ignored")
        }
        slicelen <- limitlist
        for (i in seq_along(limitlist))
        {
            slicelen[[i]] <-
                ## if (is.numeric(limitlist[[i]]))
                if (!is.character(limitlist[[i]]))
                {
                    if (any(is.finite(limitlist[[i]])))
                        ## range unnecessary, but...
                        diff(range(as.numeric(limitlist[[i]]), finite = TRUE))
                    else NA_real_
                }
                else if (!any(is.na(numlimitlist[[i]])))
                    diff(range(as.numeric(numlimitlist[[i]])))
                else NA_real_
        }
        slicelen <-
            (if (axs == "i") 1 else 1 + 2 * lattice.getOption("axis.padding")$numeric) *
                max(unlist(slicelen), na.rm = TRUE)
        for (i in seq_along(limitlist))
        {
            if (is.numeric(limitlist[[i]]))
                limitlist[[i]] <-
                    extend.limits(limitlist[[i]], length = slicelen)
        }
        for (i in seq_along(numlimitlist))
        {
            if (!all(is.na(numlimitlist[[i]])))
                numlimitlist[[i]] <-
                    extend.limits(as.numeric(numlimitlist[[i]]), length = slicelen)
        }
        ans <-
            list(limits = limitlist,
                 used.at = used.at,
                 numlimitlist = numlimitlist,
                 slicelen = slicelen)
    }
    else if (relation == "free")
    {
        if (have.lim)
        {
            ## This is the only situation where limits can be a list
            ## (doesn't make sense when relation="same", ignored when
            ## relation="sliced").  Even if limits is not a list (but
            ## is specified), it will be treated as a list, and
            ## repeated as necessary (see further comments below).

            if (!is.list(lim)) lim <- list(lim)

            ## There's a subtle consideration here.  It is possible
            ## for some panels to have nothing in them (or only NA's).
            ## Such panels usually have their prepanel functions
            ## return NA.  When 'limits' is specified as a list, this
            ## will be interpreted as the limit specification for the
            ## non-empty panels only (this is an arbitrary choice, but
            ## it usually makes more sense, even though it's less
            ## general than the other choice).

            ## which ones are non-NA?
            id <- which(sapply(limitlist, function(x) !all(is.na(x))))

            ## replace these with the limits supplied, except if the
            ## supplied limits are NULL, in which case retain limits
            ## calculated by prepanel.

            old.limitlist <- limitlist
            limitlist[id] <- lim
            which.null <- sapply(limitlist, is.null)
            limitlist[which.null] <- old.limitlist[which.null]

            ## lim overrides prepanel except NAs
            for (i in seq_along(limitlist))
            {
                if (!is.character(limitlist[[i]]) &&
                    !is.character(old.limitlist[[i]]))
                {
                    isna <- is.na(limitlist[[i]])
                    limitlist[[i]][isna] <- old.limitlist[[i]][isna]
                }
            }
        }
        for (i in seq_along(limitlist))
        {
            if (!all(is.na(limitlist[[i]])) && !is.character(limitlist[[i]])) 
                limitlist[[i]] <- ## preserves class
                    extend.limits(limitlist[[i]], axs = axs)
            ## o.w., keep it as it is
        }
        slicelen <- numeric(length(limitlist))
        for (i in seq_along(limitlist))
            slicelen[i] <-
                if (!is.character(limitlist[[i]]))
                    diff(range(as.numeric(limitlist[[i]])))
                else if (!any(is.na(numlimitlist[[i]])))
                    diff(range(numlimitlist[[i]]))
                else NA_real_
        ans <-
            list(limits = limitlist,
                 used.at = used.at,
                 numlimitlist = numlimitlist,
                 slicelen = slicelen)
    }
    ans
}




limits.and.aspect <-
    function(prepanel.default,
             prepanel = NULL,
             have.xlim = FALSE, xlim = NULL,
             have.ylim = FALSE, ylim = NULL,
             x.relation, y.relation,
             panel.args.common = list(),
             panel.args = list(),
             aspect,
             banking = lattice.getOption("banking"),
             npackets = length(panel.args),
             x.axs = "r", y.axs = "r",
             ...)  ## extra arguments for prepanel (for qqmathline)
{
    prepanel.default.function <- getFunctionOrName(prepanel.default)
    prepanel <- getFunctionOrName(prepanel)
    if (npackets<1) stop("need at least one panel")
    x.limits <- vector("list", npackets)
    y.limits <- vector("list", npackets)
    x.used.at <- vector("list", npackets)
    y.used.at <- vector("list", npackets)
    x.num.limit <- vector("list", npackets)
    y.num.limit <- vector("list", npackets)
    dxdy <- vector("list", npackets)

    for (count in seq_len(npackets))
    {
        if (is.list(panel.args[[count]]))
        {
            pargs <- c(panel.args.common, panel.args[[count]], list(...))
            tem <- do.call("prepanel.default.function", pargs)
            if (is.function(prepanel)) ## results will 'overwrite' defaults
            {
                prenames <- names(formals(prepanel))
                if (!("..." %in% prenames)) pargs <- pargs[intersect(names(pargs), prenames)]
                pretem <- do.call("prepanel", pargs)
                ## prepanel() over-rides defaults except NAs - e.g. ylim = c(0, NA)
                if (!is.null(pretem$xlim) && !is.character(pretem$xlim))
                    if (any(isna <- is.na(pretem$xlim)))
                        pretem$xlim[isna] <- tem$xlim[isna]
                if (!is.null(pretem$ylim) && !is.character(pretem$ylim))
                    if (any(isna <- is.na(pretem$ylim)))
                        pretem$ylim[isna] <- tem$ylim[isna]
                tem <- updateList(tem, pretem)
                ## tem[names(pretem)] <- pretem
            }
            x.limits[[count]] <- tem$xlim
            y.limits[[count]] <- tem$ylim
            x.used.at[[count]] <- if (is.null(tem$xat)) NA else tem$xat
            y.used.at[[count]] <- if (is.null(tem$yat)) NA else tem$yat
            x.num.limit[[count]] <- if (is.null(tem$xat)) NA else range(tem$xat)
            y.num.limit[[count]] <- if (is.null(tem$yat)) NA else range(tem$yat)
            dxdy[[count]] <- list(dx = tem$dx, dy = tem$dy)
        }
        else  ## this happens for empty panels
        {
            x.limits[[count]] <- c(NA_real_, NA_real_)
            y.limits[[count]] <- c(NA_real_, NA_real_)
            x.used.at[[count]] <- NA_real_
            y.used.at[[count]] <- NA_real_
            x.num.limit[[count]] <- NA_real_
            y.num.limit[[count]] <- NA_real_
            dxdy[[count]] <- list(dx = NA_real_, dy = NA_real_)
        }
    }

    ## Some explanation might be helpful here. The for loop above
    ## creates a list of xlims/ylims. Each of these might be either
    ## numeric (when x/y is numeric, shingle or POSIXt etc), or levels
    ## of a factor (that's how prepanel.default.functions are set
    ## up). However, at this point, all x.limits[[i]] must be of the
    ## same type. Returned limits must be in accordance with this
    ## type. The only exception is when relation = "free", in which
    ## case they may be different. This could happen if [xy]lim or
    ## limits is supplied as a list in the high level function.

    x.limits <-
        limitsFromLimitlist(have.lim = have.xlim,
                            lim = xlim,
                            relation = x.relation,
                            limitlist = x.limits,
                            used.at = x.used.at,
                            numlimitlist = x.num.limit,
                            axs = x.axs,
                            npackets = npackets)
    y.limits <-
        limitsFromLimitlist(have.lim = have.ylim,
                            lim = ylim,
                            relation = y.relation,
                            limitlist = y.limits,
                            used.at = y.used.at,
                            numlimitlist = y.num.limit,
                            axs = y.axs,
                            npackets = npackets)

    if (is.character(aspect))
    {
        if (aspect == "xy")
        {
            aspect <-
                median(sapply(dxdy, banking) *
                       y.limits$slicelen /
                       x.limits$slicelen, 
                       na.rm = TRUE)
### old aspect calculation
##             aspect <- median(unlist(lapply(dxdy, banking)),
##                              na.rm = TRUE) * y.limits$slicelen /
##                                  x.limits$slicelen
##             if (y.relation == "free" || x.relation == "free")
##                 warning("'aspect=xy' when 'relation=free' is not sensible")
        }
        else if (aspect == "iso")
        {
            aspect <-
                median(y.limits$slicelen / x.limits$slicelen,
                       na.rm = TRUE)
            if (y.relation == "free" || x.relation == "free")
                warning("'aspect=\"iso\"' approximate since 'relation=\"free\"'")
        }
        else aspect <- 1
    }
    list(x.limits = x.limits$limits,
         y.limits = y.limits$limits,
         x.used.at = x.limits$used.at,
         y.used.at = y.limits$used.at,
         x.num.limit = x.limits$numlimitlist,
         y.num.limit = y.limits$numlimitlist,
         aspect.ratio = aspect,
         prepanel.default = prepanel.default,
         prepanel = prepanel)
}


