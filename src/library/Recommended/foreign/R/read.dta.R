### This file is part of the 'foreign' package for R.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

read.dta <- function(file, convert.dates = TRUE,
                     convert.factors = TRUE, missing.type = FALSE,
                     convert.underscore = FALSE, warn.missing.labels = TRUE)
{
    if(length(grep("^(http|ftp|https)://", file))) {
        tmp <- tempfile()
        download.file(file, tmp, quiet = TRUE, mode = "wb")
        file <- tmp
        on.exit(unlink(file))
    }
    rval <- .External(do_readStata, file)

    if(convert.underscore)
        names(rval) <- gsub("_", ".", names(rval))

    types <- attr(rval, "types")
    stata.na <- data.frame(type = 251L:255L,
                           min = c(101, 32741, 2147483621, 2^127, 2^1023),
                           inc = c(1,1,1,2^115,2^1011)
                           )


    if(!missing.type) {
        if (abs(attr(rval, "version")) >= 8L) {
            for(v in which(types > 250L)) {
                this.type <- types[v] - 250L
                rval[[v]][rval[[v]] >= stata.na$min[this.type]] <- NA
            }
        }
    } else {
        if (abs(attr(rval, "version")) >= 8L) {
            missings <- vector("list", length(rval))
            names(missings) <- names(rval)
            for(v in which(types > 250L)) {
                this.type <- types[v] - 250L
                nas <- is.na(rval[[v]]) |  rval[[v]] >= stata.na$min[this.type]
                natype <- (rval[[v]][nas] - stata.na$min[this.type])/stata.na$inc[this.type]
                natype[is.na(natype)] <- 0L
                missings[[v]] <- rep(NA, NROW(rval))
                missings[[v]][nas] <- natype
                rval[[v]][nas] <- NA
            }
            attr(rval,"missing") <- missings
        } else
            warning("'missing.type' only applicable to version >= 8 files")
    }

    if (convert.dates) {
        ff <- attr(rval,"formats")
        dates <- grep("%-*d", ff)
        ## avoid as.Date in case strptime is messed up
        base <- structure(-3653, class = "Date")
        for(v in dates) rval[[v]] <- base+rval[[v]]
    }
    if (convert.factors %in% c(TRUE, NA)) {
        if (attr(rval, "version") == 5L)
            warning("cannot read factor labels from Stata 5 files")
        else {
            ll <- attr(rval, "val.labels")
            tt <- attr(rval, "label.table")
            factors <- which(ll != "")
            for(v in factors) {
                labels <- tt[[ll[v]]]
                if (warn.missing.labels && is.null(labels)) {
                    warning(gettextf("value labels (%s) for %s are missing",
                                     sQuote(ll[v]), sQuote(names(rval)[v])),
                            domain = NA)
                    next
                }
                if(!is.na(convert.factors)) {
                    ## some levels don't have labels, so skip
                    if (!all(rval[[v]] %in% c(NA, NaN, tt[[ll[v]]])))
                        next
                }
                rval[[v]] <- factor(rval[[v]], levels=tt[[ll[v]]],
                                    labels=names(tt[[ll[v]]]))
            }
        }
    }

    att <- attributes(rval)
    ##rval <- as.data.frame(rval, stringsAsFactors=FALSE)
    class(rval) <- "data.frame"
    newatt <- attributes(rval)
    newatt <- c(newatt, att[!(names(att) %in% names(newatt))])
    attributes(rval) <- newatt
    rval
}

write.dta <-
    function(dataframe, file, version = 7L,
             convert.dates = TRUE, tz = "GMT",
             convert.factors = c("labels","string","numeric","codes"))
{

    if(!is.data.frame(dataframe))
        stop("The object \"dataframe\" must have class data.frame")
    if (version < 6L) stop("Version must be 6-12")
    if (version == 9L) version <- 8L
    if (version == 11L) version <- 10L
    if (version == 12L) version <- 10L
    if (version > 12L) {
        warning("Version must be 6-12: using 7")
        version <- 7L
    }


    ## assume this is in chars: probably only works for ASCII
    ## But Stata formats are ASCII only
    namelength <- if (version == 6L) 8L else 31L
    oldn <- names(dataframe)
    nn <- abbreviate(oldn, namelength)
    if (any(nchar(nn) > namelength))
        stop("cannot uniquely abbreviate variable names")
    if (any(nchar(oldn) > namelength))
        warning("abbreviating variable names")
    names(dataframe) <- nn
    attr(dataframe,"orig.names") <- oldn

    if (convert.dates) {
        dates <- which(sapply(dataframe,
                              function(x) inherits(x, "Date")))
        for(v in dates)
            dataframe[[v]] <- as.vector(julian(dataframe[[v]],
                                               as.Date("1960-1-1", tz="GMT")))
        dates <- which(sapply(dataframe,
                              function(x) inherits(x, "POSIXt")))
        for(v in dates)
            dataframe[[v]] <- as.vector(round(julian(dataframe[[v]],
                                                     ISOdate(1960,1,1, tz=tz))))
    }
    convert.factors <- match.arg(convert.factors)
    factors <- which(sapply(dataframe,is.factor))
    if(convert.factors == "string") {
        for(v in factors)
            dataframe[[v]] <- I(as.character(dataframe[[v]]))
    } else if (convert.factors == "numeric") {
        for(v in factors)
            dataframe[[v]] <- as.numeric(as.character(dataframe[[v]]))
    } else if (convert.factors == "codes") {
        for (v in factors)
            dataframe[[v]] <- as.numeric(dataframe[[v]])
    }

    shortlevels <- function(f) {
        ll <- levels(f)
        if (is.null(ll)) return(NULL)
        ## avoid warning if non-ASCII strings are used (unwisely)
        if (all(nchar(ll, "bytes") <= 80L)) ll else abbreviate(ll, 80L)
    }
    leveltable <- lapply(dataframe, shortlevels)

    if (any(sapply(dataframe, function(x) {
        d <- dim(x)
        !is.null(d) && d[1L] < length(x)
        })))
        stop("cannot handle multicolumn columns")
    invisible(.External(do_writeStata, file, dataframe, version, leveltable))
}
