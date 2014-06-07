### This file is part of the 'foreign' package for R.

# Copyright (c) 2004-5  R Development Core Team
# Enhancements Copyright (c) 2006 Stephen Weigand

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

make.SAS.names <- function(varnames, validvarname = c("V7", "V6")){
  validvarname <- match.arg(validvarname)
  nmax <- if(validvarname == "V7") 32L else 8L

  x <- sub("^([0-9])", "_\\1", varnames)
  x <- gsub("[^a-zA-Z0-9_]", "_", x)
  x <- abbreviate(x, minlength = nmax)

  if (any(nchar(x) > nmax) || any(duplicated(x)))
      stop(gettextf("Cannot uniquely abbreviate the variable names to %d or fewer characters", nmax), domain = NA)
  names(x) <- varnames
  x
}

make.SAS.formats <- function(varnames){
  x <- sub("^([0-9])", "_\\1", varnames)
  x <- gsub("[^a-zA-Z0-9_]", "_", x)
  x <- sub("([0-9])$", "\\1f", x) # can't end in digit so append 'f'
  x <- abbreviate(x, minlength = 8L)

  if(any(nchar(x) > 8L) || any(duplicated(x)))
    stop("Cannot uniquely abbreviate format names to conform to ",
         " eight-character limit and not ending in a digit")
  names(x) <- varnames
  x
}

writeForeignSAS <- function(df, datafile, codefile, dataname = "rdata",
                          validvarname = c("V7", "V6"), libpath = NULL)
{
    ## FIXME: re-write this to hold a connection open
    factors <- sapply(df, is.factor)
    strings <- sapply(df, is.character)
    logicals <- sapply(df, is.logical)
    dates <- sapply(df, FUN = function(x) inherits(x, "Date") || inherits(x, "dates") || inherits(x, "date"))
    xdates <- sapply(df, FUN = function(x)  inherits(x, "dates") || inherits(x, "date"))
    datetimes <- sapply(df, FUN = function(x) inherits(x, "POSIXt"))

    varlabels <- names(df)
    varnames <- make.SAS.names(names(df), validvarname = validvarname)
    if (any(varnames != varlabels))
        message("Some variable names were abbreviated or otherwise altered.")


    dfn <- df
    if (any(factors))
        dfn[factors] <- lapply(dfn[factors], as.numeric)
    if (any(logicals))
        dfn[logicals] <- lapply(dfn[logicals], as.numeric)
    if (any(datetimes))
        dfn[datetimes] <- lapply(dfn[datetimes],
                                 function(x) format(x, "%d%b%Y %H:%M:%S"))
    if(any(xdates))
        dfn[xdates] <- lapply(dfn[xdates], function(x) as.Date(as.POSIXct(x)))

    write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
                sep = ",", quote = TRUE, na = "")
    lrecl <- max(sapply(readLines(datafile),nchar)) + 4L

    cat("* Written by R;\n", file = codefile)
    cat("* ", deparse(sys.call(-2L))[1L], ";\n\n",
        file = codefile, append = TRUE)
    if (any(factors)) {
        cat("PROC FORMAT;\n", file=codefile, append=TRUE)
        fmtnames <- make.SAS.formats(varnames[factors])
        fmt.values <- lapply(df[, factors, drop = FALSE], levels)
        names(fmt.values) <- fmtnames
        for (f in fmtnames) {
            cat("value", f, "\n", file = codefile, append = TRUE)
            values <- fmt.values[[f]]
            for(i in 1L:length(values)){
                cat("    ", i,"=", adQuote(values[i]), "\n",
                    file=codefile, append=TRUE)
            }
            cat(";\n\n",file=codefile,append=TRUE)
        }
    }

    if (!is.null(libpath)) {
    	cat("libname ROutput '", libpath, "';\n", file = codefile,
            append = TRUE, sep = "")
    	cat("DATA ROutput.", dataname, ";\n", file = codefile,
            append = TRUE, sep = "")
    } else cat("DATA ", dataname, ";\n", file = codefile, append = TRUE)

    if (any(strings)) {
        cat("LENGTH", file = codefile, append = TRUE)
        lengths <- sapply(df[,strings, drop = FALSE],
                          FUN = function(x) max(nchar(x)))
        names(lengths) <- varnames[strings]
        for(v in varnames[strings])
            cat("\n", v, "$", lengths[v], file = codefile, append = TRUE)
        cat("\n;\n\n", file = codefile, append = TRUE)
    }

    if (any(dates)) {
        cat("INFORMAT", file = codefile, append = TRUE)
        for(v in varnames[dates])
            cat("\n", v, file = codefile, append = TRUE)
        cat("\n YYMMDD10.\n;\n\n", file = codefile, append = TRUE)
    }

    if (any(datetimes)) {
        cat("INFORMAT", file = codefile, append = TRUE)
        for(v in varnames[datetimes])
            cat("\n", v, file = codefile, append = TRUE)
        cat("\n DATETIME18.\n;\n\n", file = codefile, append = TRUE)
    }

    cat("INFILE ",adQuote(datafile),
        "\n     DSD",
        "\n     LRECL=", lrecl, ";\n",
        file = codefile ,append = TRUE)

    cat("INPUT", file = codefile, append = TRUE)
    for(v in 1L:ncol(df))
        cat("\n", varnames[v], file = codefile, append = TRUE)
        if(strings[v]) cat(" $ ", file = codefile, append = TRUE)

    cat("\n;\n", file = codefile, append = TRUE)

    for(v in 1L:ncol(df))
        if (varnames[v] != names(varnames)[v])
            cat("LABEL ", varnames[v],"=", adQuote(varlabels[v]), ";\n",
                file = codefile, append = TRUE)

    if (any(factors))
        for (f in 1L:length(fmtnames))
            cat("FORMAT", names(fmtnames)[f], paste(fmtnames[f],".", sep = ""),
                ";\n", file = codefile, append = TRUE)

    if (any(dates))
        for(v in varnames[dates])
            cat("FORMAT", v, "yymmdd10.;\n", file = codefile, append = TRUE)

    if (any(datetimes))
        for(v in varnames[datetimes])
            cat("FORMAT", v, "datetime18.;\n", file = codefile, append = TRUE)

    cat("RUN;\n", file=  codefile, append = TRUE)
}

