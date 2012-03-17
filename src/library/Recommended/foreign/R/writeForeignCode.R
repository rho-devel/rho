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

write.foreign <-
    function(df, datafile, codefile, package = c("SPSS","Stata","SAS"), ...)
{
    do.call(paste("writeForeign", package, sep = ""),
            c(list(df = df, datafile = datafile, codefile = codefile), ...))
    invisible(NULL)
}

## we want ASCII quotes, not UTF-8 quotes here
adQuote <- function(x) paste("\"", x, "\"", sep = "")

writeForeignSPSS <- function(df, datafile, codefile, varnames = NULL)
{
    ## FIXME: re-write this to hold a connection open
    dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
    write.table(dfn, file=datafile, row.names=FALSE, col.names=FALSE,
                sep=",", quote=FALSE, na="",eol=",\n")

    varlabels <- names(df)
    if (is.null(varnames)) {
        varnames <- abbreviate(names(df), 8L)
        if (any(sapply(varnames, nchar) > 8L))
            stop("I cannot abbreviate the variable names to eight or fewer letters")
        if (any(varnames!=varlabels))
            warning("some variable names were abbreviated")
    }

    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)

    dl.varnames <- varnames
    if (any(chv <- sapply(df,is.character))) {
        lengths <- sapply(df[chv],function(v) max(nchar(v)))
        if(any(lengths > 255L))
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep="")
        star <- ifelse(c(FALSE, diff(which(chv) > 1L))," *", " ")
        dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
  }

    cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
        file = codefile)
    cat("/",  dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
    cat(paste(varnames, adQuote(varlabels),"\n"), ".\n",
        file = codefile, append = TRUE)
    factors <- sapply(df,is.factor)
    if (any(factors)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for(v in which(factors)){
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v]," \n", file = codefile, append = TRUE)
            levs <- levels(df[[v]])
            cat(paste(seq_along(levs), adQuote(levs), "\n", sep = " "),
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}


writeForeignStata <- function(df, datafile, codefile)
{
    write.table(df, file=datafile, row.names=FALSE, col.names=FALSE,
                sep=",", quote=FALSE, na=".")
    nms <- names(df)
    factors <- sapply(df,is.factor) | sapply(df, is.character)
    formats <- paste(nms,"fmt",sep="_")
    nms <- ifelse(factors,paste(nms,formats,sep=":"),nms)

    cat("infile",nms," using ",datafile,", automatic\n", file=codefile)
}
