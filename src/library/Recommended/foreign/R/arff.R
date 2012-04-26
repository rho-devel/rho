### This file is part of the 'foreign' package for R.
### Functions for reading and writing files in Weka ARFF format.

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

### <NOTE>
### String and evaluation types are enclosed by single quotes upon
### writing and enclosing single quotes are removed upon reading.
### Escaped single quotes inside single quotes may also occur.
### </NOTE>

read.arff <-
function(file)
{
    ## See read.table().
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }

    ## Get header.
    col_names <- NULL
    col_types <- NULL
    col_dfmts <- character()
    line <- readLines(file, n = 1L)
    while(length(line) &&
          regexpr('^[[:space:]]*@(?i)data', line, perl = TRUE) == -1L) {
        if(regexpr('^[[:space:]]*@(?i)attribute', line,
                   perl = TRUE) > 0L) {
            con <- textConnection(line)
            line <- scan(con, character(), quiet = TRUE)
            close(con)
            if(length(line) < 3L)
                stop("Invalid attribute specification.")
            col_names <- c(col_names, line[2L])
            if((type <- tolower(line[3L])) == "date") {
                col_types <- c(col_types, "character")
                col_dfmts <- c(col_dfmts,
                               if(length(line) > 3L)
                               ISO_8601_to_POSIX_datetime_format(line[4L])
                               else "%Y-%m-%d %H:%M:%S")
            }
            else if(type == "relational")
                stop("Type 'relational' currently not implemented.")
            else {
                type <- sub("\\{.*", "factor", type)
                ## (Could try to preserve factor levels ...)
                type <- sub("string", "character", type)
                type <- sub("real", "numeric", type)
                col_types <- c(col_types, type)
                col_dfmts <- c(col_dfmts, NA)
            }
        }
        line <- readLines(file, n = 1L)
    }

    ## Test header.
    if(length(line) == 0L)
        stop("Missing data section.")
    if(is.null(col_names))
        stop("Missing attribute section.")
    if(length(col_names) !=
       length(grep('factor|numeric|character', col_types)))
        stop("Invalid type specification.")

    ## Get data.
    data <- read.table(file, sep = ",", na.strings = "?",
                       colClasses = col_types, comment.char = '%')
    if(any(ind <- which(!is.na(col_dfmts))))
        for(i in ind)
            data[i] <- as.data.frame(strptime(data[[i]], col_dfmts[i]))
    ## Remove left over escapes.
    for (i in seq_len(length(data)))
        if (is.factor(data[[i]]))
           levels(data[[i]]) <- gsub("\\\\", "", levels(data[[i]]))
    names(data) <- col_names
    data
}

write.arff <-
function(x, file, eol = "\n", relation = deparse(substitute(x)))
{
    ## See write.table().
    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, "wb")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")

    if (!is.data.frame(x) && !is.matrix(x))
        x <- data.frame(x)

    ## We need to quote for ourselves, as write.table() escapes the quote
    ## char but not the backslash.  Weka seems to prefer backslash
    ## escapes inside single quotes, so we provide that ...
    squote <- function(s) {
        ## Don't quote NAs.
        ifelse(is.na(s), s,
               sprintf("'%s'", gsub("(['\\])", "\\\\\\1", s)))
    }
    spquote <- function(s) {
        if (length(grep("^[[:alpha:]]", s)) == 0L) s <- paste("X", s, sep="")
        if (length(grep(" ", s))) s <- paste('"', s, '"', sep="")
        s
    }

    ## Write header.  Quote, mangle if necessary.
    text <- paste('@relation', spquote(make.names(relation)))
    writeLines(text, file, sep = eol)
    for (name in colnames(x)) {
        ## Attribute names need to start with a letter, quoted if contain spaces.
        text <- paste('@attribute', spquote(name))
        if (is.data.frame(x) && is.factor(x[[name]])) {
            lev <- squote(levels(x[[name]]))
            levels(x[[name]]) <- lev
            text <- paste(text, " {", paste(lev, collapse = ","), "}", sep = "")
        }
        else if (is.character(x[,name])) {
            text <- paste(text, "string")
            x[,name] <- squote((x[,name]))
        }
        else if (inherits(x[,name], "Date")) {
            text <- paste(text, "date \"yyyy-MM-dd\"")
            x[,name] <- squote(format(x[,name]))
        }
        else if (inherits(x[,name], "POSIXt")) {
            text <- paste(text, "date \"yyyy-MM-dd hh:mm:ss\"")
            x[,name] <- squote(format(x[,name]))
        }
        else
            text <- paste(text, "numeric")
        writeLines(text, file, sep = eol)
    }

    ## Write data.
    writeLines("@data", file)
    write.table(x, file = file, na = "?", sep = ",",
                eol = eol, quote = FALSE, row.names = FALSE,
                col.names = FALSE)
}


ISO_8601_to_POSIX_datetime_format <-
function(x)
{
    ## First, Weka thinks that 'yyyy' is ISO 8601 ...
    x <- sub("yyyy", "%Y", x, ignore.case = TRUE)
    ## And it's 'DD' and not 'dd' ...
    x <- sub("dd", "%d", x)
    ## And it's 'hh' and not 'HH' ...
    x <- sub("HH", "%H", x)

    ## Now the real stuff.
    ## Is there a POSIX format string for the century component of year?
    x <- sub("CCYY", "%Y", x)
    x <- sub("YY", "%y", x)
    x <- sub("MM", "%m", x)
    x <- sub("DD", "%d", x)
    x <- sub("DDD", "%j", x)
    x <- sub("ww", "%U", x)
    x <- sub("D", "%w", x)
    x <- sub("hh", "%H", x)
    x <- sub("mm", "%M", x)
    x <- sub("ss", "%S", x)
    ## Is there a POSIX format string for fractions of seconds? [No]

    x
}
