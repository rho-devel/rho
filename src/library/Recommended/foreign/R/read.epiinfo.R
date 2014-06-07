### This file is part of the 'foreign' package for R.

### R/read.epiinfo.R
### (c) 2002-4 Thomas Lumley
### Patches (c) 2002 Mark Myatt

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

read.epiinfo <- function (file, read.deleted = FALSE,
                          guess.broken.dates = FALSE, thisyear = NULL,
                          lower.case.names = FALSE)
  {
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument 'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file))
    }
    line <- readLines(file, 1L, ok = TRUE)
    headerlength <- na.omit(sapply(strsplit(line, " ")[[1L]], as.numeric))[1L]
    if (headerlength <= 0L)
        stop("file has zero or fewer variables: probably not an EpiInfo file")
    headerlines <- readLines(file, n = headerlength)
    pushBack(headerlines, file)
    comments <- sapply(headerlines, function(s) substring(s, 46L, 46L + 80L))
    #
    # Added comment = ""  to fix '#' as entrychar being read as a comment
    #
    header <- scan(file, nlines = headerlength,
                   what = list(name = "", x = 0, y = 0, color = 0, x1 = 0,
                   y1 = 0, type = 0, len = 0, color = 0),
                   flush = TRUE, quiet = TRUE, comment.char = "")
    header <- as.data.frame(lapply(header, I))
    header$start <- cumsum(c(1L, header$len))[1L:headerlength]
    header$stop <- cumsum(header$len)
    multiline <- ceiling(max(header$stop) / 78L)
    really.variables <- header$len != 0
    header <- header[really.variables, ]
    entrychar <- substr(header$name, 1L, 1L)
    if (all(entrychar %in% c("#", "_")))
        header$name <- substr(header$name, 2L, 12L)
    comments <- comments[really.variables]
    #
    # Added support for EpiData introduced field types:
    #
    #   12  Automatic ID number fields (treated as numeric)
    #   16  European (i.e. dd/mm/yyyy) format automatic date
    #   17  SOUNDEX field
    #
    numbers <- (header$len > 0L) & ((header$type %in% c(0L, 6L, 12L)) | (header$type > 12L)) & !(header$type %in% c(16L, 17L))
    datalines <- scan(file, what = "", sep = "\n", quote = "", quiet = TRUE, blank.lines.skip = TRUE, comment.char = "")
    #
    # Added check for empty file
    #
    if (length(datalines) == 0L)
      stop("no records in file")
    if (length(datalines)%%multiline)
        warning("wrong number of records")
    datalines <- matrix(datalines, nrow = multiline)
    if (multiline > 1L)
        datalines[-multiline, ] <- substr(datalines[-multiline, ], 1L, 78L)
    datalines <- apply(datalines, 2L, paste, collapse = "")
    deleted <- substr(datalines, nchar(datalines), nchar(datalines)) == "?"
    nvars <- NROW(header)
    data <- as.data.frame(lapply(1L:nvars, function(i) I(substring(datalines, header$start[i], header$stop[i]))))
    names(data) <- header$name
    names(comments) <- header$name
    if (is.na(read.deleted))
        data[deleted, ] <- NA
    else if (!read.deleted)
        data <- data[!deleted, ]
    if (guess.broken.dates && is.null(thisyear))
        thisyear <- format(Sys.time(), format = "%Y")
    #
    # Added support for field types:
    #
    #   10  US (i.e. mm/dd/yyyy) format field (EpiInfo)
    #   12  Automatic ID number (treated as numeric)
    #   16  European (i.e. dd/mm/yyyy) format automatic date (EpiData)
    #   17  SOUNDEX field (EpiData)
    #
    for (i in 1L:nvars) {
        if (numbers[i])
            data[[i]] <- as.numeric(data[[i]])
        else if (header$type[i] == 5L)
            data[[i]] <- ifelse(data[[i]] %in% c("Y", "N"), data[[i]] == "Y", NA)
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 5L && guess.broken.dates)
            data[[i]] <- as.Date(strptime(paste(data[[i]], thisyear, sep = "/"), format = "%d/%m/%Y"))
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 8L && guess.broken.dates)
            data[[i]] <- as.Date(strptime(data[[i]], format = "%d/%m/%y"))
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 10L)
            data[[i]] <- as.Date(strptime(data[[i]], format = "%d/%m/%Y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 5L && guess.broken.dates)
            data[[i]] <- as.Date(strptime(paste(data[[i]], thisyear, sep = "/"), format = "%m/%d/%Y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 8L && guess.broken.dates)
            data[[i]] <- as.Date(strptime(data[[i]], format = "%m/%d/%y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 10L)
            data[[i]] <- as.Date(strptime(data[[i]], format = "%m/%d/%Y"))
        #
        # SOUNDEX (type 17) fields
        #
        else if (header$type[i] == 17L) {
            data[[i]][substr(data[[i]], 1L, 1L) == " "] <- NA
            data[[i]] <- substr(data[[i]], 1L, 5L)
            }
        else {
            blanks <- grep("^[[:blank:]]*$", data[[i]])
            data[[i]][blanks] <- NA
        }
    }
    if (!is.na(read.deleted) && read.deleted)
        attr(data, "deleted") <- deleted
    attr(data, "prompts") <- comments
    #
    # Added parameter and code to specify lower case variable names
    #
    if (lower.case.names)
      names(data) <- tolower(names(data))
    data
}
