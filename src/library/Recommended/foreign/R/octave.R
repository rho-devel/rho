### This file is part of the 'foreign' package for R.

# Copyright (c) 2004 Stephen Eglen
# Enhancements Copyright (c) 2004-7 R Development Core Team

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

## Read in a file in Octave text data format.

read.octave <-
function(file)
{
    ## Read in a file in Octave text data format (as created by "save
    ## -ascii" in Octave 2.x) and return a list of the objects
    ## successfully read, along with information on read failures.

    ## E.g., create two variables in Octave

    ## octave> ident_mat = eye(3);
    ## octave> twopi = 2 * pi;
    ## octave> save -ascii 'octfile.dat'

    ## then load this file into R:
    ## > o <- read.octave("octfile.dat")
    ## > o
    ## $twopi
    ## [1] 6.283185
    ## $ident.mat
    ##      [,1] [,2] [,3]
    ## [1,]    1    0    0
    ## [2,]    0    1    0
    ## [3,]    0    0    1

    skip_lines_to_next_item <- function(con) {
        ## Read in data from the connection until we hit the next
        ## variable (assuming "# name" is the first line for a new
        ## variable).  We need to also handle the special case when this
        ## gets us to the end of the connection.
        looking <- TRUE
        while(looking) {
            line <- readLines(con, n = 1L)
            if(length(grep("^# name: ", line)) == 1L) {
                ## We have reached the next variable.
                pushBack(line, con)
                looking <- FALSE
            } else if(length(line) == 0L) {
                ## We have reached the end of file.
                looking <- FALSE
            }
        }
    }

    read_octave_matrix <- function(con) {
        ## Helper function: read in a real matrix/array.
        ## Format of the header varies depending on whether matrix is 2d
        ## or higher.  So need to do a check first.
        line <- readLines(con, 1L)
        if(regexpr("^# rows:", line) > 0L) {
            ## Dealing with a 2d matrix; this header is of the form:
            ## # type: matrix
            ## # rows: 15
            ## # columns: 15
            ## followed by 15 rows of data ...
            nr <- as.integer(gsub("# rows: ", "", line))
            nc <- as.integer(gsub("# columns: ", "", readLines(con, 1L)))
            data <- scan(con, nlines = nr, quiet = TRUE)
            matrix(data, nrow = nr, ncol = nc, byrow = TRUE)
        }
        else {
            ## Assume we have N-d array; this has the format:
            ## # type: matrix
            ## # ndims: 3
            ##  15 15 6
            ## followed by the data, one element per row.
            ## After reading in ndims (3 here), we read in the size of
            ## each dimension (15 x 15 x 6) and then read in the
            ## corresponding number of elements.
            ndims <- as.integer(gsub("# ndims: ", "", line))
            dims <- scan(con, nlines = 1L, quiet = TRUE)
            data <- scan(con, n = prod(dims), quiet = TRUE)
            array(data, dim = dims)
        }
    }

    read_octave_complex_matrix <- function(con) {
        ## Helper function: read in a complex matrix/array.
        ## See read_octave_matrix().
        line <- readLines(con, 1L)
        if(regexpr("^# rows:", line) > 0L) {
            nr <- as.integer(gsub("# rows: ", "", line))
            nc <- as.integer(gsub("# columns: ", "", readLines(con, 1L)))
            data <- readLines(con, n = nr)
            cl <- paste(data, sep = "", collapse = "")
            c1 <- gsub("\\(", "", cl)
            c1 <- gsub("\\)", "", c1)
            c1 <- gsub(",", " ", c1)
            s <- unlist(strsplit(c1, " "))
            nums <- as.numeric(s[-1L])   # Remove initial space.
            reals <- nums[seq.int(from = 1L, by = 2L, length.out = length(nums)/2)]
            imags <- nums[seq.int(from = 2L, by = 2L, length.out = length(nums)/2)]
            matrix(data = complex(real = reals, imaginary = imags),
                   nrow = nr, ncol = nc, byrow = TRUE)
        }
        else {
            ndims <- as.integer(gsub("# ndims: ", "", line))
            dims <- scan(con, nlines = 1L, quiet = TRUE)
            data <- readLines(con, n = prod(dims))
            data <- gsub("\\(", "", data)
            data <- gsub("\\)", "", data)
            nums <- strsplit(data, ",")
            stopifnot(all(sapply(nums, length) == 2))
            array(complex(real = as.numeric(sapply(nums, "[", 1L)),
                          imaginary = as.numeric(sapply(nums, "[", 2L))),
                  dim = dims)
        }
    }

    read_octave_string_array <- function(con) {
        ## Helper function: read in a string array.
        elements <- as.numeric(gsub("# elements: ", "",
                                    readLines(con, 1L)))
        d <- readLines(con, n = 2L * elements)
        ## Remove the odd-numbered lines, they just store "length".
        d[seq.int(from = 2L, by = 2L, length.out = length(d)/2L)]
    }

    read_octave_scalar <- function(con) {
        ## Helper function: read in a scalar.
        as.numeric(scan(con, nlines = 1L, quiet = TRUE))
    }

    read_octave_complex_scalar <- function(con) {
        ## Helper function: read in a complex scalar.
        d <- readLines(con, n = 1L)
        ## Remove parens then split.
        str <- gsub("\\(", "", d)
        str <- gsub("\\)", "", str)
        nums <- as.numeric(unlist(strsplit(str, ",")))
        stopifnot(length(nums) == 2L)
        complex(real = nums[1L], imaginary = nums[2L])
    }


    read_octave_range <- function(con) {
        ## Helper function: read in a range.
        d <- readLines(con, n = 1L) # Skip over "# base, limit, increment".
        d <- as.numeric(scan(con, nlines = 1L, quiet = TRUE))
        stopifnot(length(d) == 3L)
        seq.int(from = d[1L], to = d[2L], by = d[3L])
    }

    read_octave_unknown <- function(con, type) {
        ## Skip over unknown Octave types.
        ## If we do not recognize the type of Octave variable, give a
        ## warning, and try reading until the next variable.
        ## This only works for unknown atomic types, so let us hope we
        ## have code for all recursive ones ...
        warning(gettextf("cannot handle unknown type %s", sQuote(type)),
                domain = NA)
        skip_lines_to_next_item(con)
        NULL
    }

    read_octave_list <- function(con) {
        ## Helper function: read in a list.
        ## Note that lists are deprecated now in favor of cells.
        n <- as.numeric(gsub("# length: ", "", readLines(con, 1L)))
        out <- vector("list", n)
        for(i in seq_len(n)) {
            ## Skip over "# name: _val" lines.
            readLines(con, 1L)
            out[[i]] <- read_item(con)
        }
        out
    }

    read_octave_cell <- function(con) {
        ## Helper function: read in a cell.
        nr <- as.numeric(gsub("# rows: ", "", readLines(con, 1L)))
        nc <- as.numeric(gsub("# columns: ", "", readLines(con, 1L)))
        out <- vector("list", nr * nc)
        dim(out) <- c(nr, nc)
        for(j in seq_len(nc)) {
            for(i in seq_len(nr)) {
                ## Skip over "# name: <cell-element>" lines.
                readLines(con, 1L)
                ## Get the next cell element.
                out[[i, j]] <- read_item(con)
            }
            ## Argh.  There seem to be empty lines after each column of
            ## cell elements?  Let us not rely on this, and instead read
            ## on to the next item.
            skip_lines_to_next_item(con)
        }
        out
    }

    read_octave_struct <- function(con) {
        ## Helper function: read in a struct.
        n <- as.numeric(gsub("# length: ", "", readLines(con, 1L)))
        out <- vector("list", n)
        for(i in seq_len(n)) {
            ## Skip over "# name: _val" lines.
            name <- gsub("# name: ", "", readLines(con, 1L))
            out[[i]] <- read_item(con)
            names(out)[i] <- name
        }
        out
    }

    read_octave_bool <- function(con) {
        ## Helper function: read in a bool.
        as.logical(scan(con, nlines = 1L, quiet = TRUE))
    }

    read_octave_bool_matrix <- function(con) {
        ## Helper function: read in a bool matrix.
        nr <- as.integer(gsub("# rows: ", "", readLines(con, 1L)))
        nc <- as.integer(gsub("# columns: ", "", readLines(con, 1L)))
        data <- scan(con, nlines = nr, quiet = TRUE)
        matrix(as.logical(data), nrow = nr, ncol = nc, byrow = TRUE)
    }

    read_item <- function(con) {
        ## Assume that the name has already been read.
        type <- gsub("# type: ", "", readLines(con, 1L))
        switch(type,
               "matrix" = read_octave_matrix(con),
               "scalar" = read_octave_scalar(con),
               "string" = read_octave_string_array(con),
               "string array" = read_octave_string_array(con),
               "range" = read_octave_range(con),
               "complex matrix" = read_octave_complex_matrix(con),
               "complex scalar" = read_octave_complex_scalar(con),
               "list" = read_octave_list(con),
               "cell" = read_octave_cell(con),
               "struct" = read_octave_struct(con),
               "bool" = read_octave_bool(con),
               "bool matrix" = read_octave_bool_matrix(con),
               read_octave_unknown(con, type))
    }

    zz <- file(file, "r")
    on.exit(close(zz))

    readLines(zz, n = 1L)                # Skip over the header line.

    ## Build up a return list of items -- separately store the return
    ## values and the names.
    items <- list()
    names <- character()
    reading <- TRUE
    while(reading) {
        line <- readLines(zz, 1L, ok = TRUE)
        if(length(line) == 0L) {
            reading <- FALSE
        }
        else {
            items <- c(items, list(read_item(zz)))
            names <- c(names, gsub("# name: ", "", line))
        }
    }

    names(items) <- names
    items
}
