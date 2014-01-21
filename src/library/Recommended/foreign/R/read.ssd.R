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

read.ssd <- function(libname, sectionnames, tmpXport=tempfile(),
                     tmpProgLoc=tempfile(), sascmd="sas")
{
    ##
    ## copyright 2002 VJ Carey <stvjc@channing.harvard.edu>
    ##           2004 R Development Core Team
    ##
    ## read.ssd -- 'read' a SAS v6 ssd format file by converting
    ## the data to sas xport format and then using R foreign:read.xport
    ## march 22 2002 -- works fine if the desired sas lib/section exist
    ## but cannot detect when sas 'fails' owing to nonexistence
    ##
    ## tries to clean up interim results
    ##
    ## works for sas v8

    tmpFiles <- tmpXport
    on.exit(unlink(tmpFiles))
    logGuess <- function (x)
    {
        ## guess the name of the log file by stripping all
        ## path to the sas program (log will lie in executing dir)
        expl <- strsplit(x, "")[[1L]]
        rex <- rev(expl)
        br <- match("/", rex)[1L]
        if (is.na(br))
            return(x)
        return(paste(rev(rex[1L:(br - 1L)]), sep = "", collapse = ""))
    }
    fileExtension <- function(string)
    {
        n <- nchar(string)
        chars <- substring(string, 1L:n, 1L:n)
        lastDot <- n + 1L - match(".", rev(chars), nomatch = n + 1L)
        substring(string, lastDot + 1L, n)
    }
    sn <- sectionnames
    if(any(nchar(sn) > 8L)) {
        oldDir   <- libname
        libname  <- tempdir()
        allFiles <- list.files(oldDir)
        oldNames <- character(0L)
        for(i in 1L:length(sn)){
            fName <- grep(sn[i], allFiles, value = TRUE)
            if(length(fName) == 0L)
                stop(gettextf("sectionname %s not found", sn[i]), domain = NA)
            oldNames <- c(oldNames, fName)
        }
        sectionnames <- linkNames <- character(length(oldNames))
        for(i in 1L:length(oldNames)) {
            sectionnames[i] <- paste("sn", i, sep = "")
            linkNames[i] <- paste(sectionnames[i],
                                  fileExtension(oldNames[i]),
                                  sep = ".")
            oldPath  <- file.path(oldDir,  oldNames[i])
            linkPath <- file.path(libname, linkNames[i])
            file.symlink(oldPath, linkPath)

            tmpFiles <- c(tmpFiles, linkPath)
        }
    }
    st0 <- "option validvarname = v6;"
    st1 <- paste("libname src2rd '",libname,"';\n",sep="")
    st2 <- paste("libname rd xport '", tmpXport, "';\n", sep="")
    st3 <- paste("proc copy in=src2rd out=rd;\n")
    st4 <- paste("select", sectionnames, ";\n", sep=" ")
    tmpProg <- paste(tmpProgLoc, ".sas", sep="")
    tmpProgLogBase <- logGuess(tmpProgLoc)
    tmpProgLog <- paste(tmpProgLogBase, ".log", sep="")
    cat(st0, file=tmpProg)
    cat(st1, file = tmpProg, append = TRUE)
    cat(st2, file = tmpProg, append = TRUE)
    cat(st3, file = tmpProg, append = TRUE)
    cat(st4, file = tmpProg, append = TRUE)
    if(.Platform$OS.type == "windows")
        sascmd <- paste(shQuote(sascmd), "-sysin")
    sasrun <- try(sysret <- system( paste( sascmd, tmpProg ) ))
    if (!inherits(sasrun, "try-error") & sysret == 0L)
    {
        unlink( tmpProg )
        unlink( tmpProgLog )
        if(length(sectionnames) == 1L) return( read.xport( tmpXport ) )
        else {
            zz <- read.xport(tmpXport)
            names(zz) <- sn
            return(zz)
        }
    } else {
        cat("SAS failed.  SAS program at", tmpProg,"\n")
        if(.Platform$OS.type == "unix") {
            cat("a log and other error products should be in the vicinity\n")
            system(paste("ls -l ", tmpProgLog))
        } else {
            cat("The log file will be ",
                paste(basename(tmpProgLoc), ".log", sep=""),
                " in the current directory\n", sep="")
        }
        warning(gettextf("SAS return code was %d", sysret), domain = NA)
        return(NULL)
    }
}
