## From: Prof Brian Ripley <ripley@stats.ox.ac.uk>
## To: Martin Maechler <maechler@stat.math.ethz.ch>
## cc: Kurt.Hornik@wu-wien.ac.at, Matrix-authors@r-project.org,
##         simon.urbanek@r-project.org
## Subject: Re: Matrix 0.999375-16 uploaded to CRAN
## Date: Thu, 16 Oct 2008 16:26:57 +0100 (BST)

## ................

## A) The good news is that I have a set of Makefiles that work on Sun make
## and I believe are POSIX-compliant.  The following R script

## was applied to src/CHOLMOD/Lib/Makefile
## and            src/SPQR/Lib/Makefile
## to 'POSIXify' them,
fixup <- function(file="Makefile")
{
    file.copy(file, paste(file, "orig", sep="."))
    orig <- readLines(file)
    current <- ""
    for(i in seq_along(orig)) {
	if (length(grep(".o: ", orig[i], fixed = TRUE))) {
	    print(orig[i]) # "verbose info"
	    current <- sub("[^ ]* (.*)", "\\1", orig[i])
	    current <- strsplit(current, " ")[[1]][1]
	} else if (length(grep("$<", orig[i], fixed = TRUE))) {
            ## use last line's current :
	    orig[i] <- sub("$<", current, orig[i], fixed = TRUE)
	} else if (length(grep("^PKG_CFLAGS *= *-I", orig[i]))) {
	    orig[i] <- sub("^PKG_CFLAGS", "PKG_CPPFLAGS", orig[i])
        }
    }
    writeLines(orig, file)
}

## and I hand edited ../AMD/Source/Makefile and
## ../CHOLMOD/Lib/Makefile.  If I did it right those changes are attached as
## Matrix.patch2.  Hopefully such a script makes future maintenance easy.

## MM: Try to apply it to all 4 Makefiles  and only use a patch file for
##    "the rest"
