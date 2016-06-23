# This file just provides stub implementations of these functions for
# compatibility.  Rho doesn't use the GNU R bytecode compiler.

##
## Compiler options
##

compilerOptions <- new.env(hash = TRUE, parent = emptyenv())
compilerOptions$optimize <- 2
compilerOptions$suppressAll <- FALSE
compilerOptions$suppressUndefined <-
    c(".Generic", ".Method", ".Random.seed", ".self")

getCompilerOption <- function(name, options = NULL) {
    if (name %in% names(options))
        options[[name]]
    else
        get(name, compilerOptions)
}

# You can set compiler options.  They just don't do anything in rho.
setCompilerOptions <- function(...) {
    options <- list(...)
    nm <- names(options)
    for (n in nm)
        if (! exists(n, compilerOptions))
            stop(gettextf("'%s' is not a valid compiler option", n),
                 domain = NA)
    old <- list()
    newOptions <- as.list(compilerOptions) # copy options
    for (n in nm) {
        op <- options[[n]]
        switch(n,
               optimize = {
                   op <- as.integer(op)
                   if (length(op) == 1 && 0 <= op && op <= 3) {
                       old <- c(old, list(optimize =
                                          compilerOptions$optimize))
                       newOptions$optimize <- op
                   }
               },
               suppressAll = {
                   if (identical(op, TRUE) || identical(op, FALSE)) {
                       old <- c(old, list(suppressAll =
                                          compilerOptions$suppressAll))
                       newOptions$suppressAll <- op
                   }
               },
               suppressUndefined = {
                   if (identical(op, TRUE) || identical(op, FALSE) ||
                       is.character(op)) {
                       old <- c(old, list(suppressUndefined =
                                          compilerOptions$suppressUndefined))
                       newOptions$suppressUndefined <- op
                   }
               })
    }
    jitEnabled <- enableJIT(-1)
    for(n in names(newOptions)) # commit the new options
        assign(n, newOptions[[n]], compilerOptions)
    invisible(old)
}

cmpfun <- function(f, options = NULL) {
  if (!is.function(f))
      stop("cannot compile a non-function")
}

cmpfile <- function(infile, outfile, ascii = FALSE, env = .GlobalEnv,
                    verbose = FALSE, options = NULL) {
    if (! is.environment(env) || ! identical(env, topenv(env)))
        stop("'env' must be a top level environment")
    if (missing(outfile)) {
        basename <- sub("\\.[a-zA-Z0-9]$", "", infile)
        outfile <- paste0(basename, ".Rc")
    }
    if (infile == outfile)
        stop("input and output file names are the same")
    file.copy(infile, outfile)
    invisible(NULL)
}

loadcmp <- function (file, envir = .GlobalEnv, chdir = FALSE) {
  sys.source(file, envir = envir, chdir = chdir)
  invisible()
}

compile <- function(e, env = .GlobalEnv, options = NULL) {
  e
}

disassemble <- function(code) {
  # There's no bytecode, so nothing to do.
  invisible()
}

enableJIT <- function(level) {
  invisible()  # A no-op in rho.
}

compilePKGS <- function(enable) {
  invisible()  # A no-op in rho.
}

.onLoad <- function(libname, pkgname) {
    if (Sys.getenv("R_COMPILER_SUPPRESS_ALL") != "")
        setCompilerOptions(suppressAll = TRUE)
    if (Sys.getenv("R_COMPILER_SUPPRESS_UNDEFINED") != "")
        setCompilerOptions(suppressUndefined = TRUE)
    if (Sys.getenv("R_COMPILER_OPTIMIZE") != "")
        tryCatch({
            lev <- as.integer(Sys.getenv("R_COMPILER_OPTIMIZE"))
            if (0 <= lev && lev <= 3)
                setCompilerOptions(optimize = lev)
        }, error = function(e) e, warning = function(w) w)
}
