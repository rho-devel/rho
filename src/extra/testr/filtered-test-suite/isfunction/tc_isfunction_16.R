expected <- TRUE           
test(id=717, code={           
argv <- list(function (..., na.rm)            
{           
    coerceTimeUnit <- function(x) {           
        as.vector(switch(attr(x, "units"), secs = x, mins = 60 *            
            x, hours = 60 * 60 * x, days = 60 * 60 * 24 * x,            
            weeks = 60 * 60 * 24 * 7 * x))           
    }           
    ok <- switch(.Generic, max = , min = , sum = , range = TRUE,            
        FALSE)           
    if (!ok)            
        stop(gettextf("'%s' not defined for \"difftime\" objects",            
            .Generic), domain = NA)           
    x <- list(...)           
    Nargs <- length(x)           
    if (Nargs == 0) {           
        .difftime(do.call(.Generic), "secs")           
    }           
    else {           
        units <- sapply(x, function(x) attr(x, "units"))           
        if (all(units == units[1L])) {           
            args <- c(lapply(x, as.vector), na.rm = na.rm)           
        }           
        else {           
            args <- c(lapply(x, coerceTimeUnit), na.rm = na.rm)           
            units <- "secs"           
        }           
        .difftime(do.call(.Generic, args), units[[1L]])           
    }           
})           
do.call('is.function', argv);           
},  o = expected);           
           
