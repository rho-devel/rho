expected <- TRUE               
test(id=678, code={               
argv <- list(function (x, format = "", usetz = FALSE, ...)                
{               
    if (!inherits(x, "POSIXlt"))                
        stop("wrong class")               
    if (format == "") {               
        times <- unlist(unclass(x)[1L:3L])               
        secs <- x$sec               
        secs <- secs[!is.na(secs)]               
        np <- getOption("digits.secs")               
        if (is.null(np))                
            np <- 0L               
        else np <- min(6L, np)               
        if (np >= 1L)                
            for (i in seq_len(np) - 1L) if (all(abs(secs - round(secs,                
                i)) < 1e-06)) {               
                np <- i               
                break               
            }               
        format <- if (all(times[!is.na(times)] == 0))                
            "%Y-%m-%d"               
        else if (np == 0L)                
            "%Y-%m-%d %H:%M:%S"               
        else paste0("%Y-%m-%d %H:%M:%OS", np)               
    }               
    y <- .Internal(format.POSIXlt(x, format, usetz))               
    names(y) <- names(x$year)               
    y               
})               
do.call('is.function', argv);               
},  o = expected);               
               
