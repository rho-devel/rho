expected <- TRUE            
test(id=772, code={            
argv <- list(function (x, width = 12, ...)             
{            
    if (is.character(x))             
        return(format.default(x, ...))            
    if (is.null(width))             
        width = 12L            
    n <- length(x)            
    rvec <- rep.int(NA_character_, n)            
    for (i in seq_len(n)) {            
        y <- x[[i]]            
        cl <- oldClass(y)            
        if (m <- match("AsIs", cl, 0L))             
            oldClass(y) <- cl[-m]            
        rvec[i] <- toString(y, width = width, ...)            
    }            
    dim(rvec) <- dim(x)            
    dimnames(rvec) <- dimnames(x)            
    format.default(rvec, justify = "right")            
})            
do.call('is.function', argv);            
},  o = expected);            
            
