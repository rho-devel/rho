expected <- TRUE                
test(id=939, code={                
argv <- list(function (e1, e2)                 
{                
    ok <- switch(.Generic, "<" = , ">" = , "<=" = , ">=" = ,                 
        "==" = , "!=" = TRUE, FALSE)                
    if (!ok) {                
        warning(sprintf("'%s' is not meaningful for ordered factors",                 
            .Generic))                
        return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))                
    }                
    if (.Generic %in% c("==", "!="))                 
        return(NextMethod(.Generic))                
    nas <- is.na(e1) | is.na(e2)                
    ord1 <- FALSE                
    ord2 <- FALSE                
    if (nzchar(.Method[1L])) {                
        l1 <- levels(e1)                
        ord1 <- TRUE                
    }                
    if (nzchar(.Method[2L])) {                
        l2 <- levels(e2)                
        ord2 <- TRUE                
    }                
    if (all(nzchar(.Method)) && (length(l1) != length(l2) ||                 
        !all(l2 == l1)))                 
        stop("level sets of factors are different")                
    if (ord1 && ord2) {                
        e1 <- as.integer(e1)                
        e2 <- as.integer(e2)                
    }                
    else if (!ord1) {                
        e1 <- match(e1, l2)                
        e2 <- as.integer(e2)                
    }                
    else if (!ord2) {                
        e2 <- match(e2, l1)                
        e1 <- as.integer(e1)                
    }                
    value <- get(.Generic, mode = "function")(e1, e2)                
    value[nas] <- NA                
    value                
})                
do.call('is.function', argv);                
},  o = expected);                
                
