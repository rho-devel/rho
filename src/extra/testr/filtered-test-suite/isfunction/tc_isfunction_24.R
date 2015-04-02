expected <- TRUE            
test(id=948, code={            
argv <- list(function (qr, y)             
{            
    if (!is.qr(qr))             
        stop("first argument must be a QR decomposition")            
    n <- as.integer(nrow(qr$qr))            
    if (is.na(n))             
        stop("invalid nrow(qr$qr)")            
    p <- as.integer(ncol(qr$qr))            
    if (is.na(p))             
        stop("invalid ncol(qr$qr)")            
    k <- as.integer(qr$rank)            
    if (is.na(k))             
        stop("invalid ncol(qr$rank)")            
    im <- is.matrix(y)            
    if (!im)             
        y <- as.matrix(y)            
    ny <- as.integer(ncol(y))            
    if (is.na(ny))             
        stop("invalid ncol(y)")            
    if (p == 0L)             
        return(if (im) matrix(0, p, ny) else numeric())            
    ix <- if (p > n)             
        c(seq_len(n), rep(NA, p - n))            
    else seq_len(p)            
    if (is.complex(qr$qr)) {            
        coef <- matrix(NA_complex_, nrow = p, ncol = ny)            
        coef[qr$pivot, ] <- .Internal(qr_coef_cmplx(qr, y))[ix,             
            ]            
        return(if (im) coef else c(coef))            
    }            
    if (isTRUE(attr(qr, "useLAPACK"))) {            
        coef <- matrix(NA_real_, nrow = p, ncol = ny)            
        coef[qr$pivot, ] <- .Internal(qr_coef_real(qr, y))[ix,             
            ]            
        return(if (im) coef else c(coef))            
    }            
    if (k == 0L)             
        return(if (im) matrix(NA, p, ny) else rep.int(NA, p))            
    storage.mode(y) <- "double"            
    if (nrow(y) != n)             
        stop("'qr' and 'y' must have the same number of rows")            
    z <- .Fortran(.F_dqrcf, as.double(qr$qr), n, k, as.double(qr$qraux),             
        y, ny, coef = matrix(0, nrow = k, ncol = ny), info = integer(1L),             
        NAOK = TRUE)[c("coef", "info")]            
    if (z$info)             
        stop("exact singularity in 'qr.coef'")            
    if (k < p) {            
        coef <- matrix(NA_real_, nrow = p, ncol = ny)            
        coef[qr$pivot[seq_len(k)], ] <- z$coef            
    }            
    else coef <- z$coef            
    if (!is.null(nam <- colnames(qr$qr)))             
        if (k < p)             
            rownames(coef)[qr$pivot] <- nam            
        else rownames(coef) <- nam            
    if (im && !is.null(nam <- colnames(y)))             
        colnames(coef) <- nam            
    if (im)             
        coef            
    else drop(coef)            
})            
do.call('is.function', argv);            
},  o = expected);            
            
