# part of R package boot
# copyright (C) 1997-2001 Angelo J. Canty
# corrections (C) 1997-2011 B. D. Ripley
#
# Unlimited distribution is permitted

# safe version of sample
# needs R >= 2.9.0
# only works if size is not specified in R >= 2.11.0, but it always is in boot
sample0 <- function(x, ...) x[sample.int(length(x), ...)]
bsample <- function(x, ...) x[sample.int(length(x), replace = TRUE, ...)]

isMatrix <- function(x) length(dim(x)) == 2L

## random permutation of x.
rperm <- function(x) sample0(x, length(x))



antithetic.array <- function(n, R, L, strata)
#
#  Create an array of indices by antithetic resampling using the
#  empirical influence values in L.  This function just calls anti.arr
#  to do the sampling within strata.
#
{
    inds <- as.integer(names(table(strata)))
    out <- matrix(0L, R, n)
    for (s in inds) {
	gp <- seq_len(n)[strata == s]
        out[, gp] <- anti.arr(length(gp), R, L[gp], gp)
    }
    out
}

anti.arr <- function(n, R, L, inds=seq_len(n))
{
#  R x n array of bootstrap indices, generated antithetically
#  according to the empirical influence values in L.
    unique.rank <- function(x) {
# Assign unique ranks to a numeric vector
        ranks <- rank(x)
        if (any(duplicated(ranks))) {
            inds <- seq_along(x)
            uniq <- sort(unique(ranks))
            tab <- table(ranks)
            for (i in seq_along(uniq))
                if (tab[i] > 1L) {
                    gp <- inds[ranks == uniq[i]]
                    ranks[gp] <- rperm(inds[sort(ranks) == uniq[i]])
                }
        }
        ranks
    }
    R1 <- floor(R/2)
    mat1 <- matrix(bsample(inds, R1*n), R1, n)
    ranks <- unique.rank(L)
    rev <- inds
    for (i in seq_len(n)) rev[i] <- inds[ranks == (n+1-ranks[i])]
    mat1 <- rbind(mat1, matrix(rev[mat1], R1, n))
    if (R != 2*R1) mat1 <- rbind(mat1, bsample(inds, n))
    mat1
}




balanced.array <- function(n, R, strata)
{
#
# R x n array of bootstrap indices, sampled hypergeometrically
# within strata.
#
    output <- matrix(rep(seq_len(n), R), n, R)
    inds <- as.integer(names(table(strata)))
    for(is in inds) {
        group <- seq_len(n)[strata == is]
        if(length(group) > 1L) {
            g <- matrix(rperm(output[group,  ]), length(group), R)
            output[group,  ] <- g
        }
    }
    t(output)
}

boot <- function(data, statistic, R, sim = "ordinary",
                 stype = c("i", "f", "w"),
                 strata  =  rep(1, n), L = NULL, m = 0, weights = NULL,
		 ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ...,
                 parallel = c("no", "multicore", "snow"),
                 ncpus = getOption("boot.ncpus", 1L), cl = NULL)
{
#
# R replicates of bootstrap applied to  statistic(data)
# Possible sim values are: "ordinary", "balanced", "antithetic",
#                     "permutation", "parametric"
# Various auxilliary functions find the indices to be used for the
# bootstrap replicates and then this function loops over those replicates.
#
    call <- match.call()
    stype <- match.arg(stype)
    if (missing(parallel)) parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") have_snow <- TRUE
        if (!have_mc && !have_snow) ncpus <- 1L
    }
    if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
        warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
        simple <- FALSE
    }
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- NROW(data)
    if ((n == 0) || is.null(n))
        stop("no data in call to 'boot'")
    temp.str <- strata
    strata <- tapply(seq_len(n),as.numeric(strata))
    t0 <- if (sim != "parametric") {
	if ((sim == "antithetic") && is.null(L))
            L <- empinf(data = data, statistic = statistic,
                        stype = stype, strata = strata, ...)
        if (sim != "ordinary") m <- 0
        else if (any(m < 0)) stop("negative value of 'm' supplied")
        if ((length(m) != 1L) && (length(m) != length(table(strata))))
            stop("length of 'm' incompatible with 'strata'")
        if ((sim == "ordinary") || (sim == "balanced")) {
            if (isMatrix(weights) && (nrow(weights) != length(R)))
                stop("dimensions of 'R' and 'weights' do not match")}
        else weights <- NULL
        if (!is.null(weights))
            weights <- t(apply(matrix(weights, n, length(R), byrow = TRUE),
                               2L, normalize, strata))
        if (!simple) i <- index.array(n, R, sim, strata, m, L, weights)

        original <- if (stype == "f") rep(1, n)
        else if (stype == "w") {
            ns <- tabulate(strata)[strata]
            1/ns
        } else seq_len(n)

        t0 <- if (sum(m) > 0L) statistic(data, original, rep(1, sum(m)), ...)
        else statistic(data, original, ...)
        rm(original)
        t0
    } else # "parametric"
	statistic(data, ...)

    pred.i <- NULL
    fn <- if (sim == "parametric") {
        ## force promises, so values get sent by parallel
        ran.gen; data; mle
        function(r) {
            dd <- ran.gen(data, mle)
            statistic(dd, ...)
        }
    } else {
        if (!simple && ncol(i) > n) {
            pred.i <- as.matrix(i[ , (n+1L):ncol(i)])
            i <- i[, seq_len(n)]
        }
        if (stype %in% c("f", "w")) {
            f <- freq.array(i)
            rm(i)
            if (stype == "w") f <- f/ns
            if (sum(m) == 0L) function(r) statistic(data, f[r,  ], ...)
            else function(r) statistic(data, f[r, ], pred.i[r, ], ...)
        } else if (sum(m) > 0L)
            function(r) statistic(data, i[r, ], pred.i[r,], ...)
        else if (simple)
            function(r)
                statistic(data,
                          index.array(n, 1, sim, strata, m, L, weights), ...)
        else function(r) statistic(data, i[r, ], ...)
    }
    RR <- sum(R)
    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(RR), fn, mc.cores = ncpus)
        } else if (have_snow) {
            list(...) # evaluate any promises
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
                if(RNGkind()[1L] == "L'Ecuyer-CMRG")
                    parallel::clusterSetRNGStream(cl)
                res <- parallel::parLapply(cl, seq_len(RR), fn)
                parallel::stopCluster(cl)
                res
            } else parallel::parLapply(cl, seq_len(RR), fn)
        }
    } else lapply(seq_len(RR), fn)
    t.star <- matrix(, RR, length(t0))
    for(r in seq_len(RR)) t.star[r, ] <- res[[r]]

    if (is.null(weights)) weights <- 1/tabulate(strata)[strata]
    boot.return(sim, t0, t.star, temp.str, R, data, statistic, stype, call,
                seed, L, m, pred.i, weights, ran.gen, mle)
}

normalize <- function(wts, strata)
{
#
# Normalize a vector of weights to sum to 1 within each strata.
#
    n <- length(strata)
    out <- wts
    inds <- as.integer(names(table(strata)))
    for (is in inds) {
        gp <- seq_len(n)[strata == is]
        out[gp] <- wts[gp]/sum(wts[gp]) }
    out
}

boot.return <- function(sim, t0, t, strata, R, data, stat, stype, call,
			seed, L, m, pred.i, weights, ran.gen, mle)
#
# Return the results of a bootstrap in the form of an object of class
# "boot".
#
{
    out <- list(t0=t0, t=t, R=R, data=data, seed=seed,
                statistic=stat, sim=sim, call=call)
    if (sim == "parametric")
        out <- c(out, list(ran.gen=ran.gen, mle=mle))
    else if (sim == "antithetic")
        out <- c(out, list(stype=stype, strata=strata, L=L))
    else if (sim == "ordinary") {
        if (sum(m) > 0)
            out <- c(out, list(stype=stype, strata=strata,
                               weights=weights, pred.i=pred.i))
        else 	out <- c(out, list(stype=stype, strata=strata,
                                   weights=weights))
    } else if (sim == "balanced")
        out <- c(out, list(stype=stype, strata=strata,
                           weights=weights ))
    else
        out <- c(out, list(stype=stype, strata=strata))
    class(out) <- "boot"
    out
}

c.boot <- function (..., recursive = TRUE)
{
    args <- list(...)
    nm <- lapply(args, names)
    if (!all(sapply(nm, function(x) identical(x, nm[[1]]))))
        stop("arguments are not all the same type of \"boot\" object")
    res <- args[[1]]
    res$R <- sum(sapply(args, "[[", "R"))
    res$t <- do.call(rbind, lapply(args, "[[", "t"))
    res
}

boot.array <- function(boot.out, indices=FALSE) {
#
#  Return the frequency or index array for the bootstrap resamples
#  used in boot.out
#  This function recreates such arrays from the information in boot.out
#
    if (exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE))
        temp <- get(".Random.seed", envir=.GlobalEnv, inherits = FALSE)
    else temp<- NULL
    assign(".Random.seed",  boot.out$seed, envir=.GlobalEnv)
    n <- NROW(boot.out$data)
    R <- boot.out$R
    sim <- boot.out$sim
    if (boot.out$call[[1L]] == "tsboot") {
#  Recreate the array for an object created by tsboot, The default for
#  such objects is to return the index array unless index is specifically
#  passed as F
        if (missing(indices)) indices <- TRUE
        if (sim == "model")
            stop("index array not defined for model-based resampling")
        n.sim <- boot.out$n.sim
        i.a <- ts.array(n, n.sim, R, boot.out$l,
			sim, boot.out$endcorr)
        out <- matrix(NA,R,n.sim)
        for(r in seq_len(R)) {
            if (sim == "geom")
                ends <- cbind(i.a$starts[r,  ],
                              i.a$lengths[r,  ])
            else
                ends <- cbind(i.a$starts[r,], i.a$lengths)
            inds <- apply(ends, 1L, make.ends, n)
            if (is.list(inds))
                inds <- unlist(inds)[seq_len(n.sim)]
            out[r,] <- inds
        }
    }
    else if (boot.out$call[[1L]] == "censboot") {
#  Recreate the array for an object created by censboot as long
#  as censboot was called with sim = "ordinary"
        if (sim == "ordinary") {
            strata <- tapply(seq_len(n), as.numeric(boot.out$strata))
            out <- cens.case(n,strata,R)
        }
        else	stop("boot.array not implemented for this object")
    }
    else {
#  Recreate the array for objects created by boot or tilt.boot
        if (sim == "parametric")
            stop("array cannot be found for parametric bootstrap")
        strata <- tapply(seq_len(n),as.numeric(boot.out$strata))
        if (boot.out$call[[1L]] == "tilt.boot")
            weights <- boot.out$weights
        else {
            weights <- boot.out$call$weights
            if (!is.null(weights))
                weights <- boot.out$weights
        }
        out <- index.array(n, R, sim, strata, 0, boot.out$L, weights)
    }
    if (!indices) out <- freq.array(out)
    if (!is.null(temp)) assign(".Random.seed", temp, envir=.GlobalEnv)
    else rm(.Random.seed, pos=1)
    out
}

plot.boot <- function(x,index=1, t0=NULL, t=NULL, jack=FALSE,
	qdist="norm",nclass=NULL,df, ...) {
#
#  A plot method for bootstrap output objects.  It produces a histogram
#  of the bootstrap replicates and a QQ plot of them.  Optionally it can
#  also produce a jackknife-after-bootstrap plot.
#
    boot.out <- x
    t.o <- t
    if (is.null(t)) {
        t <- boot.out$t[,index]
        if (is.null(t0)) t0 <- boot.out$t0[index]
    }
    t <- t[is.finite(t)]
    if (const(t, min(1e-8,mean(t, na.rm=TRUE)/1e6))) {
        print(paste("All values of t* are equal to ", mean(t, na.rm=TRUE)))
        return(invisible(boot.out))
    }
    if (is.null(nclass)) nclass <- min(max(ceiling(length(t)/25),10),100)
    if (!is.null(t0)) {
#  Calculate the breakpoints for the histogram so that one of them is
#  exactly t0.
        rg <- range(t)
        if (t0<rg[1L]) rg[1L] <- t0
        else if (t0 >rg[2L]) rg[2L] <- t0
        rg <- rg+0.05*c(-1,1)*diff(rg)
        lc <- diff(rg)/(nclass-2)
        n1 <- ceiling((t0-rg[1L])/lc)
        n2 <- ceiling((rg[2L]-t0)/lc)
        bks <- t0+(-n1:n2)*lc
    }
    R <- boot.out$R
    if (qdist == "chisq") {
        qq <- qchisq((seq_len(R))/(R+1),df=df)
        qlab <- paste("Quantiles of Chi-squared(",df,")",sep="")
    }
    else {
	if (qdist!="norm")
            warning(gettextf("%s distribution not supported: using normal instead", sQuote(qdist)), domain = NA)
        qq <- qnorm((seq_len(R))/(R+1))
        qlab <-"Quantiles of Standard Normal"
    }
    if (jack) {
        layout(mat = matrix(c(1,2,3,3), 2L, 2L, byrow=TRUE))
        if (is.null(t0))
            hist(t,nclass=nclass,probability=TRUE,xlab="t*")
        else	hist(t,breaks=bks,probability=TRUE,xlab="t*")
        if (!is.null(t0)) abline(v=t0,lty=2)
        qqplot(qq,t,xlab=qlab,ylab="t*")
        if (qdist == "norm") abline(mean(t),sqrt(var(t)),lty=2)
        else abline(0,1,lty=2)
        jack.after.boot(boot.out,index=index,t=t.o, ...)
    }
    else {
        par(mfrow=c(1,2))
        if (is.null(t0))
            hist(t,nclass=nclass,probability=TRUE,xlab="t*")
        else	hist(t,breaks=bks,probability=TRUE,xlab="t*")
        if (!is.null(t0)) abline(v=t0,lty=2)
        qqplot(qq,t,xlab=qlab,ylab="t*")
        if (qdist == "norm") abline(mean(t),sqrt(var(t)),lty=2)
        else abline(0,1,lty=2)
    }
    par(mfrow=c(1,1))
    invisible(boot.out)
}

print.boot <- function(x, digits = getOption("digits"),
                          index = 1L:ncol(boot.out$t), ...)
{
#
# Print the output of a bootstrap
#
    boot.out <- x
    sim <- boot.out$sim
    cl <- boot.out$call
    t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
    allNA <- apply(t,2L,function(t) all(is.na(t)))
    ind1 <- index[allNA]
    index <- index[!allNA]
    t <- matrix(t[, !allNA], nrow = nrow(t))
    rn <- paste("t",index,"*",sep="")
    if (length(index) == 0L)
        op <- NULL
    else if (is.null(t0 <- boot.out$t0)) {
        if (is.null(boot.out$call$weights))
            op <- cbind(apply(t,2L,mean,na.rm=TRUE),
                        sqrt(apply(t,2L,function(t.st) var(t.st[!is.na(t.st)]))))
        else {
            op <- NULL
            for (i in index)
                op <- rbind(op, imp.moments(boot.out,index=i)$rat)
            op[,2L] <- sqrt(op[,2])
        }
        dimnames(op) <- list(rn,c("mean", "std. error"))
    }
    else {
        t0 <- boot.out$t0[index]
        if (is.null(boot.out$call$weights)) {
            op <- cbind(t0,apply(t,2L,mean,na.rm=TRUE)-t0,
                        sqrt(apply(t,2L,function(t.st) var(t.st[!is.na(t.st)]))))
            dimnames(op) <- list(rn, c("original"," bias  "," std. error"))
        }
        else {
            op <- NULL
            for (i in index)
                op <- rbind(op, imp.moments(boot.out,index=i)$rat)
            op <- cbind(t0,op[,1L]-t0,sqrt(op[,2L]),
                        apply(t,2L,mean,na.rm=TRUE))
            dimnames(op) <- list(rn,c("original", " bias  ",
                                      " std. error", " mean(t*)"))
        }
    }
    if (cl[[1L]] == "boot") {
        if (sim == "parametric")
            cat("\nPARAMETRIC BOOTSTRAP\n\n")
        else if (sim == "antithetic") {
            if (is.null(cl$strata))
                cat("\nANTITHETIC BOOTSTRAP\n\n")
            else	cat("\nSTRATIFIED ANTITHETIC BOOTSTRAP\n\n")
        }
        else if (sim == "permutation") {
            if (is.null(cl$strata))
                cat("\nDATA PERMUTATION\n\n")
            else	cat("\nSTRATIFIED DATA PERMUTATION\n\n")
        }
        else if (sim == "balanced") {
            if (is.null(cl$strata) && is.null(cl$weights))
                cat("\nBALANCED BOOTSTRAP\n\n")
            else if (is.null(cl$strata))
                cat("\nBALANCED WEIGHTED BOOTSTRAP\n\n")
            else if (is.null(cl$weights))
                cat("\nSTRATIFIED BALANCED BOOTSTRAP\n\n")
            else	cat("\nSTRATIFIED WEIGHTED BALANCED BOOTSTRAP\n\n")
        }
        else {
            if (is.null(cl$strata) && is.null(cl$weights))
                cat("\nORDINARY NONPARAMETRIC BOOTSTRAP\n\n")
            else if (is.null(cl$strata))
                cat("\nWEIGHTED BOOTSTRAP\n\n")
            else if (is.null(cl$weights))
                cat("\nSTRATIFIED BOOTSTRAP\n\n")
            else 	cat("\nSTRATIFIED WEIGHTED BOOTSTRAP\n\n")
        }
    }
    else if (cl[[1L]] == "tilt.boot") {
        R <- boot.out$R
        th <- boot.out$theta
        if (sim == "balanced")
            cat("\nBALANCED TILTED BOOTSTRAP\n\n")
        else	cat("\nTILTED BOOTSTRAP\n\n")
        if ((R[1L] == 0) || is.null(cl$tilt) || eval(cl$tilt))
            cat("Exponential tilting used\n")
        else	cat("Frequency Smoothing used\n")
        i1 <- 1
        if (boot.out$R[1L]>0)
            cat(paste("First",R[1L],"replicates untilted,\n"))
        else {
            cat(paste("First ",R[2L]," replicates tilted to ",
                      signif(th[1L],4),",\n",sep=""))
            i1 <- 2
        }
        if (i1 <= length(th)) {
            for (j in i1:length(th))
                cat(paste("Next ",R[j+1L]," replicates tilted to ",
                          signif(th[j],4L),
                          ifelse(j!=length(th),",\n",".\n"),sep=""))
        }
        op <- op[, 1L:3L]
    }
    else if (cl[[1L]] == "tsboot") {
        if (!is.null(cl$indices))
            cat("\nTIME SERIES BOOTSTRAP USING SUPPLIED INDICES\n\n")
        else if (sim == "model")
            cat("\nMODEL BASED BOOTSTRAP FOR TIME SERIES\n\n")
        else if (sim == "scramble") {
            cat("\nPHASE SCRAMBLED BOOTSTRAP FOR TIME SERIES\n\n")
            if (boot.out$norm)
                cat("Normal margins used.\n")
            else	cat("Observed margins used.\n")
        }
        else if (sim == "geom") {
            if (is.null(cl$ran.gen))
                cat("\nSTATIONARY BOOTSTRAP FOR TIME SERIES\n\n")
            else	cat(paste("\nPOST-BLACKENED STATIONARY",
                                  "BOOTSTRAP FOR TIME SERIES\n\n"))
            cat(paste("Average Block Length of",boot.out$l,"\n"))
        }
        else {	if (is.null(cl$ran.gen))
                    cat("\nBLOCK BOOTSTRAP FOR TIME SERIES\n\n")
        else	cat(paste("\nPOST-BLACKENED BLOCK",
                          "BOOTSTRAP FOR TIME SERIES\n\n"))
                    cat(paste("Fixed Block Length of",boot.out$l,"\n"))
		}
    }
    else {
        cat("\n")
        if (sim == "weird") {
            if (!is.null(cl$strata)) cat("STRATIFIED ")
            cat("WEIRD BOOTSTRAP FOR CENSORED DATA\n\n")
        }
        else if ((sim == "ordinary") ||
                 ((sim == "model") && is.null(boot.out$cox))) {
            if (!is.null(cl$strata)) cat("STRATIFIED ")
            cat("CASE RESAMPLING BOOTSTRAP FOR CENSORED DATA\n\n")
        }
        else if (sim == "model") {
            if (!is.null(cl$strata)) cat("STRATIFIED ")
            cat("MODEL BASED BOOTSTRAP FOR COX REGRESSION MODEL\n\n")
        }
        else if (sim == "cond") {
            if (!is.null(cl$strata)) cat("STRATIFIED ")
            cat("CONDITIONAL BOOTSTRAP ")
            if (is.null(boot.out$cox))
                cat("FOR CENSORED DATA\n\n")
            else	cat("FOR COX REGRESSION MODEL\n\n")
        }
    }
    cat("\nCall:\n")
    dput(cl, control=NULL)
    cat("\n\nBootstrap Statistics :\n")
    if (!is.null(op)) print(op,digits=digits)
    if (length(ind1) > 0L)
        for (j in ind1)
            cat(paste("WARNING: All values of t", j, "* are NA\n", sep=""))
    invisible(boot.out)
}




corr <- function(d, w=rep(1,nrow(d))/nrow(d))
{
#  The correlation coefficient in weighted form.
    s <- sum(w)
    m1 <- sum(d[, 1L] * w)/s
    m2 <- sum(d[, 2L] * w)/s
    (sum(d[, 1L] * d[, 2L] * w)/s - m1 * m2)/sqrt((sum(d[, 1L]^2 * w)/s - m1^2) * (sum(d[, 2L]^2 * w)/s - m2^2))
}


extra.array <- function(n, R, m, strata=rep(1,n))
{
#
# Extra indices for predictions.  Can only be used with
# types "ordinary" and "stratified".  For type "ordinary"
# m is a positive integer.  For type "stratified" m can
# be a positive integer or a vector of the same length as
# strata.
#
    if (length(m) == 1L)
        output <- matrix(sample.int(n, m*R, replace=TRUE), R, m)
    else {
        inds <- as.integer(names(table(strata)))
        output <- matrix(NA, R, sum(m))
        st <- 0
        for (i in inds) {
            if (m[i] > 0) {
                gp <- seq_len(n)[strata == i]
                inds1 <- (st+1):(st+m[i])
                output[,inds1] <- matrix(bsample(gp, R*m[i]), R, m[i])
                st <- st+m[i]
            }
        }
    }
    output
}

freq.array <- function(i.array)
{
#
# converts R x n array of bootstrap indices into
# R X n array of bootstrap frequencies
#
    result <- NULL
    n <- ncol(i.array)
    result <- t(apply(i.array, 1, tabulate, n))
    result
}



importance.array <- function(n, R, weights, strata){
#
#  Function to do importance resampling  within strata based
#  on the weights supplied.  If weights is a matrix with n columns
#  R must be a vector of length nrow(weights) otherwise weights
#  must be a vector of length n and R must be a scalar.
#
    imp.arr <- function(n, R, wts, inds=seq_len(n))
        matrix(bsample(inds, n*R, prob=wts), R, n)
    output <- NULL
    if (!isMatrix(weights))
        weights <- matrix(weights, nrow=1)
    inds <- as.integer(names(table(strata)))
    for (ir in seq_along(R)) {
        out <- matrix(rep(seq_len(n), R[ir]), R[ir], n, byrow=TRUE)
        for (is in inds) {
            gp <- seq_len(n)[strata == is]
            out[, gp] <- imp.arr(length(gp), R[ir],
                                 weights[ir,gp], gp)
        }
        output <- rbind(output, out)
    }
    output
}

importance.array.bal <- function(n, R, weights, strata) {
#
#  Function to do balanced importance resampling within strata
#  based on the supplied weights.  Balancing is achieved in such
#  a way that each index appears in the array approximately in
#  proportion to its weight.
#
    imp.arr.bal <- function(n, R, wts, inds=seq_len(n)) {
        if (sum (wts) != 1) wts <- wts / sum(wts)
        nRw1 <- floor(n*R*wts)
        nRw2 <- n*R*wts - nRw1
        output <- rep(inds, nRw1)
        if (any (nRw2 != 0))
            output <- c(output,
                        sample0(inds, round(sum(nRw2)), prob=nRw2))
        matrix(rperm(output), R, n)
    }
    output <- NULL
    if (!isMatrix(weights))
        weights <- matrix(weights, nrow = 1L)
    inds <- as.integer(names(table(strata)))
    for (ir in seq_along(R)) {
        out <- matrix(rep(seq_len(n), R[ir]), R[ir], n, byrow=TRUE)
        for (is in inds) {
            gp <- seq_len(n)[strata == is]
            out[,gp] <- imp.arr.bal(length(gp), R[ir], weights[ir,gp], gp)
        }
        output <- rbind(output, out)
    }
    output
}



index.array <- function(n, R, sim, strata=rep(1,n), m=0, L=NULL, weights=NULL)
{
#
#  Driver function for generating a bootstrap index array.  This function
#  simply determines the type of sampling required and calls the appropriate
#  function.
#
    indices <- NULL
    if (is.null (weights)) {
        if (sim == "ordinary") {
            indices <- ordinary.array(n, R, strata)
            if (sum(m) > 0)
                indices <- cbind(indices, extra.array(n, R, m, strata))
        }
    else if (sim == "balanced")
        indices <- balanced.array(n, R, strata)
    else if (sim == "antithetic")
        indices <- antithetic.array(n, R, L, strata)
    else if (sim == "permutation")
        indices <- permutation.array(n, R, strata)
    } else {
        if (sim == "ordinary")
            indices <- importance.array(n, R, weights, strata)
        else if (sim == "balanced")
            indices <- importance.array.bal(n, R, weights, strata)
    }
    indices
}

jack.after.boot <- function(boot.out, index=1, t=NULL, L=NULL,
	useJ=TRUE, stinf = TRUE, alpha=NULL, main = "", ylab=NULL, ...)
{
# jackknife after bootstrap plot
    t.o <- t
    if (is.null(t)) {
        if (length(index) > 1L) {
            index <- index[1L]
            warning("only first element of 'index' used")
        }
        t <- boot.out$t[, index]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    if (is.null(alpha)) {
        alpha <- c(0.05, 0.1, 0.16, 0.5, 0.84, 0.9, 0.95)
        if (is.null(ylab))
            ylab <- "5, 10, 16, 50, 84, 90, 95 %-iles of (T*-t)"
    }
    if (is.null(ylab)) ylab <- "Percentiles of (T*-t)"
    data <- boot.out$data
    n <- NROW(data)
    f <- boot.array(boot.out)[fins, , drop=TRUE]
    percentiles <- matrix(data = NA, length(alpha), n)
    J <- numeric(n)
    for(j in seq_len(n)) {
# Find the quantiles of the bootstrap distribution on omitting each point.
        values <- t[f[, j] == 0]
        J[j] <- mean(values)
        percentiles[, j] <- quantile(values, alpha) - J[j]
    }
# Now find the jackknife values to be plotted, and standardize them,
# if required.
    if (!useJ) {
        if (is.null(L))
            J <- empinf(boot.out, index=index, t=t.o, ...)
        else 	J <- L
    }
    else	J <- (n - 1) * (mean(J) - J)
    xtext <- "jackknife value"
    if (!useJ) {
        if (!is.null(L) || (is.null(t.o) && (boot.out$stype == "w")))
            xtext <- paste("infinitesimal", xtext)
        else	xtext <- paste("regression", xtext)
    }
    if (stinf) {
        J <- J/sqrt(var(J))
        xtext <- paste("standardized", xtext)
    }
    top <- max(percentiles)
    bot <- min(percentiles)
    ylts <- c(bot - 0.35 * (top - bot), top + 0.1 * (top - bot))
    percentiles <- percentiles[,order(J)]#
# Plot the overall quantiles and the delete-1 quantiles against the
# jackknife values.
    plot(sort(J), percentiles[1,  ], ylim = ylts, type = "n", xlab = xtext,
         ylab = ylab, main=main)
    for(j in seq_along(alpha))
        lines(sort(J), percentiles[j,  ], type = "b", pch = "*")
    percentiles <- quantile(t, alpha) - mean(t)
    for(j in seq_along(alpha))
        abline(h=percentiles[j], lty=2)
# Now print the observation numbers below the plotted lines.  They are printed
# in five rows so that all numbers can be read easily.
    text(sort(J), rep(c(bot - 0.08 * (top - bot), NA, NA, NA, NA), n, n),
         order(J), cex = 0.5)
    text(sort(J), rep(c(NA, bot - 0.14 * (top - bot), NA, NA, NA), n, n),
         order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, bot - 0.2 * (top - bot), NA, NA), n, n),
         order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, NA, bot - 0.26 * (top - bot), NA), n, n),
         order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, NA, NA, bot - 0.32 * (top - bot)), n, n),
         order(J), cex = 0.5)
    invisible()
}


ordinary.array <- function(n, R, strata)
{
#
# R x n array of bootstrap indices, resampled within strata.
# This is the function which generates a regular bootstrap array
# using equal weights within each stratum.
#
    inds <- as.integer(names(table(strata)))
    if (length(inds) == 1L) {
        output <- sample.int(n, n*R, replace=TRUE)
        dim(output) <- c(R, n)
    } else {
        output <- matrix(as.integer(0L), R, n)
        for(is in inds) {
            gp <- seq_len(n)[strata == is]
            output[, gp] <- if (length(gp) == 1) rep(gp, R) else bsample(gp, R*length(gp))
        }
    }
    output
}

permutation.array <- function(n, R, strata)
{
#
# R x n array of bootstrap indices, permuted within strata.
# This is similar to ordinary array except that resampling is
# done without replacement in each row.
#
    output <- matrix(rep(seq_len(n), R), n, R)
    inds <- as.integer(names(table(strata)))
    for(is in inds) {
        group <- seq_len(n)[strata == is]
        if (length(group) > 1L) {
            g <- apply(output[group,  ], 2L, rperm)
            output[group,  ] <- g
        }
    }
    t(output)
}


cv.glm <- function(data, glmfit, cost=function(y,yhat) mean((y-yhat)^2),
		   K=n)
{
# cross-validation estimate of error for glm prediction with K groups.
# cost is a function of two arguments: the observed values and the
# the predicted values.
    call <- match.call()
    if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
    seed <- get(".Random.seed", envir=.GlobalEnv, inherits = FALSE)
    n <- nrow(data)
    out <- NULL
    if ((K > n) || (K <= 1))
        stop("'K' outside allowable range")
    K.o <- K
    K <- round(K)
    kvals <- unique(round(n/(1L:floor(n/2))))
    temp <- abs(kvals-K)
    if (!any(temp == 0))
        K <- kvals[temp == min(temp)][1L]
    if (K!=K.o) warning(gettextf("'K' has been set to %f", K), domain = NA)
    f <- ceiling(n/K)
    s <- sample0(rep(1L:K, f), n)
    n.s <- table(s)
#    glm.f <- formula(glmfit)
    glm.y <- glmfit$y
    cost.0 <- cost(glm.y, fitted(glmfit))
    ms <- max(s)
    CV <- 0
    Call <- glmfit$call
    for(i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        ## we want data from here but formula from the parent.
        Call$data <- data[j.in, , drop=FALSE]
        d.glm <- eval.parent(Call)
        p.alpha <- n.s[i]/n
        cost.i <- cost(glm.y[j.out],
                       predict(d.glm, data[j.out, , drop=FALSE],
                               type = "response"))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha *
            cost(glm.y, predict(d.glm, data, type = "response"))
    }
    list(call = call, K = K,
         delta = as.numeric(c(CV, CV + cost.0)),  # drop any names
         seed = seed)
}


boot.ci <- function(boot.out,conf = 0.95,type = "all",
		    index = 1L:min(2L, length(boot.out$t0)),
		    var.t0 = NULL ,var.t = NULL, t0 = NULL, t = NULL,
		    L = NULL, h = function(t) t,
                    hdot = function(t) rep(1, length(t)),
		    hinv = function(t) t, ...)
#
#  Main function to calculate bootstrap confidence intervals.
#  This function calls a number of auxilliary functions to do
#  the actual calculations depending on the type of interval(s)
#  requested.
#
{
    call <- match.call()
#  Get and transform the statistic values and their variances,
    if ((is.null(t) && !is.null(t0)) ||
        (!is.null(t) && is.null(t0)))
        stop("'t' and 't0' must be supplied together")
    t.o <- t; t0.o <- t0
#    vt.o <- var.t
    vt0.o <- var.t0
    if (is.null(t)) {
        if (length(index) == 1L) {
            t0 <- boot.out$t0[index]
            t <- boot.out$t[,index]
        }
        else if (ncol(boot.out$t)<max(index)) {
            warning("index out of bounds; minimum index only used.")
            index <- min(index)
            t0 <- boot.out$t0[index]
            t <- boot.out$t[,index]
        }
        else {
            t0 <- boot.out$t0[index[1L]]
            t <- boot.out$t[,index[1L]]
            if (is.null(var.t0)) var.t0 <- boot.out$t0[index[2L]]
            if (is.null(var.t)) var.t <- boot.out$t[,index[2L]]
        }
    }
    if (const(t, min(1e-8, mean(t, na.rm=TRUE)/1e6))) {
        print(paste("All values of t are equal to ", mean(t, na.rm=TRUE),
                    "\n Cannot calculate confidence intervals"))
        return(NULL)
    }
    if (length(t) != boot.out$R)
        stop(gettextf("'t' must of length %d", boot.out$R), domain = NA)
    if (is.null(var.t))
        fins <- seq_along(t)[is.finite(t)]
    else {
        fins <- seq_along(t)[is.finite(t) & is.finite(var.t)]
        var.t <- var.t[fins]
    }
    t <- t[fins]
    R <- length(t)
    if (!is.null(var.t0)) var.t0 <- var.t0*hdot(t0)^2
    if (!is.null(var.t))  var.t <- var.t*hdot(t)^2
    t0 <- h(t0); t <- h(t)
    if (missing(L)) L <- boot.out$L
    output <- list(R = R, t0 = hinv(t0), call = call)
    #  Now find the actual intervals using the methods listed in type
    if (any(type == "all" | type == "norm"))
        output <- c(output,
                    list(normal = norm.ci(boot.out, conf,
                         index[1L], var.t0=vt0.o, t0=t0.o, t=t.o,
                         L=L, h=h, hdot=hdot, hinv=hinv)))
    if (any(type == "all" | type == "basic"))
        output <- c(output, list(basic=basic.ci(t0,t,conf,hinv=hinv)))
    if (any(type == "all" | type == "stud")) {
        if (length(index)==1L)
            warning("bootstrap variances needed for studentized intervals")
        else
            output <- c(output, list(student=stud.ci(c(t0,var.t0),
                                     cbind(t ,var.t), conf, hinv=hinv)))
    }
    if (any(type == "all" | type == "perc"))
        output <- c(output, list(percent=perc.ci(t,conf,hinv=hinv)))
    if (any(type == "all" | type == "bca")) {
        if (as.character(boot.out$call[1L]) == "tsboot")
            warning("BCa intervals not defined for time series bootstraps")
        else
            output <- c(output, list(bca=bca.ci(boot.out,conf,
                                     index[1L],L=L,t=t.o, t0=t0.o,
                                     h=h,hdot=hdot, hinv=hinv, ...)))
    }
    class(output) <- "bootci"
    output
}

print.bootci <- function(x, hinv = NULL, ...) {
#
#  Print the output from boot.ci
#
    ci.out <- x
    cl <- ci.out$call
    ntypes <- length(ci.out)-3L
    nints <- nrow(ci.out[[4L]])
    t0 <- ci.out$t0
    if (!is.null(hinv)) t0 <- hinv(t0)  #
#  Find the number of decimal places which should be used
    digs <- ceiling(log10(abs(t0)))
    if (digs <= 0) digs <- 4
    else if (digs >= 4) digs <- 0
    else digs <- 4-digs
    intlabs <- NULL
    basrg <- strg <- perg <- bcarg <- NULL
    if (!is.null(ci.out$normal))
        intlabs <- c(intlabs,"     Normal        ")
    if (!is.null(ci.out$basic)) {
        intlabs <- c(intlabs,"     Basic         ")
        basrg <- range(ci.out$basic[,2:3]) }
    if (!is.null(ci.out$student)) {
        intlabs <- c(intlabs,"   Studentized     ")
        strg <- range(ci.out$student[,2:3]) }
    if (!is.null(ci.out$percent)) {
        intlabs <- c(intlabs,"    Percentile     ")
        perg <- range(ci.out$percent[,2:3]) }
    if (!is.null(ci.out$bca)) {
        intlabs <- c(intlabs,"      BCa          ")
        bcarg <- range(ci.out$bca[,2:3]) }
    level <- 100*ci.out[[4L]][, 1L]
    if (ntypes == 4L) n1 <- n2 <- 2L
    else if (ntypes == 5L) {n1 <- 3L; n2 <- 2L}
    else {n1 <- ntypes; n2 <- 0L}
    ints1 <- matrix(NA,nints,2L*n1+1L)
    ints1[,1L] <- level
    n0 <- 4L
#  Re-organize the intervals and coerce them into character data
    for (i in n0:(n0+n1-1)) {
        j <- c(2L*i-6L,2L*i-5L)
        nc <- ncol(ci.out[[i]])
        nc <- c(nc-1L,nc)
        if (is.null(hinv))
            ints1[,j] <- ci.out[[i]][,nc]
        else	ints1[,j] <- hinv(ci.out[[i]][,nc])
    }
    n0 <- 4L+n1
    ints1 <- format(round(ints1,digs))
    ints1[,1L] <- paste("\n",level,"%  ",sep="")
    ints1[,2*(1L:n1)] <- paste("(",ints1[,2*(1L:n1)],",",sep="")
    ints1[,2*(1L:n1)+1L] <- paste(ints1[,2*(1L:n1)+1L],")  ")
    if (n2 > 0) {
        ints2 <- matrix(NA,nints,2L*n2+1L)
        ints2[,1L] <- level
        j <- c(2L,3L)
        for (i in n0:(n0+n2-1L)) {
            if (is.null(hinv))
                ints2[,j] <- ci.out[[i]][,c(4L,5L)]
            else	ints2[,j] <- hinv(ci.out[[i]][,c(4L,5L)])
            j <- j+2L
        }
        ints2 <- format(round(ints2,digs))
        ints2[,1L] <- paste("\n",level,"%  ",sep="")
        ints2[,2*(1L:n2)] <- paste("(",ints2[,2*(1L:n2)],",",sep="")
        ints2[,2*(1L:n2)+1L] <- paste(ints2[,2*(1L:n2)+1L],")  ")
    }
    R <- ci.out$R                       #
#  Print the intervals
    cat("BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS\n")
    cat(paste("Based on",R,"bootstrap replicates\n\n"))
    cat("CALL : \n")
    dput(cl, control=NULL)
    cat("\nIntervals : ")
    cat("\nLevel",intlabs[1L:n1])
    cat(t(ints1))
    if (n2 > 0) {
        cat("\n\nLevel",intlabs[(n1+1):(n1+n2)])
        cat(t(ints2))
    }
    if (!is.null(cl$h)) {
        if (is.null(cl$hinv) && is.null(hinv))
            cat("\nCalculations and Intervals on ",
                "Transformed Scale\n")
        else	cat("\nCalculations on Transformed Scale;",
                    " Intervals on Original Scale\n")
    }
    else if (is.null(cl$hinv) && is.null(hinv))
        cat("\nCalculations and Intervals on Original Scale\n")
    else 	cat("\nCalculations on Original Scale",
                    " but Intervals Transformed\n")#
#  Print any warnings about extreme values.
    if (!is.null(basrg)) {
        if ((basrg[1L] <= 1) || (basrg[2L] >= R))
            cat("Warning : Basic Intervals used Extreme Quantiles\n")
        if ((basrg[1L] <= 10) || (basrg[2L] >= R-9))
            cat("Some basic intervals may be unstable\n")
    }
    if (!is.null(strg)) {
        if ((strg[1L] <= 1) || (strg[2L] >= R))
            cat("Warning : Studentized Intervals used Extreme Quantiles\n")
        if ((strg[1L] <= 10) || (strg[2L] >= R-9))
            cat("Some studentized intervals may be unstable\n")
    }
    if (!is.null(perg)) {
        if ((perg[1L] <= 1) || (perg[2L] >= R))
            cat("Warning : Percentile Intervals used Extreme Quantiles\n")
        if ((perg[1L] <= 10) || (perg[2L] >= R-9))
            cat("Some percentile intervals may be unstable\n")
    }
    if (!is.null(bcarg)) {
        if ((bcarg[1L] <= 1) || (bcarg[2L] >= R))
            cat("Warning : BCa Intervals used Extreme Quantiles\n")
        if ((bcarg[1L] <= 10) || (bcarg[2L] >= R-9))
            cat("Some BCa intervals may be unstable\n")
    }
    invisible(ci.out)
}

norm.ci <-
    function(boot.out = NULL,conf = 0.95,index = 1,var.t0 = NULL, t0 = NULL,
             t = NULL, L = NULL, h = function(t) t, hdot = function(t) 1,
             hinv = function(t) t)
#
#  Normal approximation method for confidence intervals.  This can be
#  used with or without a bootstrap object.  If a bootstrap object is
#  given then the intervals are bias corrected and the bootstrap variance
#  estimate can be used if none is supplied.
#
{
    if (is.null(t0))  {
        if (!is.null(boot.out)) t0 <-boot.out$t0[index]
        else stop("bootstrap output object or 't0' required")
    }
    if (!is.null(boot.out) && is.null(t))
        t <- boot.out$t[,index]
    if (!is.null(t)) {
        fins <- seq_along(t)[is.finite(t)]
        t <- h(t[fins])
    }
    if (is.null(var.t0)) {
        if (is.null(t)) {
            if (is.null(L))
                stop("unable to calculate 'var.t0'")
            else	var.t0 <- sum((hdot(t0)*L/length(L))^2)
        }
        else	var.t0 <- var(t)
    }
    else	var.t0 <- var.t0*hdot(t0)^2
    t0 <- h(t0)
    if (!is.null(t))
        bias <- mean(t)-t0
    else	bias <- 0
    merr <- sqrt(var.t0)*qnorm((1+conf)/2)
    out <- cbind(conf,hinv(t0-bias-merr),hinv(t0-bias+merr))
    out
}

norm.inter <- function(t,alpha)
#
#  Interpolation on the normal quantile scale.  For a non-integer
#  order statistic this function interpolates between the surrounding
#  order statistics using the normal quantile scale.  See equation
#  5.8 of Davison and Hinkley (1997)
#
{
    t <- t[is.finite(t)]
    R <- length(t)
    rk <- (R+1)*alpha
    if (!all(rk>1 & rk<R))
        warning("extreme order statistics used as endpoints")
    k <- trunc(rk)
    inds <- seq_along(k)
    out <- inds
    kvs <- k[k>0 & k<R]
    tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs+1))))
    ints <- (k == rk)
    if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
    out[k == 0] <- tstar[1L]
    out[k == R] <- tstar[R]
    not <- function(v) xor(rep(TRUE,length(v)),v)
    temp <- inds[not(ints) & k != 0 & k != R]
    temp1 <- qnorm(alpha[temp])
    temp2 <- qnorm(k[temp]/(R+1))
    temp3 <- qnorm((k[temp]+1)/(R+1))
    tk <- tstar[k[temp]]
    tk1 <- tstar[k[temp]+1L]
    out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)
    cbind(round(rk, 2), out)
}

basic.ci <- function(t0, t, conf = 0.95, hinv = function(t) t)
#
#  Basic bootstrap confidence method
#
{
    qq <- norm.inter(t,(1+c(conf,-conf))/2)
    cbind(conf, matrix(qq[,1L],ncol=2L), matrix(hinv(2*t0-qq[,2L]),ncol=2L))
}

stud.ci <- function(tv0, tv, conf = 0.95, hinv=function(t) t)
#
#  Studentized version of the basic bootstrap confidence interval
#
{
    if ((length(tv0) < 2) || (ncol(tv) < 2)) {
        warning("variance required for studentized intervals")
        NA
    } else {
        z <- (tv[,1L]-tv0[1L])/sqrt(tv[,2L])
        qq <- norm.inter(z, (1+c(conf,-conf))/2)
        cbind(conf, matrix(qq[,1L],ncol=2L),
              matrix(hinv(tv0[1L]-sqrt(tv0[2L])*qq[,2L]),ncol=2L))
    }
}

perc.ci <- function(t, conf = 0.95, hinv = function(t) t)
#
#  Bootstrap Percentile Confidence Interval Method
#
{
    alpha <- (1+c(-conf,conf))/2
    qq <- norm.inter(t,alpha)
    cbind(conf,matrix(qq[,1L],ncol=2L),matrix(hinv(qq[,2]),ncol=2L))
}

bca.ci <-
    function(boot.out,conf = 0.95,index = 1,t0 = NULL,t = NULL, L = NULL,
             h = function(t) t, hdot = function(t) 1, hinv = function(t) t,
             ...)
#
#  Adjusted Percentile (BCa) Confidence interval method.  This method
#  uses quantities calculated from the empirical influence values to
#  improve on the precentile interval.  Usually the required order
#  statistics for this method will not be integers and so norm.inter
#  is used to find them.
#
{
    t.o <- t
    if (is.null(t) || is.null(t0)) {
        t <- boot.out$t[,index]
        t0 <- boot.out$t0[index]
    }
    t <- t[is.finite(t)]
    w <- qnorm(sum(t < t0)/length(t))
    if (!is.finite(w)) stop("estimated adjustment 'w' is infinite")
    alpha <- (1+c(-conf,conf))/2
    zalpha <- qnorm(alpha)
    if (is.null(L))
        L <- empinf(boot.out, index=index, t=t.o, ...)
    a <- sum(L^3)/(6*sum(L^2)^1.5)
    if (!is.finite(a)) stop("estimated adjustment 'a' is NA")
    adj.alpha <- pnorm(w + (w+zalpha)/(1-a*(w+zalpha)))
    qq <- norm.inter(t,adj.alpha)
    cbind(conf, matrix(qq[,1L],ncol=2L), matrix(hinv(h(qq[,2L])),ncol=2L))
}



abc.ci <- function(data, statistic, index = 1, strata = rep(1, n), conf = 0.95,
                   eps = 0.001/n, ...)
#
#   Non-parametric ABC method for constructing confidence intervals.
#
{
    y <- data
    n <- NROW(y)
    strata1 <- tapply(strata,as.numeric(strata))
    if (length(index) != 1L) {
	warning("only first element of 'index' used in 'abc.ci'")
        index <- index[1L]
    }
    S <- length(table(strata1))
    mat <- matrix(0,n,S)
    for (s in 1L:S) {
        gp <- seq_len(n)[strata1 == s]
        mat[gp,s] <- 1
    }
#  Calculate the observed value of the statistic
    w.orig <- rep(1/n,n)
    t0 <- statistic(y,w.orig/(w.orig%*%mat)[strata1], ...)[index]#
#  Now find the linear and quadratic empirical influence values through
#  numerical differentiation
    L <- L2 <- numeric(n)
    for (i in seq_len(n)) {
        w1 <- (1-eps)*w.orig
        w1[i] <- w1[i]+eps
        w2 <- (1+eps)*w.orig
        w2[i] <- w2[i] - eps
        t1 <- statistic(y,w1/(w1%*%mat)[strata1], ...)[index]
        t2 <- statistic(y,w2/(w2%*%mat)[strata1], ...)[index]
        L[i] <- (t1-t2)/(2*eps)
        L2[i] <- (t1-2*t0+t2)/eps^2
    }
#  Calculate the required quantities for the intervals
    temp1 <- sum(L*L)
    sigmahat <- sqrt(temp1)/n
    ahat <- sum(L^3)/(6*temp1^1.5)      # called a in the text
    bhat <- sum(L2)/(2*n*n)             # called b in the text
    dhat <- L/(n*n*sigmahat)            # called k in the text
    w3 <- w.orig+eps*dhat
    w4 <- w.orig-eps*dhat
    chat <- (statistic(y,w3/(w3%*%mat)[strata1], ...)[index]-2*t0 +
             statistic(y,w4/(w4%*%mat)[strata1], ...)[index]) /
                 (2*eps*eps*sigmahat)   # called c in the text
    bprime <- ahat-(bhat/sigmahat-chat) # called w in the text
    alpha <- (1+as.vector(rbind(-conf,conf)))/2
    zalpha <- qnorm(alpha)
    lalpha <- (bprime+zalpha)/(1-ahat*(bprime+zalpha))^2#
#  Finally calculate the interval endpoints by calling the statistic with
#  various weight vectors.
    out <- seq(alpha)
    for (i in seq_along(alpha)) {
        w.fin <- w.orig+lalpha[i]*dhat
        out[i] <- statistic(y,w.fin/(w.fin%*%mat)[strata1], ...)[index]
    }
    out <- cbind(conf,matrix(out,ncol=2L,byrow=TRUE))
    if (length(conf) == 1L) out <- as.vector(out)
    out
}

censboot <-
    function(data, statistic, R, F.surv, G.surv, strata = matrix(1, n, 2),
             sim = "ordinary", cox = NULL, index = c(1, 2), ...,
             parallel = c("no", "multicore", "snow"),
             ncpus = getOption("boot.ncpus", 1L), cl = NULL)
{
#
#  Bootstrap replication for survival data.  Possible resampling
#  schemes are case, model-based, conditional bootstrap (with or without
#  a model) and the weird bootstrap.
#
    mstrata <- missing(strata)
    if (any(is.na(data)))
        stop("missing values not allowed in 'data'")
    if ((sim != "ordinary") && (sim != "model") && (sim != "cond")
        && (sim != "weird")) stop("unknown value of 'sim'")
    if ((sim == "model") && (is.null(cox))) sim <- "ordinary"
    if (missing(parallel)) parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") have_snow <- TRUE
        if (!have_mc && !have_snow) ncpus <- 1L
    }
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    call <- match.call()
    if (isMatrix(data)) n <- nrow(data)
    else stop("'data' must be a matrix with at least 2 columns")
    if (ncol(data) < 2L)
        stop("'data' must be a matrix with at least 2 columns")
    if (length(index) < 2L)
        stop("'index' must contain 2 elements")
    if (length(index) > 2L) {
        warning("only first 2 elements of 'index' used")
        index <- index[1L:2L]
    }
    if (ncol(data) < max(index))
        stop("indices are incompatible with 'ncol(data)'")
    if (sim == "weird") {
        if (!is.null(cox))
            stop("sim = \"weird\" cannot be used with a \"coxph\" object")
        if (ncol(data) > 2L)
            warning(gettextf("only columns %s and %s of 'data' used",
                             index[1L], index[2L]), domain = NA)
        data <- data[,index]
    }
    if (!is.null(cox) && is.null(cox$coefficients) &&
        ((sim == "cond") || (sim == "model"))) {
        warning("no coefficients in Cox model -- model ignored")
        cox <- NULL
    }
    if ((sim != "ordinary")  && missing(F.surv))
        stop("'F.surv' is required but missing")
    if (missing(G.surv) && ((sim == "cond") || (sim == "model")))
        stop("'G.surv' is required but missing")
    if (NROW(strata) != n) stop("'strata' of wrong length")
    if (!isMatrix(strata)) {
        if (!((sim == "weird") || (sim == "ordinary")))
            strata <- cbind(strata, 1)
    } else {
        if ((sim == "weird") || (sim == "ordinary")) strata <- strata[, 1L]
        else  strata <- strata[, 1L:2L]
    }
    temp.str <- strata
    strata <- if (isMatrix(strata))
        apply(strata, 2L, function(s, n) tapply(seq_len(n), as.numeric(s)), n)
    else  tapply(seq_len(n), as.numeric(strata))
    t0 <- if ((sim == "weird") && !mstrata) statistic(data, temp.str, ...)
    else  statistic(data, ...)
    ## Calculate the resampled data sets.  For ordinary resampling this
    ## involves finding the matrix of indices of the case to be resampled.
    ## For the conditional bootstrap or model-based we must find an array
    ## consisting of R matrices containing the resampled times and their
    ## censoring indicators.  The data sets for the weird bootstrap must be
    ## calculated individually.
    fn <- if (sim == "ordinary") {
        bt <- cens.case(n, strata, R)
        function(r) statistic(data[sort(bt[r, ]), ], ...)
    } else if (sim == "weird") {
        ## force promises
        data; F.surv
        if (!mstrata) {
            function(r) {
                bootdata <- cens.weird(data, F.surv, strata)
                statistic(bootdata[, 1:2], bootdata[, 3L], ...)
            }
        } else  {
            function(r) {
                bootdata <- cens.weird(data, F.surv, strata)
                statistic(bootdata[, 1:2], ...)
            }
        }
    } else {
        bt <- cens.resamp(data, R, F.surv, G.surv, strata, index, cox, sim)
        function(r) {
            bootdata <- data
            bootdata[, index] <- bt[r, , ]
            oi <- order(bt[r, , 1L], 1-bt[r, , 2L])
            statistic(bootdata[oi, ], ...)
        }
    }
    rm(mstrata)

    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(R), fn, ..., mc.cores = ncpus)
        } else if (have_snow) {
            list(...) # evaluate any promises
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
                if(RNGkind()[1L] == "L'Ecuyer-CMRG")
                    parallel::clusterSetRNGStream(cl)
                parallel::clusterEvalQ(cl, library(survival))
                res <- parallel::parLapply(cl, seq_len(R), fn)
                parallel::stopCluster(cl)
                res
            } else {
                parallel::clusterEvalQ(cl, library(survival))
                parallel::parLapply(cl, seq_len(R), fn)
            }
       }
    } else lapply(seq_len(R), fn)

    t <- matrix(, R, length(t0))
    for(r in seq_len(R)) t[r, ] <- res[[r]]

    cens.return(sim, t0, t, temp.str, R, data, statistic, call, seed)
}

cens.return <- function(sim, t0, t, strata, R, data, statistic, call, seed) {
#
#  Create an object of class "boot" from the output of a censored bootstrap.
#
    out <- list(t0 = t0, t = t, R = R, sim = sim, data = data, seed = seed,
                statistic = statistic, strata = strata, call = call)
    class(out) <- "boot"
    out
}

cens.case <- function(n, strata, R) {
#
#  Simple case resampling.
#
    out <- matrix(NA, nrow = R, ncol = n)
    for (s in seq_along(table(strata))) {
        inds <- seq_len(n)[strata == s]
        ns <- length(inds)
        out[, inds] <- bsample(inds,  ns*R)
    }
    out
}


cens.weird <- function(data, surv, strata) {
#
#  The weird bootstrap.  Censoring times are fixed and the number of
#  failures at each failure time are sampled from a binomial
#  distribution.  See Chapter 3 of Davison and Hinkley (1997).
#
#  data is a two column matrix containing the times and censoring
#    indicator.
#  surv is a survival object giving the failure time distribution.
#  strata is a the strata vector used in surv or a vector of 1's if no
#    strata were used.
#
    m <- length(surv$time)
    if (is.null(surv$strata)) {
        nstr <- 1
        str <- rep(1, m)
    } else {
        nstr <- length(surv$strata)
        str <- rep(1L:nstr, surv$strata)
    }
    n.ev <- rbinom(m, surv$n.risk, surv$n.event/surv$n.risk)
    while (any(tapply(n.ev, str, sum) == 0))
        n.ev <- rbinom(m, surv$n.risk, surv$n.event/surv$n.risk)
    times <- rep(surv$time, n.ev)
    str <- rep(str, n.ev)
    out <- NULL
    for (s in 1L:nstr) {
        temp <- cbind(times[str == s], 1)
        temp <- rbind(temp,
                      as.matrix(data[(strata == s&data[, 2L] == 0), , drop=FALSE]))
        temp <- cbind(temp, s)
        oi <- order(temp[, 1L], 1-temp[, 2L])
        out <- rbind(out, temp[oi, ])
    }
    if (is.data.frame(data)) out <- as.data.frame(out)
    out
}



cens.resamp <- function(data, R, F.surv, G.surv, strata, index = c(1,2),
                        cox = NULL, sim = "model")
{
#
#  Other types of resampling for the censored bootstrap.  This function
#  uses some local functions to implement the conditional bootstrap for
#  censored data and resampling based on a Cox regression model.  This
#  latter method of sampling can also use conditional sampling to get the
#  censoring times.
#
#  data is the data set
#  R is the number of replicates
#  F.surv is a survfit object for the failure time distribution
#  G.surv is a survfit object for the censoring time distribution
#  strata is a two column matrix, the first column gives the strata
#     gives the strata for the failure times and the second for the
#     censoring times.
#  index is a vector with two integer components giving the position
#     of the times and censoring indicators in data
#  cox is an object returned by the coxph function to give the Cox
#     regression model for the failure times.
#  sim is the simulation type which will always be "model" or "cond"
#
    gety1 <- function(n, R, surv, inds) {
# Sample failure times from the product limit estimate of the failure
# time distribution.
        survival <- surv$surv[inds]
        time <- surv$time[inds]
        n1 <- length(time)
        if (survival[n1] > 0L) {
            survival <- c(survival, 0)
            time <- c(time, Inf)
        }
        probs <- diff(-c(1, survival))
        matrix(bsample(time, n*R, prob = probs), R, n)
    }
    gety2 <- function(n, R, surv, eta, inds) {
# Sample failure times from the Cox regression model.
        F0 <- surv$surv[inds]
        time <- surv$time[inds]
        n1 <- length(time)
        if (F0[n1] > 0) {
            F0 <- c(F0, 0)
            time <- c(time, Inf)
        }
        ex <- exp(eta)
        Fh <- 1 - outer(F0, ex, "^")
        apply(rbind(0, Fh), 2L,
              function(p, y, R) bsample(y, R, prob = diff(p)), time, R)
    }
    getc1 <- function(n, R, surv, inds) {
# Sample censoring times from the product-limit estimate of the
# censoring distribution.
        cens <- surv$surv[inds]
        time <- surv$time[inds]
        n1 <- length(time)
        if (cens[n1] > 0) {
            cens <- c(cens, 0)
            time <- c(time, Inf)
        }
        probs <- diff(-c(1, cens))
        matrix(bsample(time, n*R, prob = probs), nrow = R)
    }
    getc2 <- function(n, R, surv, inds, data, index) {
# Sample censoring times form the conditional distribution.  If a failure
# was observed then sample from the product-limit estimate of the censoring
# distribution conditional on the time being greater than the observed
# failure time.  If the observation is censored then resampled time is the
# observed censoring time.
        cens <- surv$surv[inds]
        time <- surv$time[inds]
        n1 <- length(time)
        if (cens[n1] > 0) {
            cens <- c(cens, 0)
            time <- c(time, Inf)
        }
        probs <- diff(-c(1, cens))
        cout <- matrix(NA, R, n)
        for (i in seq_len(n)) {
            if (data[i, 2] == 0) cout[, i] <- data[i, 1L]
            else {
                pri <- probs[time > data[i, 1L]]
                ti <- time[time > data[i, 1L]]
                if (length(ti) == 1L) cout[, i] <- ti
                else cout[, i] <- bsample(ti, R, prob = pri)
            }
        }
        cout
    }
    n <- nrow(data)
    Fstart <- 1
    Fstr <- F.surv$strata
    if (is.null(Fstr)) Fstr <- length(F.surv$time)
    Gstart <- 1
    Gstr <- G.surv$strata
    if (is.null(Gstr)) Gstr <- length(G.surv$time)
    out <- array(NA, c(R, n, 2))
    y0 <- matrix(NA, R, n)
    for (s in seq_along(table(strata[, 1L]))) {
# Find the resampled failure times within strata for failures
        ns <- sum(strata[, 1L] == s)
        inds <- Fstart:(Fstr[s]+Fstart-1)
        y0[, strata[, 1L] == s] <- if (is.null(cox)) gety1(ns, R, F.surv, inds)
        else  gety2(ns, R, F.surv, cox$linear.predictors[strata[, 1L] == s], inds)
        Fstart <- Fstr[s]+Fstart
    }
    c0 <- matrix(NA, R, n)
    for (s in seq_along(table(strata[, 2L]))) {
# Find the resampled censoring times within strata for censoring times
        ns <- sum(strata[, 2] == s)
        inds <- Gstart:(Gstr[s]+Gstart-1)
        c0[, strata[, 2] == s] <- if (sim != "cond") getc1(ns, R, G.surv, inds)
        else  getc2(ns, R, G.surv, inds, data[strata[,2] == s, index])
        Gstart <- Gstr[s]+Gstart
    }
    infs <- (is.infinite(y0) & is.infinite(c0))
    if (sum(infs) > 0) {
# If both the resampled failure time and the resampled censoring time
# are infinite then set the resampled time to be a failure at the largest
# failure time in the failure time stratum containing the observation.
        evs <- seq_len(n)[data[, index[2L]] == 1]
        maxf <- tapply(data[evs, index[1L]], strata[evs, 1L], max)
        maxf <- matrix(maxf[strata[, 1L]], nrow = R, ncol = n, byrow = TRUE)
        y0[infs] <- maxf[infs]
    }
    array(c(pmin(y0, c0), 1*(y0 <= c0)), c(dim(y0), 2))
}

empinf <- function(boot.out = NULL, data = NULL, statistic = NULL,
                   type = NULL, stype = NULL ,index = 1, t = NULL,
                   strata = rep(1, n), eps = 0.001, ...)
{
#
#   Calculation of empirical influence values.  Possible types are
#   "inf" = infinitesimal jackknife (numerical differentiation)
#   "reg" = regression based estimation
#   "jack" = usual jackknife estimates
#   "pos" = positive jackknife estimates
#
    if (!is.null(boot.out))
    {
	if (boot.out$sim == "parametric")
            stop("influence values cannot be found from a parametric bootstrap")
        data <- boot.out$data
        if (is.null(statistic))
            statistic <- boot.out$statistic
        if (is.null(stype))
            stype <- boot.out$stype
        if (!is.null(boot.out$strata))
            strata <- boot.out$strata
    }
    else
    {
	if (is.null(data))
            stop("neither 'data' nor bootstrap object specified")
        if (is.null(statistic))
            stop("neither 'statistic' nor bootstrap object specified")
        if (is.null(stype)) stype <- "w"
    }
    n <- NROW(data)
    if (is.null(type)) {
        if (!is.null(t)) type <- "reg"
        else if (stype == "w") type <- "inf"
        else if (!is.null(boot.out) &&
                 (boot.out$sim != "parametric") &&
                 (boot.out$sim != "permutation")) type <- "reg"
        else type <- "jack"
    }

    if (type == "inf") {
# calculate the infinitesimal jackknife values by numerical differentiation
    	if (stype !="w") stop("'stype' must be \"w\" for type=\"inf\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        if (!is.null(t))
            warning("input 't' ignored; type=\"inf\"")
        L <- inf.jack(data, statistic, index, strata, eps, ...)
    } else if (type == "reg") {
# calculate the regression estimates of the influence values
        if (is.null(boot.out))
            stop("bootstrap object needed for type=\"reg\"")
        if (is.null(t)) {
            if (length(index) != 1L) {
                warning("only first element of 'index' used")
                index <- index[1L]
            }
            t <- boot.out$t[,index]
        }
        L <- empinf.reg(boot.out, t)
    } else if (type == "jack") {
        if (!is.null(t))
            warning("input 't' ignored; type=\"jack\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        L <- usual.jack(data, statistic, stype, index, strata, ...)
    } else if (type == "pos") {
        if (!is.null(t))
            warning("input 't' ignored; type=\"pos\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        L <- positive.jack(data, statistic, stype, index, strata, ...)
    }
    L
}

inf.jack <-
    function(data, stat, index = 1, strata  =  rep(1, n), eps  =  0.001, ...)
{
#
#   Numerical differentiation to get infinitesimal jackknife estimates
#   of the empirical influence values.
#
    n <- NROW(data)
    L <- seq_len(n)
    eps <- eps/n
    strata <- tapply(strata, as.numeric(strata))
    w.orig <- 1/table(strata)[strata]
    tobs <- stat(data, w.orig, ...)[index]
    for(i in seq_len(n)) {
        group <- seq_len(n)[strata == strata[i]]
        w <- w.orig
        w[group] <- (1 - eps)*w[group]
        w[i] <- w[i] + eps
        L[i] <- (stat(data, w, ...)[index] - tobs)/eps
    }
    L
}

empinf.reg <- function(boot.out, t = boot.out$t[,1L])
#
#  Function to estimate empirical influence values using regression.
#  This method regresses the observed bootstrap values on the bootstrap
#  frequencies to estimate the empirical influence values
#
{
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    R <- length(t)
    n <- NROW(boot.out$data)
    strata <- boot.out$strata
    if (is.null(strata))
        strata <- rep(1,n)
    else 	strata <- tapply(strata,as.numeric(strata))
    ns <- table(strata)
#    S <- length(ns)
    f <- boot.array(boot.out)[fins,]
    X <- f/matrix(ns[strata], R, n ,byrow=TRUE)
    out <- tapply(seq_len(n), strata, min)
    inc <- seq_len(n)[-out]
    X <- X[,inc]
    beta <- coefficients(glm(t ~ X))[-1L]
    l <- rep(0, n)
    l[inc] <- beta
    l <- l - tapply(l,strata,mean)[strata]
    l
}

usual.jack <- function(data, stat, stype = "w", index = 1,
                       strata = rep(1, n), ...)
#
#  Function to use the normal (delete 1) jackknife method to estimate the
#  empirical influence values
#
{
    n <- NROW(data)
    l <- rep(0,n)
    strata <- tapply(strata,as.numeric(strata))
    if (stype == "w") {
        w0 <- rep(1, n)/table(strata)[strata]
        tobs <- stat(data, w0, ...)[index]
        for (i in seq_len(n)) {
            w1 <- w0
            w1[i] <- 0
            gp <- strata == strata[i]
            w1[gp] <- w1[gp]/sum(w1[gp])
            l[i] <- (sum(gp)-1)*(tobs - stat(data,w1, ...)[index])
        }
    } else if (stype == "f") {
        f0 <- rep(1,n)
        tobs <- stat(data, f0, ...)[index]
        for (i in seq_len(n)) {
            f1 <- f0
            f1[i] <- 0
            gp <- strata == strata[i]
            l[i] <- (sum(gp)-1)*(tobs - stat(data, f1, ...)[index])
        }
    } else {
        i0 <- seq_len(n)
        tobs <- stat(data, i0, ...)[index]
        for (i in seq_len(n)) {
            i1 <- i0[-i]
            gp <- strata == strata[i]
            l[i] <- (sum(gp)-1)*(tobs - stat(data, i1, ...)[index])
        }
    }
    l
}

positive.jack <- function(data, stat, stype = "w", index = 1,
                          strata = rep(1 ,n), ...)
{
#
#  Use the positive jackknife to estimate the empirical influence values.
#  The positive jackknife includes one observation twice to find its
#  influence.
#
    strata <- tapply(strata,as.numeric(strata))
    n <- NROW(data)
    L <- rep(0, n)
    if (stype == "w") {
        w0 <- rep(1, n)/table(strata)[strata]
        tobs <- stat(data, w0, ...)[index]
        for (i in seq_len(n)) {
            st1 <- c(strata,strata[i])
            w1 <- 1/table(st1)[strata]
            w1[i] <- 2*w1[i]
            gp <- strata == strata[i]
            w1[gp] <- w1[gp]/sum(w1[gp])
            L[i] <- (sum(gp)+1)*(stat(data, w1, ...)[index] - tobs)
        }
    } else if (stype == "f") {
        f0 <- rep(1,n)
        tobs <- stat(data, f0, ...)[index]
        for (i in seq_len(n)) {
            f1 <- f0
            f1[i] <- 2
            gp <- strata == strata[i]
            L[i] <- (sum(gp)+1)*(stat(data, f1, ...)[index] - tobs)
        }
    } else if (stype == "i") {
        i0 <- seq_len(n)
        tobs <- stat(data, i0, ...)[index]
        for (i in seq_len(n)) {
            i1 <- c(i0, i)
            gp <- strata == strata[i]
            L[i] <- (sum(gp)+1)*(stat(data, i1, ...)[index] - tobs)
        }
    }
    L
}

linear.approx <- function(boot.out, L = NULL, index = 1, type = NULL,
                          t0 = NULL, t = NULL, ...)
#
#  Find the linear approximation to the bootstrap replicates of a
#  statistic.  L should be the linear influence values which will
#  be found by empinf if they are not supplied.
#
{
    f <- boot.array(boot.out)
    n <- length(f[1,  ])
    if ((length(index) > 1L) && (is.null(t0) || is.null(t))) {
        warning("only first element of 'index' used")
        index <- index[1L]
    }
    if (is.null(t0)) {
        t0 <- boot.out$t0[index]
        if (is.null(L))
            L <- empinf(boot.out, index=index, type=type, ...)
    } else if (is.null(t) && is.null(L)) {
        warning("input 't0' ignored: neither 't' nor 'L' supplied")
        t0 <- t0[index]
        L <- empinf(boot.out, index=index, type=type, ...)
    }
    else if (is.null(L))
        L <- empinf(boot.out, type=type, t=t, ...)
    tL <- rep(t0, boot.out$R)
    strata <- boot.out$strata
    if (is.null(strata))
        strata <- rep(1, n)
    else 	strata <- tapply(strata,as.numeric(strata))
    S <- length(table(strata))
    for(s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        tL <- tL + f[, i.s] %*% L[i.s]/length(i.s)
    }
    as.vector(tL)
}

envelope <-
    function(boot.out = NULL, mat = NULL, level = 0.95, index = 1L:ncol(mat))
#
#  Function to estimate pointwise and overall confidence envelopes for
#  a function.
#
#  mat is a matrix of bootstrap values of the function at a number of
#     points.  The points at which they are evaluated are assumed to
#     be constant over the rows.
#
{
    emperr <- function(rmat, p = 0.05, k = NULL)
#  Local function to estimate the overall error rate of an envelope.
    {
        R <- nrow(rmat)
        if (is.null(k)) k <- p*(R+1)/2 else p <- 2*k/(R+1)
        kf <- function(x, k, R) 1*((min(x) <= k)|(max(x) >= R+1L-k))
        c(k, p, sum(apply(rmat, 1L, kf, k, R))/(R+1))
    }
    kfun <- function(x, k1, k2)
# Local function to find the cut-off points in each column of the matrix.
        sort(x ,partial = sort(c(k1, k2)))[c(k1, k2)]
    if (!is.null(boot.out) && isMatrix(boot.out$t)) mat <- boot.out$t
    if (!isMatrix(mat)) stop("bootstrap output matrix missing")
    n <- ncol(mat)
    if (length(index) < 2L) stop("use 'boot.ci' for scalar parameters")
    mat <- mat[,index]
    rmat <- apply(mat,2L,rank)
    R <- nrow(mat)
    if (length(level) == 1L) level <- rep(level,2L)
    k.pt <- floor((R+1)*(1-level[1L])/2+1e-10)
    k.pt <- c(k.pt, R+1-k.pt)
    err.pt <- emperr(rmat,k = k.pt[1L])
    ov <- emperr(rmat,k = 1)
    ee <- err.pt
    al <- 1-level[2L]
    if (ov[3L] > al)
        warning("unable to achieve requested overall error rate")
    else {
        continue <- !(ee[3L] < al)
        while(continue) {
#  If the observed error is greater than the level required for the overall
#  envelope then try another envelope.  This loop uses linear interpolation
#  on the integers between 1 and k.pt[1L] to find the required value.
            kk <- ov[1L]+round((ee[1L]-ov[1L])*(al-ov[3L])/ (ee[3L]-ov[3L]))
            if (kk == ov[1L]) kk <- kk+1
            else if (kk == ee[1L]) kk <- kk-1
            temp <- emperr(rmat, k = kk)
            if (temp[3L] > al) ee <- temp
            else ov <- temp
            continue <- !(ee[1L] == ov[1L]+1)
        }
    }
    k.ov <- c(ov[1L], R+1-ov[1L])
    err.ov <- ov[-1L]
    out <- apply(mat, 2L, kfun, k.pt, k.ov)
    list(point = out[2:1,], overall = out[4:3,], k.pt = k.pt,
         err.pt = err.pt[-1L], k.ov = k.ov, err.ov = err.ov, err.nom = 1-level)
}


glm.diag <- function(glmfit)
{
#
#  Calculate diagnostics for objects of class "glm".  The diagnostics
#  calculated are various types of residuals as well as the Cook statistics
#  and the leverages.
#
    w <- if (is.null(glmfit$prior.weights)) rep(1,length(glmfit$residuals))
         else glmfit$prior.weights
    sd <- switch(family(glmfit)$family[1L],
                 "gaussian" = sqrt(glmfit$deviance/glmfit$df.residual),
                 "Gamma" = sqrt(sum(w*(glmfit$y/fitted(glmfit) - 1)^2)/
                           glmfit$df.residual),
                 1)
##     sd <- ifelse(family(glmfit)$family[1L] == "gaussian",
##                  sqrt(glmfit$deviance/glmfit$df.residual), 1)
##     sd <- ifelse(family(glmfit)$family[1L] == "Gamma",
##                  sqrt(sum(w*(glmfit$y/fitted(glmfit) - 1)^2)/glmfit$df.residual), sd)
    dev <- residuals(glmfit, type = "deviance")/sd
    pear <- residuals(glmfit, type = "pearson")/sd
    ## R change: lm.influence drops 0-wt cases.
    h <- rep(0, length(w))
    h[w != 0] <- lm.influence(glmfit)$hat
    p <- glmfit$rank
    rp <- pear/sqrt(1 - h)
    rd <- dev/sqrt(1 - h)
    cook <- (h * rp^2)/((1 - h) * p)
    res <- sign(dev) * sqrt(dev^2 + h * rp^2)
    list(res = res, rd = rd, rp = rp, cook = cook, h = h, sd = sd)
}


glm.diag.plots <-
    function(glmfit, glmdiag = glm.diag(glmfit), subset  =  NULL,
             iden = FALSE, labels = NULL, ret = FALSE)
{
#  Diagnostic plots for objects of class "glm"
    if (is.null(glmdiag))
        glmdiag <- glm.diag(glmfit)
    if (is.null(subset))
        subset <- seq_along(glmdiag$h)
    else if (is.logical(subset))
        subset <- seq_along(subset)[subset]
    else if (is.numeric(subset) && all(subset<0))
        subset <- (1L:(length(subset)+length(glmdiag$h)))[subset]
    else if (is.character(subset)) {
        if (is.null(labels)) labels <- subset
        subset <- seq_along(subset)
    }
#	close.screen(all = T)
#	split.screen(c(2, 2))
#	screen(1) #
    par(mfrow = c(2,2))
#  Plot the deviance residuals against the fitted values
    x1 <- predict(glmfit)
    plot(x1, glmdiag$res, xlab = "Linear predictor", ylab = "Residuals")
    pars <- vector(4L, mode="list")
    pars[[1L]] <- par("usr")
#	screen(2) #
#  Plot a normal QQ plot of the standardized deviance residuals
    y2 <- glmdiag$rd
    x2 <- qnorm(ppoints(length(y2)))[rank(y2)]
    plot(x2, y2, ylab = "Quantiles of standard normal",
         xlab = "Ordered deviance residuals")
    abline(0, 1, lty = 2)
    pars[[2L]] <- par("usr")
#	screen(3) #
#  Plot the Cook statistics against h/(1-h) and draw line to highlight
#  possible influential and high leverage points.
    hh <- glmdiag$h/(1 - glmdiag$h)
    plot(hh, glmdiag$cook, xlab = "h/(1-h)", ylab = "Cook statistic")
    rx <- range(hh)
    ry <- range(glmdiag$cook)
    rank.fit <- glmfit$rank
    nobs <- rank.fit + glmfit$df.residual
    cooky <- 8/(nobs - 2 * rank.fit)
    hy <- (2 * rank.fit)/(nobs - 2 * rank.fit)
    if ((cooky >= ry[1L]) && (cooky <= ry[2L])) abline(h = cooky, lty = 2)
    if ((hy >= rx[1L]) && (hy <= rx[2L])) abline(v = hy, lty = 2)
    pars[[3L]] <- par("usr")
#	screen(4) #
#  Plot the Cook statistics against the observation number in the original
#  data set.
    plot(subset, glmdiag$cook, xlab = "Case", ylab = "Cook statistic")
    if ((cooky >= ry[1L]) && (cooky <= ry[2L])) abline(h = cooky, lty = 2)
    xx <- list(x1,x2,hh,subset)
    yy <- list(glmdiag$res, y2, glmdiag$cook, glmdiag$cook)
    pars[[4L]] <- par("usr")

    if (is.null(labels)) labels <- names(x1)
    while (iden) {
#  If interaction with the plots is required then ask the user which plot
#  they wish to interact with and then run identify() on that plot.
#  When the user terminates identify(), reprompt until no further interaction
#  is required and the user inputs a 0.
        cat("****************************************************\n")
        cat("Please Input a screen number (1,2,3 or 4)\n")
        cat("0 will terminate the function \n")
#		num <- scan(nmax=1)
        num <- as.numeric(readline())
        if ((length(num) > 0L) &&
            ((num == 1)||(num == 2)||(num == 3)||(num == 4))) {
            cat(paste("Interactive Identification for screen",
                      num,"\n"))
            cat("left button = Identify, center button = Exit\n")
#			screen(num, new=F)
            nm <- num+1
            par(mfg = c(trunc(nm/2),1 +nm%%2, 2, 2))
            par(usr = pars[[num]])
            identify(xx[[num]], yy[[num]], labels)
        }
        else 	iden <- FALSE
    }
#	close.screen(all=T)
    par(mfrow = c(1, 1))
    if (ret) glmdiag else invisible()
}

exp.tilt <- function(L, theta = NULL, t0 = 0, lambda = NULL,
                     strata = rep(1, length(L)) )
{
# exponential tilting of linear approximation to statistic
# to give mean theta.
#
    tilt.dis <- function(lambda)  {
#  Find the squared error in the mean using the multiplier lambda
#  This is then minimized to find the correct value of lambda
#  Note that the function should have minimum 0.
        L <- para[[2L]]
        theta <- para[[1L]]
        strata <- para[[3L]]
        ns <- table(strata)
        tilt <- rep(NA, length(L) )
        for (s in seq_along(ns)) {
            p <- exp(lambda*L[strata == s]/ns[s])
            tilt[strata == s] <- p/sum(p)
        }
        (sum(L*tilt) - theta)^2
    }
    tilted.prob <- function(lambda, L, strata)  {
#  Find the tilted probabilities for a given value of lambda
        ns <- table(strata)
        m <- length(lambda)
        tilt <- matrix(NA, m, length(L))
        for (i in 1L:m)
            for (s in seq_along(ns)) {
                p <- exp(lambda[i]*L[strata == s]/ns[s])
                tilt[i,strata == s] <- p/sum(p)
            }
        if (m == 1) tilt <- as.vector(tilt)
        tilt
    }
    strata <- tapply(strata, as.numeric(strata))
    if (!is.null(theta)) {
        theta <- theta-t0
        m <- length(theta)
        lambda <- rep(NA,m)
        for (i in 1L:m) {
            para <- list(theta[i],L,strata)
#			assign("para",para,frame=1)
#			lambda[i] <- nlmin(tilt.dis, 0 )$x
            lambda[i] <- optim(0, tilt.dis, method = "BFGS")$par
            msd <- tilt.dis(lambda[i])
            if (is.na(msd) || (abs(msd) > 1e-6))
                stop(gettextf("unable to find multiplier for %f", theta[i]),
                     domain = NA)
        }
    }
    else if (is.null(lambda))
        stop("'theta' or 'lambda' required")
    probs <- tilted.prob( lambda, L, strata )
    if (is.null(theta)) theta <- t0 + sum(probs * L)
    else theta <- theta+t0
    list(p = probs, theta = theta, lambda = lambda)
}


imp.weights <- function(boot.out, def = TRUE, q = NULL)
{
#
# Takes boot.out object and calculates importance weights
# for each element of boot.out$t, as if sampling from multinomial
# distribution with probabilities q.
# If q is NULL the weights are calculated as if
# sampling from a distribution with equal probabilities.
# If def=T calculates weights using defensive mixture
# distribution, if F uses weights knowing from which element of
# the mixture they come.
#
    R <- boot.out$R
    if (length(R) == 1L)
        def <- FALSE
    f <- boot.array(boot.out)
    n <- ncol(f)
    strata <- tapply(boot.out$strata,as.numeric(boot.out$strata))
#    ns <- table(strata)
    if (is.null(q))  q <- rep(1,ncol(f))
    if (any(q == 0)) stop("0 elements not allowed in 'q'")
    p <- boot.out$weights
    if ((length(R) == 1L) && all(abs(p - q)/p < 1e-10))
        return(rep(1, R))
    np <- length(R)
    q <- normalize(q, strata)
    lw.q <- as.vector(f %*% log(q))
    if (!isMatrix(p))
        p <- as.matrix(t(p))
    p <- t(apply(p, 1L, normalize, strata))
    lw.p <- matrix(NA, sum(R), np)
    for(i in 1L:np) {
        zz <- seq_len(n)[p[i,  ] > 0]
        lw.p[, i] <- f[, zz] %*% log(p[i, zz])
    }
    if (def)
        w <- 1/(exp(lw.p - lw.q) %*% R/sum(R))
    else {
        i <- cbind(seq_len(sum(R)), rep(seq_along(R), R))
        w <- exp(lw.q - lw.p[i])
    }
    as.vector(w)
}

const <- function(w, eps=1e-8) {
# Are all of the values of w equal to within the tolerance eps.
    all(abs(w-mean(w, na.rm=TRUE)) < eps)
}

imp.moments <- function(boot.out=NULL, index=1, t=boot.out$t[,index],
			w=NULL, def=TRUE, q=NULL )
{
# Calculates raw, ratio, and regression estimates of mean and
# variance of t using importance sampling weights in w.
    if (missing(t) && is.null(boot.out$t))
        stop("bootstrap replicates must be supplied")
    if (is.null(w))
        if (!is.null(boot.out))
            w <- imp.weights(boot.out, def, q)
        else	stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && missing(t)) {
        warning("only first element of 'index' used")
        t <- boot.out$t[,index[1L]]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    if (!const(w)) {
        y <- t*w
        m.raw <- mean( y )
        m.rat <- sum( y )/sum( w )
        t.lm <- lm( y~w )
        m.reg <- mean( y ) - coefficients(t.lm)[2L]*(mean(w)-1)
        v.raw <- mean(w*(t-m.raw)^2)
        v.rat <- sum(w/sum(w)*(t-m.rat)^2)
        x <- w*(t-m.reg)^2
        t.lm2 <- lm( x~w )
        v.reg <- mean( x ) - coefficients(t.lm2)[2L]*(mean(w)-1)
    }
    else {	m.raw <- m.rat <- m.reg <- mean(t)
		v.raw <- v.rat <- v.reg <- var(t)
            }
    list( raw=c(m.raw,v.raw), rat = c(m.rat,v.rat),
         reg = as.vector(c(m.reg,v.reg)))
}


imp.reg <- function(w)
{
#  This function takes a vector of importance sampling weights and
#  returns the regression importance sampling weights.  The function
#  is called by imp.prob and imp.quantiles to enable those functions
#  to find regression estimates of tail probabilities and quantiles.
    if (!const(w)) {
        R <- length(w)
        mw <- mean(w)
        s2w <- (R-1)/R*var(w)
        b <- (1-mw)/s2w
        w <- w*(1+b*(w-mw))/R
    }
    cumsum(w)/sum(w)
}


imp.quantile <- function(boot.out=NULL, alpha=NULL, index=1,
			t=boot.out$t[,index], w=NULL, def=TRUE, q=NULL )
{
# Calculates raw, ratio, and regression estimates of alpha quantiles
#  of t using importance sampling weights in w.
    if (missing(t) && is.null(boot.out$t))
        stop("bootstrap replicates must be supplied")
    if (is.null(alpha)) alpha <- c(0.01,0.025,0.05,0.95,0.975,0.99)
    if (is.null(w))
        if (!is.null(boot.out))
            w <- imp.weights(boot.out, def, q)
        else	stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && missing(t)){
        warning("only first element of 'index' used")
        t <- boot.out$t[,index[1L]]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    o <- order(t)
    t <- t[o]
    w <- w[o]
    cum <- cumsum(w)
    o <- rev(o)
    w.m <- w[o]
    t.m <- -rev(t)
    cum.m <- cumsum(w.m)
    cum.rat <- cum/mean(w)
    cum.reg <- imp.reg(w)
    R <- length(w)
    raw <- rat <- reg <- rep(NA,length(alpha))
    for (i in seq_along(alpha)) {
        if (alpha[i]<=0.5) raw[i] <-  max(t[cum<=(R+1)*alpha[i]])
        else raw[i] <- -max(t.m[cum.m<=(R+1)*(1-alpha[i])])
        rat[i] <- max(t[cum.rat <= (R+1)*alpha[i]])
        reg[i] <- max(t[cum.reg <= (R+1)*alpha[i]])
    }
    list(alpha=alpha, raw=raw, rat=rat, reg=reg)
}

imp.prob <- function(boot.out=NULL, index=1, t0=boot.out$t0[index],
			t=boot.out$t[,index], w=NULL,  def=TRUE, q=NULL)
{
# Calculates raw, ratio, and regression estimates of tail probability
#  pr( t <= t0 ) using importance sampling weights in w.
    is.missing <- function(x) length(x) == 0L || is.na(x)

    if (missing(t) && is.null(boot.out$t))
        stop("bootstrap replicates must be supplied")
    if (is.null(w))
        if (!is.null(boot.out))
            w <- imp.weights(boot.out, def, q)
        else	stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && (missing(t) || missing(t0))) {
        warning("only first element of 'index' used")
        index <- index[1L]
        if (is.missing(t)) t <- boot.out$t[,index]
        if (is.missing(t0)) t0 <- boot.out$t0[index]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    o <- order(t)
    t <- t[o]
    w <- w[o]
    raw <- rat <- reg <- rep(NA,length(t0))
    cum <- cumsum(w)/sum(w)
    cum.r <- imp.reg(w)
    for (i in seq_along(t0)) {
        raw[i] <-sum(w[t<=t0[i]])/length(w)
        rat[i] <- max(cum[t<=t0[i]])
        reg[i] <- max(cum.r[t<=t0[i]])
    }
    list(t0=t0, raw=raw, rat=rat, reg=reg )
}

smooth.f <- function(theta, boot.out, index=1, t=boot.out$t[,index],
			width=0.5 )
{
# Does frequency smoothing of the frequency array for boot.out with
# bandwidth A to give frequencies for 'typical' distribution at theta
    if ((length(index) > 1L) && missing(t)) {
        warning("only first element of 'index' used")
        t <- boot.out$t[,index[1L]]
    }
    if (isMatrix(t)) {
        warning("only first column of 't' used")
        t <- t[,1L]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    m <- length(theta)
    v <- imp.moments(boot.out, t=t)$reg[2L]
    eps <- width*sqrt(v)
    if (m  == 1)
        w <- dnorm((theta-t)/eps )/eps
    else {
        w <- matrix(0,length(t),m)
        for (i in 1L:m)
            w[,i] <- dnorm((theta[i]-t)/eps )/eps
    }
    f <- crossprod(boot.array(boot.out)[fins,] , w)
    strata <- boot.out$strata
    strata <- tapply(strata, as.numeric(strata))
    ns <- table(strata)
    out <- matrix(NA,ncol(f),nrow(f))
    for (s in seq_along(ns)) {
        ts <- matrix(f[strata == s,],m,ns[s],byrow=TRUE)
        ss <- apply(ts,1L,sum)
        out[,strata == s] <-  ts/matrix(ss,m,ns[s])
    }
    if (m == 1) out <- as.vector(out)
    out
}

tilt.boot <- function(data, statistic, R, sim="ordinary",
		stype="i", strata = rep(1, n), L = NULL, theta=NULL,
		alpha=c(0.025,0.975), tilt=TRUE, width=0.5, index=1, ... )
{
#  Does tilted bootstrap sampling of stat applied to data with strata strata
#  and simulation type sim.
#  The levels of R give the number of simulations at each level.  For example,
#  R=c(199,100,50) will give three separate bootstraps with 199, 100, 50
#  simulations.  If R[1L]>0 the first simulation is assumed to be untilted
#  and L can be estimated from it by regression, or it can be frequency
#  smoothed to give probabilities p.
#  If tilt=T use exponential tilting with empirical influence value L
#  given explicitly or estimated from boot0, but if tilt=F
#  (in which case R[1L] should be large) frequency smoothing of boot0 is used
#  with bandwidth A.
#  Tilting/frequency smoothing is to theta (so length(theta)=length(R)-1).
#  The function assumes at present that q=0 is the median of the distribution
#  of t*.
    if ((sim != "ordinary") && (sim != "balanced"))
        stop("invalid value of 'sim' supplied")
    if (!is.null(theta) && (length(R) != length(theta)+1))
        stop("'R' and 'theta' have incompatible lengths")
    if (!tilt && (R[1L] == 0))
        stop("R[1L] must be positive for frequency smoothing")
    call <- match.call()
    n <- NROW(data)
    if (R[1L]>0) {
# If required run an initial bootstrap with equal weights.
        if (is.null(theta) && (length(R) != length(alpha)+1))
            stop("'R' and 'alpha' have incompatible lengths")
        boot0 <- boot(data, statistic, R = R[1L], sim=sim, stype=stype,
                      strata = strata, ... )
        if (is.null(theta)) {
            if (any(c(alpha,1-alpha)*(R[1L]+1) <= 5))
                warning("extreme values used for quantiles")
            theta <- quantile(boot0$t[,index],alpha)
        }
    }
    else {
# If no initial bootstrap is run then exponential tilting must be
# used.  Also set up a dummy bootstrap object to hold the output.
        tilt <- TRUE
        if (is.null(theta))
            stop("'theta' must be supplied if R[1L] = 0")
        if (!missing(alpha))
            warning("'alpha' ignored; R[1L] = 0")
        if (stype == "i") orig <- seq_len(n)
        else if (stype == "f") orig <- rep(1,n)
        else orig <- rep(1,n)/n
        boot0 <- boot.return(sim=sim,t0=statistic(data,orig, ...),
                             t=NULL, strata=strata, R=0, data=data,
                             stat=statistic, stype=stype,call=NULL,
                             seed=get(".Random.seed", envir=.GlobalEnv, inherits = FALSE),
                             m=0,weights=NULL)
    }
# Calculate the weights for the subsequent bootstraps
    if (is.null(L) & tilt)
        if (R[1L] > 0) L <- empinf(boot0, index, ...)
        else L <- empinf(data=data, statistic=statistic, stype=stype,
                         index=index, ...)
    if (tilt) probs <- exp.tilt(L, theta, strata=strata, t0=boot0$t0[index])$p
    else probs <- smooth.f(theta, boot0, index, width=width)#
# Run the weighted bootstraps and collect the output.
    boot1 <- boot(data, statistic, R[-1L], sim=sim, stype=stype,
                  strata=strata, weights=probs, ...)
    boot0$t <- rbind(boot0$t, boot1$t)
    boot0$weights <- rbind(boot0$weights, boot1$weights)
    boot0$R <- c(boot0$R, boot1$R)
    boot0$call <- call
    boot0$theta <- theta
    boot0
}


control <- function(boot.out, L=NULL, distn=NULL, index=1, t0=NULL, t=NULL,
                    bias.adj=FALSE, alpha=NULL, ... )
{
#
#  Control variate estimation.  Post-simulation balance can be used to
#  find the adjusted bias estimate.  Alternatively the linear approximation
#  to the statistic of interest can be used as a control variate and hence
#  moments and quantiles can be estimated.
#
    if (!is.null(boot.out$call$weights))
        stop("control methods undefined when 'boot.out' has weights")
    if (is.null(alpha))
        alpha <- c(1,2.5,5,10,20,50,80,90,95,97.5,99)/100
    tL <- dL <- bias <- bias.L <- var.L <- NULL
    k3.L <- q.out <- distn.L <- NULL
    stat <- boot.out$statistic
    data <- boot.out$data
    R <- boot.out$R
    f <- boot.array(boot.out)
    if (bias.adj) {
# Find the adjusted bias estimate using post-simulation balance.
        if (length(index) > 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        f.big <- apply(f, 2L, sum)
        if (boot.out$stype == "i")
        { 	n <- ncol(f)
                i.big <- rep(seq_len(n),f.big)
                t.big <- stat(data, i.big, ...)[index]
            }
        else if (boot.out$stype == "f")
            t.big <- stat(data, f.big, ...)[index]
        else if (boot.out$stype == "w")
            t.big <- stat(data, f.big/R, ...)[index]
        bias <- mean(boot.out$t[, index]) - t.big
        out <- bias
    }
    else {
# Using the linear approximation as a control variable, find estimates
# of the moments and quantiles of the statistic of interest.
        if (is.null(t) || is.null(t0)) {
            if (length(index) > 1L) {
                warning("only first element of 'index' used")
                index <- index[1L]
            }
            if (is.null(L))
                L <- empinf(boot.out, index=index, ...)
            tL <- linear.approx(boot.out, L, index, ...)
            t <- boot.out$t[,index]
            t0 <- boot.out$t0[index]
        }
        else {
            if (is.null(L))
                L <- empinf(boot.out, t=t, ...)
            tL <- linear.approx(boot.out, L, t0=t0, ...)
        }
        fins <- seq_along(t)[is.finite(t)]
        t <- t[fins]
        tL <- tL[fins]
        R <- length(t)
        dL <- t - tL                    #
# Find the moments of the statistic of interest.
        bias.L <- mean(dL)
        strata <- tapply(boot.out$strata, as.numeric(boot.out$strata))
        var.L <- var.linear(L, strata) + 2*var(tL, dL) + var(dL)
        k3.L <- k3.linear(L, strata) + 3 * cum3(tL, dL) +
            3 * cum3(dL, tL) + cum3(dL)
        if (is.null(distn)) {
# If distn is not supplied then calculate the saddlepoint approximation to
# the distribution of the linear approximation.
            distn <- saddle.distn((t0+L)/length(L),
                                  alpha = (1L:R)/(R + 1),
                                  t0=c(t0,sqrt(var.L)), strata=strata)
            dist.q <- distn$quantiles[,2]
            distn <- distn$distn
        }
        else	dist.q <- predict(distn, x=qnorm((1L:R)/(R+1)))$y#
# Use the quantiles of the distribution of the linear approximation and
# the control variates to estimate the quantiles of the statistic of interest.
        distn.L <- sort(dL[order(tL)] + dist.q)
        q.out <- distn.L[(R + 1) * alpha]
        out <- list(L=L, tL=tL, bias=bias.L, var=var.L, k3=k3.L,
                    quantiles=cbind(alpha,q.out), distn=distn)
    }
    out
}

var.linear <- function(L, strata = NULL)
{
#  estimate the variance of a statistic using its linear approximation
    vL <- 0
    n <- length(L)
    if (is.null(strata))
        strata <- rep(1, n)
    else 	strata <- tapply(seq_len(n),as.numeric(strata))
    S <- length(table(strata))
    for(s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        vL <- vL + sum(L[i.s]^2/length(i.s)^2)
    }
    vL
}

k3.linear <- function(L, strata = NULL)
{
#  estimate the skewness of a statistic using its linear approximation
    k3L <- 0
    n <- length(L)
    if (is.null(strata))
        strata <- rep(1, n)
    else	strata <- tapply(seq_len(n),as.numeric(strata))
    S <- length(table(strata))
    for(s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        k3L <- k3L + sum(L[i.s]^3/length(i.s)^3)
    }
    k3L
}

cum3 <- function(a, b=a, c=a, unbiased=TRUE)
# calculate third order cumulants.
{
    n <- length(a)
    if (unbiased) mult <- n/((n-1)*(n-2))
    else mult <- 1/n
    mult*sum((a - mean(a)) * (b - mean(b)) * (c - mean(c)))
}

logit <- function(p) qlogis(p)
#
#  Calculate the logit of a proportion in the range [0,1]
#
## {
##     out <- p
##     inds <- seq_along(p)[!is.na(p)]
##     if (any((p[inds] < 0) | (p[inds] > 1)))
##         stop("invalid proportions input")
##     out[inds] <- log(p[inds]/(1-p[inds]))
##     out[inds][p[inds] == 0] <- -Inf
##     out[inds][p[inds] == 1] <- Inf
##     out
## }

inv.logit <- function(x)
#
#  Calculate the inverse logit of a number
#
# {
#     out <- exp(x)/(1+exp(x))
#     out[x==-Inf] <- 0
#     out[x==Inf] <- 1
#     out
# }
plogis(x)

iden <- function(n)
#
#  Return the identity matrix of size n
#
    if (n > 0) diag(rep(1,n)) else NULL

zero <- function(n,m)
#
#  Return an n x m matrix of 0's
#
    if ((n > 0) & (m > 0)) matrix(0,n,m) else NULL


simplex <- function(a,A1=NULL,b1=NULL,A2=NULL,b2=NULL,A3=NULL,b3=NULL,
		maxi=FALSE, n.iter=n+2*m, eps=1e-10)
#
#   This function calculates the solution to a linear programming
#   problem using the tableau simplex method.  The constraints are
#   given by the matrices A1, A2, A3 and the vectors b1, b2 and b3
#   such that A1%*%x <= b1, A2%*%x >= b2 and A3%*%x = b3.  The 2-phase
#   Simplex method is used.
#
{
    call <- match.call()
    if (!is.null(A1))
        if (is.matrix(A1))
            m1 <- nrow(A1)
        else 	m1 <- 1
    else 	m1 <- 0
    if (!is.null(A2))
        if (is.matrix(A2))
            m2 <- nrow(A2)
        else 	m2 <- 1
    else 	m2 <- 0
    if (!is.null(A3))
        if (is.matrix(A3))
            m3 <- nrow(A3)
        else 	m3 <- 1
    else 	m3 <- 0
    m <- m1+m2+m3
    n <- length(a)
    a.o <- a
    if (maxi) a <- -a
    if (m2+m3 == 0)
# If there are no >= or = constraints then the origin is a feasible
# solution, and so only the second phase is required.
        out <- simplex1(c(a,rep(0,m1)), cbind(A1,iden(m1)), b1,
                        c(rep(0,m1),b1), n+(1L:m1), eps=eps)
    else {
        if (m2 > 0)
            out1 <- simplex1(c(a,rep(0,m1+2*m2+m3)),
                             cbind(rbind(A1,A2,A3),
                                   rbind(iden(m1),zero(m2+m3,m1)),
                                   rbind(zero(m1,m2),-iden(m2),
                                         zero(m3,m2)),
                                   rbind(zero(m1,m2+m3),
                                         iden(m2+m3))),
                             c(b1,b2,b3),
                             c(rep(0,n),b1,rep(0,m2),b2,b3),
                             c(n+(1L:m1),(n+m1+m2)+(1L:(m2+m3))),
                             stage=1, n1=n+m1+m2,
                             n.iter=n.iter, eps=eps)
        else
            out1 <- simplex1(c(a,rep(0,m1+m3)),
                             cbind(rbind(A1,A3),
                                   iden(m1+m3)),
                             c(b1,b3),
                             c(rep(0,n),b1,b3),
                             n+(1L:(m1+m3)), stage=1, n1=n+m1,
                             n.iter=n.iter, eps=eps)
#  In phase 1 use 1 artificial variable for each constraint and
#  minimize the sum of the artificial variables.  This gives a
#  feasible solution to the original problem as long as all
#  artificial variables are non-basic (and hence the value of the
#  new objective function is 0).  If this is true then optimize the
#  original problem using the result as the original feasible solution.
        if (out1$val.aux > eps)
            out <- out1
        else	out <- simplex1(out1$a[1L:(n+m1+m2)],
                                out1$A[,1L:(n+m1+m2)],
                                out1$soln[out1$basic],
                                out1$soln[1L:(n+m1+m2)],
                                out1$basic,
                                val=out1$value, n.iter=n.iter, eps=eps)
    }
    if (maxi)
        out$value <- -out$value
    out$maxi <- maxi
    if (m1 > 0L)
        out$slack <- out$soln[n+(1L:m1)]
    if (m2 > 0L)
        out$surplus <- out$soln[n+m1+(1L:m2)]
    if (out$solved == -1)
        out$artificial <- out$soln[-(1L:n+m1+m2)]
    out$obj <- a.o
    names(out$obj) <- paste("x",seq_len(n),sep="")
    out$soln <- out$soln[seq_len(n)]
    names(out$soln) <- paste("x",seq_len(n),sep="")
    out$call <- call
    class(out) <- "simplex"
    out
}




simplex1 <- function(a,A,b,init,basic,val=0,stage=2, n1=N, eps=1e-10,
                     n.iter=n1)
#
#  Tableau simplex function called by the simplex routine.  This does
#  the actual calculations required in each phase of the simplex method.
#
{
    pivot <- function(tab, pr, pc) {
#  Given the position of the pivot and the tableau, complete
#  the matrix operations to swap the variables.
        pv <- tab[pr,pc]
        pcv <- tab[,pc]
        tab[-pr,]<- tab[-pr,] - (tab[-pr,pc]/pv)%o%tab[pr,]
        tab[pr,] <- tab[pr,]/(-pv)
        tab[pr,pc] <- 1/pv
        tab[-pr,pc] <- pcv[-pr]/pv
        tab
    }
    N <- ncol(A)
    M <- nrow(A)
    nonbasic <- (1L:N)[-basic]
    tableau <- cbind(b,-A[,nonbasic,drop=FALSE])
#  If in the first stage then find the artifical objective function,
#  otherwise use the original objective function.
    if (stage == 2) {
        tableau <- rbind(tableau,c(val,a[nonbasic]))
        obfun <- a[nonbasic]
    }
    else {	obfun <- apply(tableau[(M+n1-N+1):M,,drop=FALSE],2L,sum)
		tableau <- rbind(c(val,a[nonbasic]),tableau,obfun)
		obfun <- obfun[-1L]
            }
    it <- 1
    while (!all(obfun> -eps) && (it <= n.iter))
#  While the objective function can be reduced
#	Find a pivot
#	complete the matrix operations required
#	update the lists of basic and non-basic variables
    {
        pcol <- 1+order(obfun)[1L]
        if (stage == 2)
            neg <- (1L:M)[tableau[1L:M,pcol]< -eps]
        else 	neg <- 1+ (1L:M)[tableau[2:(M+1),pcol] < -eps]
        ratios <- -tableau[neg,1L]/tableau[neg,pcol]
        prow <- neg[order(ratios)[1L]]
        tableau <- pivot(tableau,prow,pcol)
        if (stage == 1) {
            temp <- basic[prow-1L]
            basic[prow-1L] <- nonbasic[pcol-1L]
            nonbasic[pcol-1L] <- temp
            obfun <- tableau[M+2L,-1L]
        }
        else {	temp <- basic[prow]
                basic[prow] <- nonbasic[pcol-1L]
                nonbasic[pcol-1L] <- temp
                obfun <- tableau[M+1L,-1L]
            }
        it <- it+1
    }
#  END of while loop
    if (stage == 1) {
        val.aux <- tableau[M+2,1L]
# If the value of the auxilliary objective function is zero but some
# of the artificial variables are basic (with value 0) then switch
# them with some nonbasic variables (which are not artificial).
        if ((val.aux < eps) && any(basic>n1)) {
            ar <- (1L:M)[basic>n1]
            for (j in seq_along(temp)) {
                prow <- 1+ar[j]
                pcol <- 1 + order(
                                  nonbasic[abs(tableau[prow,-1L])>eps])[1L]
                tableau <- pivot(tableau,prow,pcol)
                temp1 <- basic[prow-1L]
                basic[prow-1L] <- nonbasic[pcol-1L]
                nonbasic[pcol-1L] <- temp1
            }
        }
        soln <- rep(0,N)
        soln[basic] <- tableau[2:(M+1L),1L]
        val.orig <- tableau[1L,1L]
        A.out <- matrix(0,M,N)
        A.out[,basic] <- iden(M)
        A.out[,nonbasic] <- -tableau[2L:(M+1L),-1L]
        a.orig <- rep(0,N)
        a.orig[nonbasic] <- tableau[1L,-1L]
        a.aux <- rep(0,N)
        a.aux[nonbasic] <- tableau[M+2,-1L]
        list(soln=soln, solved=-1, value=val.orig, val.aux=val.aux,
             A=A.out, a=a.orig, a.aux=a.aux, basic=basic)
    }
    else {
 	soln <- rep(0,N)
        soln[basic] <- tableau[1L:M,1L]
        val <- tableau[(M+1L),1L]
        A.out <- matrix(0,M,N)
        A.out[,basic] <- iden(M)
        A.out[,nonbasic] <- tableau[1L:M,-1L]
        a.out <- rep(0,N)
        a.out[nonbasic] <- tableau[M+1L,-1L]
        if (it <= n.iter) solved <- 1L
        else solved <- 0L
        list(soln=soln, solved=solved, value=val,  A=A.out,
             a=a.out, basic=basic)
    }
}

print.simplex <- function(x, ...) {
#
#  Print the output of a simplex solution to a linear programming problem.
#
    simp.out <- x
    cat("\nLinear Programming Results\n\n")
    cl <- simp.out$call
    cat("Call : ")
    dput(cl, control=NULL)
    if (simp.out$maxi) cat("\nMaximization ")
    else cat("\nMinimization ")
    cat("Problem with Objective Function Coefficients\n")
    print(simp.out$obj)
    if (simp.out$solved == 1) {
        cat("\n\nOptimal solution has the following values\n")
        print(simp.out$soln)
        cat(paste("The optimal value of the objective ",
                  " function is ",simp.out$value,".\n",sep=""))
    }
    else if (simp.out$solved == 0) {
        cat("\n\nIteration limit exceeded without finding solution\n")
        cat("The coefficient values at termination were\n")
        print(simp.out$soln)
        cat(paste("The objective function value was ",simp.out$value,
                  ".\n",sep=""))
    }
    else cat("\nNo feasible solution could be found\n")
    invisible(x)
}


saddle <-
    function(A = NULL, u = NULL, wdist = "m", type = "simp", d = NULL, d1 = 1,
             init = rep(0.1, d), mu = rep(0.5, n), LR = FALSE, strata = NULL,
             K.adj = NULL, K2 = NULL)
#
#  Saddle point function.  Standard multinomial saddlepoints are
#  computed using nlmin whereas the more complicated conditional
#  saddlepoints for Poisson and Binary cases are done by fitting
#  a GLM to a set of responses which, in turn, are derived from a
#  linear programming problem.
#
{
    det <- function(mat) {
#  absolute value of the determinant of a matrix.
        if (any(is.na(mat))) NA
        else if (!all(is.finite(mat))) Inf
        else  abs(prod(eigen(mat,only.values = TRUE)$values))
    }
    sgn <- function(x, eps = 1e-10)
#  sign of a real number.
        if (abs(x) < eps) 0 else 2*(x > 0) - 1

    if (!is.null(A)) {
        A <- as.matrix(A)
        d <- ncol(A)
        if (length(u) != d)
            stop(gettextf("number of columns of 'A' (%d) not equal to length of 'u' (%d)",
                          d, length(u)), domain = NA)
        n <- nrow(A)
    } else if (is.null(K.adj))
        stop("either 'A' and 'u' or 'K.adj' and 'K2' must be supplied")
    if (!is.null(K.adj)) {
#  If K.adj and K2 are supplied then calculate the simple saddlepoint.
        if (is.null(d)) d <- 1
        type <- "simp"
        wdist <- "o"
        speq <- suppressWarnings(optim(init, K.adj))
        if (speq$convergence == 0) {
            ahat <- speq$par
            Khat <- K.adj(ahat)
            K2hat <- det(K2(ahat))
            gs <- 1/sqrt((2*pi)^d*K2hat)*exp(Khat)
            if (d == 1) {
                r <- sgn(ahat)*sqrt(-2*Khat)
                v <- ahat*sqrt(K2hat)
                if (LR)	Gs <- pnorm(r)+dnorm(r)*(1/r + 1/v)
                else	Gs <- pnorm(r+log(v/r)/r)
            }
            else	Gs <- NA
        }
        else gs <- Gs <- ahat <- NA
    }
    else if (wdist == "m") {
#  Calculate the standard simple saddlepoint for the multinomial case.
        type <- "simp"
        if (is.null(strata)) {
            p <- mu/sum(mu)
            para <- list(p,A,u,n)
            K <- function(al) {
                w <- para[[1L]]*exp(al%*%t(para[[2L]]))
                para[[4L]]*log(sum(w))-sum(al*para[[3L]])
            }
            speq <- suppressWarnings(optim(init, K))
            ahat <- speq$par
            w <- as.vector(p*exp(ahat%*%t(A)))
            Khat <- n*log(sum(w))-sum(ahat*u)
            sw <- sum(w)
            if (d == 1)
                K2hat <- n*(sum(w*A*A)/sw-(sum(w*A)/sw)^2)
            else {
                saw <- w %*% A
                sa2w <- t(matrix(w,n,d)*A) %*% A
                K2hat <- det(n/sw*(sa2w-(saw%*%t(saw))/sw))
            }
        }
        else {
            sm <- as.vector(tapply(mu,strata,sum)[strata])
            p <- mu/sm
            ns <- table(strata)
            para <- list(p,A,u,strata,ns)
            K <- function(al) {
                w <- para[[1L]]*exp(al%*%t(para[[2L]]))
                sum(para[[5]]*log(tapply(w,para[[4L]],sum))) -
                    sum(al*para[[3L]])
            }
            speq <- suppressWarnings(optim(init, K))
            ahat <- speq$par
            w <- p*exp(ahat%*%t(A))
            Khat <- sum(ns*log(tapply(w,strata,sum)))-sum(ahat*u)
            temp <- matrix(0,d,d)
            for (s in seq_along(ns)) {
                gp <- seq_len(n)[strata == s]
                sw <- sum(w[gp])
                saw <- w[gp]%*%A[gp,]
                sa2w <- t(matrix(w[gp],ns[s],d)*A[gp,])%*%A[gp,]
                temp <- temp+ns[s]/sw*(sa2w-(saw%*%t(saw))/sw)
            }
            K2hat <- det(temp)
        }
        if (speq$convergence == 0) {
            gs <- 1/sqrt(2*pi*K2hat)^d*exp(Khat)
            if (d == 1) {
                r <- sgn(ahat)*sqrt(-2*Khat)
                v <- ahat*sqrt(K2hat)
                if (LR)	Gs <- pnorm(r)+dnorm(r)*(1/r - 1/v)
                else	Gs <- pnorm(r+log(v/r)/r)
            }
            else	Gs <- NA
        }
        else	gs <- Gs <- ahat <- NA
    } else if (wdist == "p") {
        if (type == "cond") {
#  Conditional Poisson and Binary saddlepoints are caculated by first
#  solving a linear programming problem and then fitting a generalized
#  linear model to find the solution to the saddlepoint equations.
            smp <- simplex(rep(0, n), A3 = t(A), b3 = u)
            if (smp$solved == 1) {
                y <- smp$soln
                A1 <- A[,1L:d1]
                A2 <- A[,-(1L:d1)]
                mod1 <- summary(glm(y ~ A1 + A2 + offset(log(mu)) - 1,
                                    poisson, control = glm.control(maxit=100)))
                mod2 <- summary(glm(y ~ A2 + offset(log(mu)) - 1,
                                    poisson, control = glm.control(maxit=100)))
                ahat <- mod1$coefficients[,1L]
                ahat2 <- mod2$coefficients[,1L]
                temp1 <- mod2$deviance - mod1$deviance
                temp2 <- det(mod2$cov.unscaled)/det(mod1$cov.unscaled)
                gs <- 1/sqrt((2*pi)^d1*temp2)*exp(-temp1/2)
                if (d1 == 1) {
                    r <- sgn(ahat[1L])*sqrt(temp1)
                    v <- ahat[1L]*sqrt(temp2)
                    if (LR)	Gs<-pnorm(r)+dnorm(r)*(1/r-1/v)
                    else	Gs <- pnorm(r+log(v/r)/r)
                }
                else	Gs <- NA
            }
            else {
                ahat <- ahat2 <- NA
                gs <- Gs <- NA
            }
        }
        else stop("this type not implemented for Poisson")
    }
    else if (wdist == "b") {
        if (type == "cond") {
            smp <- simplex(rep(0, n), A1 = iden(n), b1 = rep(1-2e-6, n),
                           A3 = t(A), b3 = u - 1e-6*apply(A, 2L, sum))
#  For the binary case we require that the values are in the interval (0,1)
#  since glm code seems to have problems when there are too many 0's or 1's.
            if (smp$solved == 1) {
                y <- smp$soln+1e-6
                A1 <- A[, 1L:d1]
                A2 <- A[, -(1L:d1)]
                mod1 <- summary(glm(cbind(y, 1-y) ~ A1+A2+offset(qlogis(mu))-1,
                                    binomial, control = glm.control(maxit=100)))
                mod2 <- summary(glm(cbind(y, 1-y) ~ A2+offset(qlogis(mu))-1,
                                    binomial, control = glm.control(maxit=100)))
                ahat <- mod1$coefficients[,1L]
                ahat2 <- mod2$coefficients[,1L]
                temp1 <- mod2$deviance-mod1$deviance
                temp2 <- det(mod2$cov.unscaled)/det(mod1$cov.unscaled)
                gs <- 1/sqrt((2*pi)^d1*temp2)*exp(-temp1/2)
                if (d1 == 1) {
                    r <- sgn(ahat[1L])*sqrt(temp1)
                    v <- ahat[1L]*sqrt(temp2)
                    if (LR)	Gs<-pnorm(r)+dnorm(r)*(1/r-1/v)
                    else	Gs <- pnorm(r+log(v/r)/r)
                }
                else	Gs <- NA
            }
            else {
                ahat <- ahat2 <- NA
                gs <- Gs <- NA
            }
        }
        else stop("this type not implemented for Binary")
    }
    if (type == "simp")
        out <- list(spa = c(gs, Gs), zeta.hat = ahat)
    else #if (type == "cond")
        out <- list(spa = c(gs, Gs), zeta.hat = ahat, zeta2.hat = ahat2)
    names(out$spa) <- c("pdf", "cdf")
    out
}


saddle.distn <-
    function(A, u = NULL, alpha = NULL, wdist = "m",
             type = "simp", npts = 20, t = NULL, t0 = NULL, init = rep(0.1, d),
             mu = rep(0.5, n), LR = FALSE, strata = NULL, ...)
#
#  This function calculates the entire saddlepoint distribution by
#  finding the saddlepoint approximations at npts values and then
#  fitting a spline to the results (on the normal quantile scale).
#  A may be a matrix or a function of t.  If A is a matrix with 1 column
#  u is not used (u = t), if A is a matrix with more than 1 column u must
#  be a vector with ncol(A)-1 elements, if A is a function of t then u
#  must also be a function returning a vector of ncol(A(t, ...)) elements.
{
    call <- match.call()
    if (is.null(alpha)) alpha <- c(0.001,0.005,0.01,0.025,0.05,0.1,0.2,0.5,
                                   0.8,0.9,0.95,0.975,0.99,0.995,0.999)
    if (is.null(t) && is.null(t0))
        stop("one of 't' or 't0' required")
    ep1 <- min(c(alpha,0.01))/10
    ep2 <- (1-max(c(alpha,0.99)))/10
    d <- if (type == "simp") 1
    else if (is.function(u)) {
        if (is.null(t)) length(u(t0[1L], ...)) else length(u(t[1L], ...))
    } else  1L+length(u)
    i <- nsads <- 0
    if (!is.null(t)) npts <- length(t)
    zeta <- matrix(NA,npts,2L*d-1L)
    spa <- matrix(NA,npts,2L)
    pts <- NULL
    if (is.function(A)) {
        n <- nrow(as.matrix(A(t0[1L], ...)))
        if (is.null(u)) stop("function 'u' missing")
        if (!is.function(u)) stop("'u' must be a function")
        if (is.null(t)) {
            t1 <- t0[1L]-2*t0[2L]
            sad <- saddle(A = A(t1, ...), u = u(t1, ...),
                          wdist = wdist, type = type, d1 = 1,
                          init = init, mu = mu, LR = LR, strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (sad$spa[2L] > ep1) ||
                   (sad$spa[2L] < ep1/100)) {
                nsads <- nsads+1
#  Find a lower bound on the effective range of the saddlepoint distribution
                if (!is.na(sad$spa[2L]) && (sad$spa[2L] > ep1)) {
                    i <- i+1
                    zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
                    spa[i,] <- sad$spa
                    pts <- c(pts,t1)
                    bdu <- t1
                }
                else	bdl <- t1
                if (nsads == npts)
                    stop("unable to find range")
                if (is.null(bdl)) {
                    t1 <- 2*t1-t0[1L]
                    sad <- saddle(A = A(t1, ...),
                                u = u(t1, ...), wdist = wdist,
                                type = type, d1 = 1, init = init,
                                mu = mu, LR = LR, strata = strata)
                }
                else if (is.null(bdu)) {
                    t1 <- (t0[1L]+bdl)/2
                    sad <- saddle(A = A(t1, ...),
                                u = u(t1, ...), wdist = wdist,
                                type = type, d1 = 1, init = init,
                                mu = mu, LR = LR, strata = strata)
                }
                else {
                    t1 <- (bdu+bdl)/2
                    sad <- saddle(A = A(t1, ...),
                                  u = u(t1, ...), wdist = wdist,
                                  type = type, d1 = 1, init = init,
                                  mu = mu, LR = LR, strata = strata)
                }
            }
            i1 <- i <- i+1
            nsads <- 0
            zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i,] <- sad$spa
            pts <- c(pts,t1)
            t2 <- t0[1L]+2*t0[2L]
            sad <- saddle(A = A(t2, ...), u = u(t2, ...),
                          wdist = wdist, type = type, d1 = 1, init = init,
                          mu = mu, LR = LR, strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (1-sad$spa[2L] > ep2) ||
                   (1-sad$spa[2L] < ep2/100)){
#  Find an upper bound on the effective range of the saddlepoint distribution
                nsads <- nsads+1
                if (!is.na(sad$spa[2L])&&(1-sad$spa[2L] > ep2)) {
                    i <- i+1
                    zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
                    spa[i,] <- sad$spa
                    pts <- c(pts,t2)
                    bdl <- t2
                }
                else	bdu <- t2
                if (nsads  == npts)
                    stop("unable to find range")
                if (is.null(bdu)) {
                    t2 <- 2*t2-t0[1L]
                    sad <- saddle(A = A(t2, ...),
                                u = u(t2, ...), wdist = wdist,
                                type = type, d1 = 1, init = init,
                                mu = mu, LR = LR, strata = strata)
                } else if (is.null(bdl)) {
                    t2 <- (t0[1L]+bdu)/2
                    sad <- saddle(A = A(t2, ...),
                                u = u(t2, ...), wdist = wdist,
                                type = type, d1 = 1, init = init,
                                mu = mu, LR = LR, strata = strata)
                } else {
                    t2 <- (bdu+bdl)/2
                    sad <- saddle(A = A(t2, ...),
                                  u = u(t2, ...), wdist = wdist,
                                  type = type, d1 = 1, init = init,
                                  mu = mu, LR = LR, strata = strata)
                }
            }
            i <- i+1
            zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i,] <- sad$spa
            pts <- c(pts,t2)
#  Now divide the rest of the npts points so that about half are at
#  either side of t0[1L].
            if ((npts %% 2) ==  0) {
                tt1<- seq.int(t1, t0[1L], length.out = npts/2-i1+2)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = npts/2+i1-i+2)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            } else {
                ex <- 1*(t1+t2 > 2*t0[1L])
                ll <- floor(npts/2)+2
                tt1 <- seq.int(t1, t0[1L], length.out = ll-i1+1-ex)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = ll+i1-i+ex)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
        }
        init1 <- init
        for (j in (i+1):npts) {
#  Calculate the saddlepoint approximations at the extra points.
            sad <- saddle(A = A(t[j-i], ...), u = u(t[j-i], ...),
                          wdist = wdist, type = type, d1 = 1,
                          init = init1, mu = mu, LR = LR,
                          strata = strata)
            zeta[j,] <- c(sad$zeta.hat, sad$zeta2.hat)
            init1 <- sad$zeta.hat
            spa[j,] <- sad$spa
        }
    }
    else {
        A <- as.matrix(A)
        n <- nrow(A)
        if (is.null(t)) {
#  Find a lower bound on the effective range of the saddlepoint distribution
            t1 <- t0[1L]-2*t0[2L]
            sad <- saddle(A = A, u = c(t1,u), wdist = wdist, type = type,
                          d = d, d1 = 1, init = init, mu = mu, LR = LR,
                          strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (sad$spa[2L] > ep1) ||
                   (sad$spa[2L] < ep1/100)) {
                if (!is.na(sad$spa[2L]) && (sad$spa[2L] > ep1)) {
                    i <- i+1
                    zeta[i,] <- c(sad$zeta.hat,
                                  sad$zeta2.hat)
                    spa[i,] <- sad$spa
                    pts <- c(pts,t1)
                    bdu <- t1
                }
                else	bdl <- t1
                if (i == floor(npts/2))
                    stop("unable to find range")
                if (is.null(bdl)) {
                    t1 <- 2*t1-t0[1L]
                    sad <- saddle(A = A, u = c(t1,u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                } else if (is.null(bdu)) {
                    t1 <- (t0[1L]+bdl)/2
                    sad <- saddle(A = A, u = c(t1,u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                } else {
                    t1 <- (bdu+bdl)/2
                    sad <- saddle(A = A, u = c(t1,u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                }
            }
            i1 <- i <- i+1
            zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i,] <- sad$spa
            pts <- c(pts,t1)
#  Find an upper bound on the effective range of the saddlepoint distribution
            t2 <- t0[1L]+2*t0[2L]
            sad <- saddle(A = A, u = c(t2,u), wdist = wdist, type = type,
                          d = d, d1 = 1, init = init, mu = mu, LR = LR,
                          strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (1-sad$spa[2L] > ep2) ||
                   (1-sad$spa[2L] < ep2/100)) {
                if (!is.na(sad$spa[2L])&&(1-sad$spa[2L] > ep2)) {
                    i <- i+1
                    zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
                    spa[i,] <- sad$spa
                    pts <- c(pts, t2)
                    bdl <- t2
                }
                else	bdu <- t2
                if ((i-i1) == floor(npts/2))
                    stop("unable to find range")
                if (is.null(bdu)) {
                    t2 <- 2*t2-t0[1L]
                    sad <- saddle(A = A, u = c(t2, u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                }
                else if (is.null(bdl)) {
                    t2 <- (t0[1L]+bdu)/2
                    sad <- saddle(A = A, u = c(t2, u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                }
                else {
                    t2 <- (bdu+bdl)/2
                    sad <- saddle(A = A, u = c(t2, u),
                                  wdist = wdist, type = type, d = d,
                                  d1 = 1, init = init, mu = mu, LR = LR,
                                  strata = strata)
                }
            }
            i <- i+1
            zeta[i,] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i,] <- sad$spa
            pts <- c(pts, t2)
#  Now divide the rest of the npts points so that about half are at
#  either side of t0[1L].
            if ((npts %% 2) == 0) {
                tt1 <- seq.int(t1, t0[1L], length.out=npts/2-i1+2)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out=npts/2+i1-i+2)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
            else {
                ex <- 1*(t1+t2 > 2*t0[1L])
                ll <- floor(npts/2)+2
                tt1 <- seq.int(t1, t0[1L], length.out=ll-i1+1-ex)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out=ll+i1-i+ex)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
        }
        init1 <- init
        for (j in (i+1):npts) {
#  Calculate the saddlepoint approximations at the extra points.
            sad <- saddle(A=A, u=c(t[j-i],u), wdist=wdist,
                          type=type, d=d, d1=1, init=init,
                          mu=mu, LR=LR, strata=strata)
            zeta[j,] <- c(sad$zeta.hat, sad$zeta2.hat)
            init1 <- sad$zeta.hat
            spa[j,] <- sad$spa
        }
    }
#  Omit points too close to the center as the distribution approximation is
#  not good at those points.
    pts.in <- (1L:npts)[(abs(zeta[,1L]) > 1e-6) &
                       (abs(spa[, 2L] - 0.5) < 0.5 - 1e-10)]
    pts <- c(pts,t)[pts.in]
    zeta <- as.matrix(zeta[pts.in, ])
    spa <- spa[pts.in, ]
#  Fit a spline to the approximations and predict at the required quantile
#  values.
    distn <- smooth.spline(qnorm(spa[,2]), pts)
    quantiles <- predict(distn, qnorm(alpha))$y
    quans <- cbind(alpha, quantiles)
    colnames(quans) <- c("alpha", "quantile")
    inds <- order(pts)
    psa <- cbind(pts[inds], spa[inds,], zeta[inds,])
    if (d == 1) anames <- "zeta"
    else {	anames <- rep("",2*d-1)
		for (j in 1L:d) anames[j] <- paste("zeta1.", j ,sep = "")
		for (j in (d+1):(2*d-1)) anames[j] <- paste("zeta2.", j-d, sep = "")
            }
    dimnames(psa) <- list(NULL,c("t", "gs", "Gs", anames))
    out <- list(quantiles = quans, points = psa, distn = distn,
                call = call, LR = LR)
    class(out) <- "saddle.distn"
    out
}

print.saddle.distn <- function(x, ...) {
#
#  Print the output from saddle.distn
#
    sad.d <- x
    cl <- sad.d$call
    rg <- range(sad.d$points[,1L])
    mid <- mean(rg)
    digs <- ceiling(log10(abs(mid)))
    if (digs <= 0) digs <- 4
    else if (digs >= 4) digs <- 0
    else digs <- 4-digs
    rg <- round(rg,digs)
    level <- 100*sad.d$quantiles[,1L]
    quans <- format(round(sad.d$quantiles,digs))
    quans[,1L] <- paste("\n",format(level),"%     ",sep="")
    cat("\nSaddlepoint Distribution Approximations\n\n")
    cat("Call : \n")
    dput(cl, control=NULL)
    cat("\nQuantiles of the Distribution\n")
    cat(t(quans))
    cat(paste("\n\nSmoothing spline used ", nrow(sad.d$points),
              " points in the range ", rg[1L]," to ", rg[2L], ".\n", sep=""))
    if (sad.d$LR)
        cat("Lugananni-Rice approximations used\n")
    invisible(sad.d)
}

lines.saddle.distn <-
    function(x, dens = TRUE, h = function(u) u, J = function(u) 1,
             npts = 50, lty = 1, ...)
{
#
#  Add lines corresponding to a saddlepoint approximation to a plot
#
    sad.d <- x
    tt <- sad.d$points[,1L]
    rg <- range(h(tt, ...))
    tt1 <- seq.int(from = rg[1L], to = rg[2L], length.out = npts)
    if (dens) {
        gs <- sad.d$points[,2]
        spl <- smooth.spline(h(tt, ...),log(gs*J(tt, ...)))
        lines(tt1,exp(predict(spl, tt1)$y), lty = lty)
    } else {
        Gs <- sad.d$points[,3]
        spl <- smooth.spline(h(tt, ...),qnorm(Gs))
        lines(tt1,pnorm(predict(spl ,tt1)$y))
    }
    invisible(sad.d)
}

ts.array <- function(n, n.sim, R, l, sim, endcorr)
{
#
#  This function finds the starting positions and lengths for the
#  block bootstrap.
#
#  n is the number of data points in the original time series
#  n.sim is the number require in the simulated time series
#  R is the number of simulated series required
#  l is the block length
#  sim is the simulation type "fixed" or "geom".  For "fixed" l is taken
#	to be the fixed block length, for "geom" l is the average block
#	length, the actual lengths having a geometric distribution.
#  endcorr is a logical specifying whether end-correction is required.
#
#  It returns a list of two components
#  starts is a matrix of starts, it has R rows
#  lens is a vector of lengths if sim="fixed" or a matrix of lengths
#	corresponding to the starting points in starts if sim="geom"
    endpt <- if (endcorr) n else n-l+1
    cont <- TRUE
    if (sim == "geom") {
        len.tot <- rep(0,R)
        lens <- NULL
        while (cont) {
#            inds <- (1L:R)[len.tot < n.sim]
            temp <- 1+rgeom(R, 1/l)
            temp <- pmin(temp, n.sim - len.tot)
            lens <- cbind(lens, temp)
            len.tot <- len.tot + temp
            cont <- any(len.tot < n.sim)
        }
        dimnames(lens) <- NULL
        nn <- ncol(lens)
        st <- matrix(sample.int(endpt, nn*R, replace = TRUE), R)
    } else {
        nn <- ceiling(n.sim/l)
        lens <- c(rep(l,nn-1), 1+(n.sim-1)%%l)
        st <- matrix(sample.int(endpt, nn*R, replace = TRUE), R)
    }
    list(starts = st, lengths = lens)
}

make.ends <- function(a, n)
{
#  Function which takes a matrix of starts and lengths and returns the
#  indices for a time series simulation. (Viewing the series as circular.)
    mod <- function(i, n) 1 + (i - 1) %% n
    if (a[2L] == 0) numeric()
    else  mod(seq.int(a[1L], a[1L] + a[2L] - 1, length.out = a[2L]), n)
}


tsboot <- function(tseries, statistic, R, l = NULL, sim = "model",
                   endcorr = TRUE, n.sim = NROW(tseries), orig.t = TRUE,
                   ran.gen = function(tser, n.sim, args) tser,
                   ran.args = NULL, norm = TRUE, ...,
                   parallel = c("no", "multicore", "snow"),
                   ncpus = getOption("boot.ncpus", 1L), cl = NULL)
{
#
#  Bootstrap function for time series data.  Possible resampling methods are
#  the block bootstrap, the stationary bootstrap (these two can also be
#  post-blackened), model-based resampling and phase scrambling.
#
    if (missing(parallel)) parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") have_snow <- TRUE
        if (!have_mc && !have_snow) ncpus <- 1L
    }

    ## This does not necessarily call statistic, so we force a promise.
    statistic

    tscl <- class(tseries)
    R <- floor(R)
    if (R <= 0) stop("'R' must be positive")
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    t0 <- if (orig.t) statistic(tseries, ...) else NULL
    ts.orig <- if (!isMatrix(tseries)) as.matrix(tseries) else tseries
    n <- nrow(ts.orig)
    if (missing(n.sim)) n.sim <- n
    class(ts.orig) <- tscl
    if ((sim == "model") || (sim == "scramble"))
        l <- NULL
    else if ((is.null(l) || (l <= 0) || (l > n)))
        stop("invalid value of 'l'")
    fn <- if (sim == "scramble") {
        rm(ts.orig)
        ## Phase scrambling
        function(r) statistic(scramble(tseries, norm), ...)
    } else if (sim == "model") {
        rm(ts.orig)
        ## Model-based resampling
        ## force promises
        ran.gen; ran.args
        function(r) statistic(ran.gen(tseries, n.sim, ran.args), ...)
    } else if (sim %in% c("fixed", "geom")) {
        ## Otherwise generate an R x n matrix of starts and lengths for blocks.
        ## The actual indices of the blocks can then easily be found and these
        ## indices used for the resampling.  If ran.gen is present then
        ## post-blackening is required when the blocks have been formed.
        if (sim == "geom") endcorr <- TRUE
	i.a <- ts.array(n, n.sim, R, l, sim, endcorr)
        ## force promises
        ran.gen; ran.args
        function(r) {
            ends <- if (sim == "geom")
                cbind(i.a$starts[r,  ], i.a$lengths[r,  ])
            else  cbind(i.a$starts[r, ], i.a$lengths)
            inds <- apply(ends, 1L, make.ends, n)
            inds <- if (is.list(inds)) matrix(unlist(inds)[1L:n.sim], n.sim, 1L)
            else matrix(inds, n.sim, 1L)
            statistic(ran.gen(ts.orig[inds, ], n.sim, ran.args), ...)
        }
    } else
        stop("unrecognized value of 'sim'")

    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(R), fn, mc.cores = ncpus)
        } else if (have_snow) {
            list(...) # evaluate any promises
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
                if(RNGkind()[1L] == "L'Ecuyer-CMRG")
                    parallel::clusterSetRNGStream(cl)
                res <- parallel::parLapply(cl, seq_len(R), fn)
                parallel::stopCluster(cl)
                res
            } else parallel::parLapply(cl, seq_len(R), fn)
       }
    } else lapply(seq_len(R), fn)

    t <- matrix(, R, length(res[[1L]]))
    for(r in seq_len(R)) t[r, ] <- res[[r]]

    ts.return(t0 = t0, t = t, R = R, tseries = tseries, seed = seed,
              stat = statistic, sim = sim, endcorr = endcorr, n.sim = n.sim,
              l = l, ran.gen = ran.gen, ran.args = ran.args, call = call,
              norm = norm)
}

scramble <- function(ts, norm = TRUE)
#
#  Phase scramble a time series.  If norm = TRUE then normal margins are
#  used otherwise exact empirical margins are used.
#
{
    cl <- class(ts)
    if (isMatrix(ts)) stop("multivariate time series not allowed")
    st <- start(ts)
    dt <- deltat(ts)
    frq <- frequency(ts)
    y <- as.vector(ts)
    e <- y - mean(y)
    n <- length(e)
    if (!norm) e <- qnorm( rank(e)/(n+1) )
    f <- fft(e) * complex(n, argument = runif(n) * 2 * pi)
    C.f <- Conj(c(0, f[seq(from = n, to = 2L, by = -1L)])) # or n:2
    e <- Re(mean(y) + fft((f + C.f)/sqrt(2), inverse = TRUE)/n)
    if (!norm) e <- sort(y)[rank(e)]
    ts(e, start = st, freq = frq, deltat = dt)
}

ts.return <- function(t0, t, R, tseries, seed, stat, sim, endcorr,
                      n.sim, l, ran.gen, ran.args, call, norm) {
#
#  Return the results of a time series bootstrap as an object of
#  class "boot".
#
    out <- list(t0 = t0,t = t, R = R, data = tseries, seed = seed,
                statistic = stat, sim = sim, n.sim = n.sim, call = call)
    if (sim ==  "scramble")
        out <- c(out, list(norm = norm))
    else if (sim == "model")
        out <- c(out, list(ran.gen = ran.gen, ran.args = ran.args))
    else {
        out <- c(out, list(l = l, endcorr = endcorr))
        if (!is.null(call$ran.gen))
            out <- c(out,list(ran.gen = ran.gen, ran.args = ran.args))
    }
    class(out) <- "boot"
    out
}
