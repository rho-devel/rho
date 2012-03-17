## file KernSmooth/R/all.R
## original file Copyright (C) M. P. Wand
## modifications for use with R copyright (C) B. D. Ripley
## Unlimited use and distribution (see LICENCE).

bkde <- function(x, kernel = "normal", canonical = FALSE, bandwidth,
                 gridsize = 401L, range.x, truncate = TRUE)
{
    ## Install safeguard against non-positive bandwidths:
    if (!missing(bandwidth) && bandwidth <= 0)
        stop("'bandwidth' must be strictly positive")

    kernel <- match.arg(kernel,
                        c("normal", "box", "epanech", "biweight", "triweight"))

    ## Rename common variables

    n <- length(x)
    M <- gridsize

    ## Set canonical scaling factors

    del0 <- switch(kernel,
                   "normal" = (1/(4*pi))^(1/10),
                   "box" = (9/2)^(1/5),
                   "epanech" = 15^(1/5),
                   "biweight" = 35^(1/5),
                   "triweight" = (9450/143)^(1/5))

    ## Set default bandwidth

    h <- if (missing(bandwidth)) del0 * (243/(35*n))^(1/5)*sqrt(var(x))
    else if(canonical) del0 * bandwidth else bandwidth

    ## Set kernel support values

    tau <-  if (kernel == "normal") 4 else 1

    if (missing(range.x)) range.x <- c(min(x)-tau*h, max(x)+tau*h)
    a <- range.x[1L]
    b <- range.x[2L]

    ## Set up grid points and bin the data

    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)

    ## Compute kernel weights

    delta  <- (b - a)/(h * (M-1L))
    L <- min(floor(tau/delta), M)
    if (L == 0)
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")

    lvec <- 0L:L
    kappa <- if (kernel == "normal")
        dnorm(lvec*delta)/(n*h)
    else if (kernel == "box")
        0.5*dbeta(0.5*(lvec*delta+1), 1, 1)/(n*h)
    else if (kernel == "epanech")
        0.5*dbeta(0.5*(lvec*delta+1), 2, 2)/(n*h)
    else if (kernel == "biweight")
        0.5*dbeta(0.5*(lvec*delta+1), 3, 3)/(n*h)
    else if (kernel == "triweight")
        0.5*dbeta(0.5*(lvec*delta+1), 4, 4)/(n*h)

    ## Now combine weight and counts to obtain estimate
    ## we need P >= 2L+1L, M: L <= M.
    P <- 2^(ceiling(log(M+L+1L)/log(2)))
    kappa <- c(kappa, rep(0, P-2L*L-1L), rev(kappa[-1L]))
    tot <- sum(kappa) * (b-a)/(M-1L) * n # should have total weight one
    gcounts <- c(gcounts, rep(0L, P-M))
    kappa <- fft(kappa/tot)
    gcounts <- fft(gcounts)
    list(x = gpoints, y = (Re(fft(kappa*gcounts, TRUE))/P)[1L:M])
}


bkde2D <-
    function(x, bandwidth, gridsize = c(51L, 51L), range.x, truncate = TRUE)
{
    ## Install safeguard against non-positive bandwidths:

    if (!missing(bandwidth) && min(bandwidth) <= 0)
        stop("'bandwidth' must be strictly positive")

    ## Rename common variables

    n <- nrow(x)
    M <- gridsize
    h <- bandwidth
    tau <- 3.4                          # For bivariate normal kernel.

    ## Use same bandwidth in each direction
    ## if only a single bandwidth is given.

    if (length(h) == 1L) h <- c(h, h)

    ## If range.x is not specified then set it at its default value.

    if (missing(range.x)) {
        range.x <- list(0, 0)
        for (id in (1L:2L))
            range.x[[id]] <- c(min(x[, id])-1.5*h[id], max(x[, id])+1.5*h[id])
    }

    a <- c(range.x[[1L]][1L], range.x[[2L]][1L])
    b <- c(range.x[[1L]][2L], range.x[[2L]][2L])

    ## Set up grid points and bin the data

    gpoints1 <- seq(a[1L], b[1L], length = M[1L])
    gpoints2 <- seq(a[2L], b[2L], length = M[2L])

    gcounts <- linbin2D(x, gpoints1, gpoints2)

    ## Compute kernel weights

    L <- numeric(2L)
    kapid <- list(0, 0)
    for (id in 1L:2L) {
        L[id] <- min(floor(tau*h[id]*(M[id]-1)/(b[id]-a[id])), M[id] - 1L)
        lvecid <- 0:L[id]
        facid <- (b[id] - a[id])/(h[id]*(M[id]-1L))
        z <- matrix(dnorm(lvecid*facid)/h[id])
        tot <- sum(c(z, rev(z[-1L]))) * facid * h[id]
        kapid[[id]] <- z/tot
    }
    kapp <- kapid[[1L]] %*% (t(kapid[[2L]]))/n

    if (min(L) == 0)
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")

    ## Now combine weight and counts using the FFT to obtain estimate

    P <- 2^(ceiling(log(M+L)/log(2)))   # smallest powers of 2 >= M+L
    L1 <- L[1L] ; L2 <- L[2L]
    M1 <- M[1L] ; M2 <- M[2L]
    P1 <- P[1L] ; P2 <- P[2L]

    rp <- matrix(0, P1, P2)
    rp[1L:(L1+1), 1L:(L2+1)] <- kapp
    if (L1) rp[(P1-L1+1):P1, 1L:(L2+1)] <- kapp[(L1+1):2, 1L:(L2+1)]
    if (L2) rp[, (P2-L2+1):P2] <- rp[, (L2+1):2]
    ## wrap-around version of "kapp"

    sp <- matrix(0, P1, P2)
    sp[1L:M1, 1L:M2] <- gcounts
    ## zero-padded version of "gcounts"

    rp <- fft(rp)                       # Obtain FFT's of r and s
    sp <- fft(sp)
    rp <- Re(fft(rp*sp, inverse = TRUE)/(P1*P2))[1L:M1, 1L:M2]
    ## invert element-wise product of FFT's
    ## and truncate and normalise it

    ## Ensure that rp is non-negative

    rp <- rp * matrix(as.numeric(rp>0), nrow(rp), ncol(rp))

    list(x1 = gpoints1, x2 = gpoints2, fhat = rp)
}


bkfe <- function(x, drv, bandwidth, gridsize = 401L, range.x,
                 binned = FALSE, truncate = TRUE)
{
    ## Install safeguard against non-positive bandwidths:

    if (!missing(bandwidth) && bandwidth <= 0)
        stop("'bandwidth' must be strictly positive")

    if (missing(range.x) && !binned) range.x <- c(min(x), max(x))

    ## Rename variables

    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]
    h <- bandwidth

    ## Bin the data if not already binned

    if (!binned) {
        gpoints <- seq(a, b, length = gridsize)
        gcounts <- linbin(x, gpoints, truncate)
    } else {
        gcounts <- x
        M <- length(gcounts)
        gpoints <- seq(a, b, length = M)
    }

    ## Set the sample size and bin width

    n <- sum(gcounts)
    delta <- (b-a)/(M-1)

    ## Obtain kernel weights

    tau <- 4 + drv
    L <- min(floor(tau*h/delta), M)

    if (L == 0)
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")

    lvec <- 0L:L
    arg <- lvec*delta/h

    kappam <- dnorm(arg)/(h^(drv+1))
    hmold0 <- 1
    hmold1 <- arg
    hmnew <- 1
    if (drv >= 2L)
        for (i in (2L:drv)) {
            hmnew <- arg*hmold1 - (i-1)*hmold0
            hmold0 <- hmold1       # Compute mth degree Hermite polynomial
            hmold1 <- hmnew        # by recurrence.
        }
    kappam <- hmnew * kappam

    ## Now combine weights and counts to obtain estimate
    ## we need P >= 2L+1L, M: L <= M.
    P <- 2^(ceiling(log(M+L+1L)/log(2)))
    kappam <- c(kappam, rep(0,  P-2L*L-1L), rev(kappam[-1L]))
    Gcounts <- c(gcounts, rep(0, P-M))
    kappam <- fft(kappam)
    Gcounts <- fft(Gcounts)

    sum(gcounts * (Re(fft(kappam*Gcounts, TRUE))/P)[1L:M] )/(n^2)
}

## For obtaining preliminary estimates of
## quantities required for the "direct plug-in"
## regression bandwidth selector based on
## blocked qth degree polynomial fits.

blkest <- function(x, y, Nval, q)
{
    n <- length(x)

    ## Sort the (x, y) data with respect to
    ## the x's.

    datmat <- cbind(x, y)
    datmat <- datmat[sort.list(datmat[, 1L]), ]
    x <- datmat[, 1L]
    y <- datmat[, 2L]

    ## Set up arrays for FORTRAN programme "blkest"

    qq <- q + 1L
    xj <- rep(0, n)
    yj <- rep(0, n)
    coef <- rep(0, qq)
    Xmat <- matrix(0, n, qq)
    wk <- rep(0, n)
    qraux <- rep(0, qq)
    sigsqe <- 0
    th22e <- 0
    th24e <- 0

    out <- .Fortran(F_blkest, as.double(x), as.double(y), as.integer(n),
                    as.integer(q), as.integer(qq), as.integer(Nval), as.double(xj),
                    as.double(yj), as.double(coef), as.double(Xmat), as.double(wk),
                    as.double(qraux), as.double(sigsqe), as.double(th22e),
                    as.double(th24e))

    list(sigsqe = out[[13]], th22e = out[[14]], th24e = out[[15]])
}

## Chooses the number of blocks for the premilinary
## step of a plug-in rule using Mallows' C_p.
cpblock <- function(X, Y, Nmax, q)
{
    n <- length(X)

    ## Sort the (X, Y) data with respect tothe X's.

    datmat <- cbind(X, Y)
    datmat <- datmat[sort.list(datmat[, 1L]), ]
    X <- datmat[, 1L]
    Y <- datmat[, 2L]

    ## Set up arrays for FORTRAN subroutine "cp"

    qq <- q + 1L
    RSS <- rep(0, Nmax)
    Xj <- rep(0, n)
    Yj <- rep(0, n)
    coef <- rep(0, qq)
    Xmat <- matrix(0, n, qq)
    Cpvals <- rep(0, Nmax)
    wk <- rep(0, n)
    qraux <- rep(0, qq)

    ## remove unused 'q' 2007-07-10
    out <- .Fortran(F_cp, as.double(X), as.double(Y), as.integer(n),
                    as.integer(qq), as.integer(Nmax), as.double(RSS), as.double(Xj),
                    as.double(Yj), as.double(coef), as.double(Xmat), as.double(wk),
                    as.double(qraux), Cpvals = as.double(Cpvals))

    Cpvec <- out$Cpvals

    order(Cpvec)[1L]
}

dpih <- function(x, scalest = "minim", level = 2L, gridsize = 401L,
                 range.x = range(x), truncate = TRUE)
{
    if (level > 5L) stop("Level should be between 0 and 5")

    ## Rename variables

    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]

    ## Set up grid points and bin the data

    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)

    ## Compute scale estimate

    scalest <- match.arg(scalest, c("minim", "stdev", "iqr"))
    scalest <- switch(scalest,
                      "stdev" = sqrt(var(x)),
                      "iqr"= (quantile(x, 3/4)-quantile(x, 1/4))/1.349,
                      "minim" = min((quantile(x, 3/4)-quantile(x, 1/4))/1.349, sqrt(var(x))) )

    if (scalest == 0) stop("scale estimate is zero for input data")

    ## Replace input data by standardised data for numerical stability:

    sx <- (x-mean(x))/scalest
    sa <- (a-mean(x))/scalest ; sb <- (b-mean(x))/scalest

    ## Set up grid points and bin the data:

    gpoints <- seq(sa, sb, length = M)
    gcounts <- linbin(sx, gpoints, truncate)
    delta <- (sb-sa)/(M - 1)

    ## Perform plug-in steps

    hpi <- if (level == 0L) (24*sqrt(pi)/n)^(1/3)
    else if (level == 1L) {
        alpha <- (2/(3*n))^(1/5)*sqrt(2) # bandwidth for psi_2
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), binned = TRUE)
        (6/(-psi2hat*n))^(1/3)
    } else if (level == 2L) {
        alpha <- ((2/(5*n))^(1/7))*sqrt(2) # bandwidth for psi_4
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat*n))^(1/5) # bandwidth for psi_2
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), binned = TRUE)
        (6/(-psi2hat*n))^(1/3)
    } else if (level == 3L) {
        alpha <- ((2/(7*n))^(1/9))*sqrt(2) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat*n))^(1/5) # bandwidth for psi_2
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), binned = TRUE)
        (6/(-psi2hat*n))^(1/3)
    } else if (level == 4L) {
        alpha <- ((2/(9*n))^(1/11))*sqrt(2) # bandwidth for psi_8
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (15*sqrt(2/pi)/(psi8hat*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat*n))^(1/5) # bandwidth for psi_2
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), binned = TRUE)
        (6/(-psi2hat*n))^(1/3)
    } else if (level == 5L) {
        alpha <- ((2/(11*n))^(1/13))*sqrt(2) # bandwidth for psi_10
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-105*sqrt(2/pi)/(psi10hat*n))^(1/11) # bandwidth for psi_8
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (15*sqrt(2/pi)/(psi8hat*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat*n))^(1/5) # bandwidth for psi_2
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), binned = TRUE)
        (6/(-psi2hat*n))^(1/3)
    }

    scalest * hpi
}

dpik <- function(x, scalest = "minim", level = 2L, kernel = "normal",
                 canonical = FALSE, gridsize = 401L, range.x = range(x),
                 truncate = TRUE)
{
    if (level > 5L) stop("Level should be between 0 and 5")

    kernel <- match.arg(kernel,
                        c("normal", "box", "epanech", "biweight", "triweight"))

    ## Set kernel constants
    del0 <- if (canonical) 1 else switch(kernel,
                                         "normal" = 1/((4*pi)^(1/10)),
                                         "box" = (9/2)^(1/5),
                                         "epanech" = 15^(1/5),
                                         "biweight" = 35^(1/5),
                                         "triweight" = (9450/143)^(1/5))

    ## Rename variables
    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]

    ## Set up grid points and bin the data
    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)

    ## Compute scale estimate
    scalest <- match.arg(scalest, c("minim", "stdev", "iqr"))
    scalest <- switch(scalest,
                      "stdev" = sqrt(var(x)),
                      "iqr"= (quantile(x, 3/4)-quantile(x, 1/4))/1.349,
                      "minim" = min((quantile(x, 3/4)-quantile(x, 1/4))/1.349, sqrt(var(x))) )

    if (scalest == 0) stop("scale estimate is zero for input data")

    ## Replace input data by standardised data for numerical stability:
    sx <- (x-mean(x))/scalest
    sa <- (a-mean(x))/scalest ; sb <- (b-mean(x))/scalest

    ## Set up grid points and bin the data:
    gpoints <- seq(sa, sb, length = M)
    gcounts <- linbin(sx, gpoints, truncate)
    delta <- (sb-sa)/(M-1)

    ## Perform plug-in steps:

    psi4hat <- if (level == 0L) 3/(8*sqrt(pi))
    else if (level == 1L) {
        alpha <- (2*(sqrt(2))^7/(5*n))^(1/7) # bandwidth for psi_4
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    } else if (level == 2L) {
        alpha <- (2*(sqrt(2))^9/(7*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    } else if (level == 3L) {
        alpha <- (2*(sqrt(2))^11/(9*n))^(1/11) # bandwidth for psi_8
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (15*sqrt(2/pi)/(psi8hat*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    } else if (level == 4L) {
        alpha <- (2*(sqrt(2))^13/(11*n))^(1/13) # bandwidth for psi_10
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-105*sqrt(2/pi)/(psi10hat*n))^(1/11) # bandwidth for psi_8
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (15*sqrt(2/pi)/(psi8hat*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    } else if (level == 5L) {
        alpha <- (2*(sqrt(2))^15/(13*n))^(1/15) # bandwidth for psi_12
        psi12hat <- bkfe(gcounts, 12L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (945*sqrt(2/pi)/(psi12hat*n))^(1/13) # bandwidth for psi_10
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-105*sqrt(2/pi)/(psi10hat*n))^(1/11) # bandwidth for psi_8
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (15*sqrt(2/pi)/(psi8hat*n))^(1/9) # bandwidth for psi_6
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), binned = TRUE)
        alpha <- (-3*sqrt(2/pi)/(psi6hat*n))^(1/7) # bandwidth for psi_4
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }

    scalest * del0 * (1/(psi4hat*n))^(1/5)
}

## Computes a direct plug-in selector of the
## bandwidth for local linear regression as
## described in the 1996 J. Amer. Statist. Assoc.
## paper by Ruppert, Sheather and Wand.
dpill <- function(x, y, blockmax = 5, divisor = 20, trim = 0.01,
                  proptrun = 0.05, gridsize = 401L,
                  range.x = range(x), truncate = TRUE)
{
    ## Trim the 100(trim)% of the data from each end (in the x-direction).

    xy <- cbind(x, y)
    xy <- xy[sort.list(xy[, 1L]), ]
    x <- xy[, 1L]
    y <- xy[, 2L]
    indlow <- floor(trim*length(x)) + 1
    indupp <- length(x) - floor(trim*length(x))

    x <- x[indlow:indupp]
    y <- y[indlow:indupp]

    ## Rename common parameters
    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]

    ## Bin the data

    gpoints <- seq(a, b, length = M)
    out <- rlbin(x, y, gpoints, truncate)
    xcounts <- out$xcounts
    ycounts <- out$ycounts

    ## Choose the value of N using Mallow's C_p
    Nmax <- max(min(floor(n/divisor), blockmax), 1)
    Nval <- cpblock(x, y, Nmax, 4)

    ## Estimate sig^2, theta_22 and theta_24 using quartic fits
    ## on "Nval" blocks.

    out <- blkest(x, y, Nval, 4)
    sigsqQ <- out$sigsqe
    th24Q <- out$th24e

    ## Estimate theta_22 using a local cubic fit
    ## with a "rule-of-thumb" bandwidth: "gamseh"

    gamseh <- (sigsqQ*(b-a)/(abs(th24Q)*n))
    if (th24Q < 0) gamseh <- (3*gamseh/(8*sqrt(pi)))^(1/7)
    if (th24Q > 0) gamseh <- (15*gamseh/(16*sqrt(pi)))^(1/7)

    mddest <- locpoly(xcounts, ycounts, drv=2L, bandwidth=gamseh,
                      range.x=range.x, binned=TRUE)$y

    llow <- floor(proptrun*M) + 1
    lupp <- M - floor(proptrun*M)
    th22kn <- sum((mddest[llow:lupp]^2)*xcounts[llow:lupp])/n

    ## Estimate sigma^2 using a local linear fit
    ## with a "direct plug-in" bandwidth: "lamseh"
    C3K <- (1/2) + 2*sqrt(2) - (4/3)*sqrt(3)
    C3K <- (4*C3K/(sqrt(2*pi)))^(1/9)
    lamseh <- C3K*(((sigsqQ^2)*(b-a)/((th22kn*n)^2))^(1/9))

    ## Now compute a local linear kernel estimate of
    ## the variance.
    mest <- locpoly(xcounts, ycounts, bandwidth=lamseh,
                    range.x=range.x, binned=TRUE)$y
    Sdg <- sdiag(xcounts, bandwidth=lamseh,
                 range.x=range.x, binned=TRUE)$y
    SSTdg <- sstdiag(xcounts, bandwidth=lamseh,
                     range.x=range.x, binned=TRUE)$y
    sigsqn <- sum(y^2) - 2*sum(mest*ycounts) + sum((mest^2)*xcounts)
    sigsqd <- n - 2*sum(Sdg*xcounts) + sum(SSTdg*xcounts)
    sigsqkn <- sigsqn/sigsqd

    ## Combine to obtain final answer.
    (sigsqkn*(b-a)/(2*sqrt(pi)*th22kn*n))^(1/5)
}

## For application of linear binning to a univariate data set.
linbin <- function(X, gpoints, truncate = TRUE)
{
    n <- length(X)
    M <- length(gpoints)
    if (truncate) trun <- 1L else 0L
    a <- gpoints[1L]
    b <- gpoints[M]
    .Fortran(F_linbin, as.double(X), as.integer(n),
             as.double(a), as.double(b), as.integer(M),
             as.integer(trun), double(M))[[7]]
}

## Creates the grid counts from a bivariate data set X
## over an equally-spaced set of grid points
## contained in "gpoints" using the linear
## binning strategy. Note that the FORTRAN subroutine
## "lbtwod" is called.
linbin2D <- function(X, gpoints1, gpoints2)
{
    n <- nrow(X)
    X <- c(X[, 1L], X[, 2L])
    M1 <- length(gpoints1)
    M2 <- length(gpoints2)
    a1 <- gpoints1[1L]
    a2 <- gpoints2[1L]
    b1 <- gpoints1[M1]
    b2 <- gpoints2[M2]
    out <- .Fortran(F_lbtwod, as.double(X), as.integer(n),
                    as.double(a1), as.double(a2), as.double(b1), as.double(b2),
                    as.integer(M1), as.integer(M2), double(M1*M2))
    matrix(out[[9L]], M1, M2)
}

## For computing a binned local polynomial
## regression estimator of a univariate regression
## function or its derivative.
## The data are discretised on an equally
## spaced grid. The bandwidths are discretised on a
## logarithmically spaced grid.
locpoly <- function(x, y, drv = 0L, degree, kernel = "normal",
                    bandwidth, gridsize = 401L, bwdisc = 25, range.x,
                    binned = FALSE, truncate = TRUE)

{
    ## Install safeguard against non-positive bandwidths:
    if (!missing(bandwidth) && bandwidth <= 0)
        stop("'bandwidth' must be strictly positive")

    drv <- as.integer(drv)
    if (missing(degree)) degree <- drv + 1L
    else degree <- as.integer(degree)

    if (missing(range.x) && !binned)
        if (missing(y)) {
            extra <- 0.05*(max(x) - min(x))
            range.x <- c(min(x)-extra,  max(x)+extra)
        } else range.x <- c(min(x), max(x))

    ## Rename common variables
    M <- gridsize
    Q <- as.integer(bwdisc)
    a <- range.x[1L]
    b <- range.x[2L]
    pp <- degree + 1L
    ppp <- 2L*degree + 1L
    tau <- 4

    ## Decide whether a density estimate or regressionestimate is required.

    if (missing(y))  {  # obtain density estimate
        y <- NULL
        n <- length(x)
        gpoints <- seq(a, b, length = M)
        xcounts <- linbin(x, gpoints, truncate)
        ycounts <- (M-1)*xcounts/(n*(b-a))
        xcounts <- rep(1, M)
    } else {            # obtain regression estimate
        ## Bin the data if not already binned
        if (!binned) {
            gpoints <- seq(a, b, length = M)
            out <- rlbin(x, y, gpoints, truncate)
            xcounts <- out$xcounts
            ycounts <- out$ycounts
        } else {
            xcounts <- x
            ycounts <- y
            M <- length(xcounts)
            gpoints <- seq(a, b, length = M)
        }
    }

    ## Set the bin width
    delta <- (b-a)/(M-1L)

    ## Discretise the bandwidths
    if (length(bandwidth) == M) {
        hlow <- sort(bandwidth)[1L]
        hupp <- sort(bandwidth)[M]
        hdisc <- exp(seq(log(hlow), log(hupp), length = Q))

        ## Determine value of L for each member of "hdisc"
        Lvec <- floor(tau*hdisc/delta)

        ## Determine index of closest entry of "hdisc"
        ## to each member of "bandwidth"
        indic <- if (Q > 1L) {
            lhdisc <- log(hdisc)
            gap <- (lhdisc[Q]-lhdisc[1L])/(Q-1)
            if (gap == 0) rep(1, M)
            else round(((log(bandwidth) - log(sort(bandwidth)[1L]))/gap) + 1)
        } else rep(1, M)
    } else if (length(bandwidth) == 1L) {
        indic <- rep(1, M)
        Q <- 1L
        Lvec <- rep(floor(tau*bandwidth/delta), Q)
        hdisc <- rep(bandwidth, Q)
    } else
        stop("'bandwidth' must be a scalar or an array of length 'gridsize'")

    if (min(Lvec) == 0)
        stop("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")

    ## Allocate space for the kernel vector and final estimate

    dimfkap <- 2L * sum(Lvec) + Q
    fkap <- rep(0, dimfkap)
    curvest <- rep(0, M)
    midpts <- rep(0, Q)
    ss <- matrix(0, M, ppp)
    tt <- matrix(0, M, pp)
    Smat <- matrix(0, pp, pp)
    Tvec <- rep(0, pp)
    ipvt <- rep(0, pp)

    ## Call FORTRAN routine "locpol"

    out <- .Fortran(F_locpol, as.double(xcounts), as.double(ycounts),
                    as.integer(drv), as.double(delta), as.double(hdisc),
                    as.integer(Lvec), as.integer(indic), as.integer(midpts),
                    as.integer(M), as.integer(Q), as.double(fkap), as.integer(pp),
                    as.integer(ppp), as.double(ss), as.double(tt),
                    as.double(Smat), as.double(Tvec), as.integer(ipvt),
                    as.double(curvest))

    curvest <- gamma(drv+1) * out[[19L]]

    list(x = gpoints, y = curvest)
}

## For application of linear binning to a regression
## data set.
rlbin <- function(X, Y, gpoints, truncate = TRUE)
{
    n <- length(X)
    M <- length(gpoints)
    trun <- if (truncate) 1L else 0L
    a <- gpoints[1L]
    b <- gpoints[M]
    out <- .Fortran(F_rlbin, as.double(X), as.double(Y), as.integer(n),
                    as.double(a), as.double(b), as.integer(M), as.integer(trun),
                    double(M), double(M))
    list(xcounts = out[[8L]],  ycounts = out[[9L]])
}

## For computing the binned diagonal entries of a smoother
## matrix for local polynomial kernel regression.

sdiag <- function(x, drv = 0L, degree = 1L, kernel = "normal",
                    bandwidth, gridsize = 401L, bwdisc = 25, range.x,
                    binned = FALSE, truncate = TRUE)
{
    if (missing(range.x) && !binned) range.x <- c(min(x), max(x))

    ## Rename common variables

    M <- gridsize
    Q <- as.integer(bwdisc)
    a <- range.x[1L]
    b <- range.x[2L]
    pp <- degree + 1L
    ppp <- 2L*degree + 1L
    tau <- 4

    ## Bin the data if not already binned

    if (!binned) {
        gpoints <- seq(a, b, length = M)
        xcounts <- linbin(x, gpoints, truncate)
    } else {
        xcounts <- x
        M <- length(xcounts)
        gpoints <- seq(a, b, length = M)
    }

    ## Set the bin width

    delta <- (b-a)/(M-1L)

    ## Discretise the bandwidths

    if (length(bandwidth) == M) {
        hlow <- sort(bandwidth)[1L]
        hupp <- sort(bandwidth)[M]
        hdisc <- exp(seq(log(hlow), log(hupp), length = Q))

        ## Determine value of L for each member of "hdisc"
        Lvec <- floor(tau*hdisc/delta)

        ## Determine index of closest entry of "hdisc"
        ## to each member of "bandwidth"
        indic <- if (Q > 1L) {
            lhdisc <- log(hdisc)
            gap <- (lhdisc[Q]-lhdisc[1L])/(Q-1)
            if (gap == 0) rep(1, M)
            else round(((log(bandwidth) - log(sort(bandwidth)[1L]))/gap) + 1)
        } else rep(1, M)
    } else if (length(bandwidth) == 1L) {
        indic <- rep(1, M)
        Q <- 1L
        Lvec <- rep(floor(tau*bandwidth/delta), Q)
        hdisc <- rep(bandwidth, Q)
    } else
        stop("'bandwidth' must be a scalar or an array of length 'gridsize'")

    dimfkap <- 2L * sum(Lvec) + Q
    fkap <- rep(0, dimfkap)
    midpts <- rep(0, Q)
    ss <- matrix(0, M, ppp)
    Smat <- matrix(0, pp, pp)
    work <- rep(0, pp)
    det <- rep(0, 2L)
    ipvt <- rep(0, pp)
    Sdg <- rep(0, M)

    out <- .Fortran(F_sdiag, as.double(xcounts), as.double(delta),
                    as.double(hdisc), as.integer(Lvec), as.integer(indic),
                    as.integer(midpts), as.integer(M), as.integer(Q),
                    as.double(fkap), as.integer(pp), as.integer(ppp),
                    as.double(ss), as.double(Smat), as.double(work),
                    as.double(det), as.integer(ipvt), as.double(Sdg))

    list(x = gpoints,  y = out[[17L]])
}

## For computing the binned diagonal entries of SS^T
## where S is a smoother matrix for local polynomial
## kernel regression.
sstdiag <- function(x, drv = 0L, degree = 1L, kernel = "normal",
                    bandwidth, gridsize = 401L, bwdisc = 25, range.x,
                    binned = FALSE, truncate = TRUE)
{
    if (missing(range.x) && !binned) range.x <- c(min(x), max(x))

    ## Rename common variables
    M <- gridsize
    Q <- as.integer(bwdisc)
    a <- range.x[1L]
    b <- range.x[2L]
    pp <- degree + 1L
    ppp <- 2L*degree + 1L
    tau <- 4L

    ## Bin the data if not already binned
    if (!binned) {
        gpoints <- seq(a, b, length = M)
        xcounts <- linbin(x, gpoints, truncate)
    } else {
        xcounts <- x
        M <- length(xcounts)
        gpoints <- seq(a, b, length = M)
    }

    ## Set the bin width

    delta <- (b-a)/(M-1L)

    ## Discretise the bandwidths
    if (length(bandwidth) == M) {
        hlow <- sort(bandwidth)[1L]
        hupp <- sort(bandwidth)[M]
        hdisc <- exp(seq(log(hlow), log(hupp), length = Q))

        ## Determine value of L for each member of "hdisc"
        Lvec <- floor(tau*hdisc/delta)

        ## Determine index of closest entry of "hdisc"
        ## to each member of "bandwidth"
        indic <- if (Q > 1L) {
            lhdisc <- log(hdisc)
            gap <- (lhdisc[Q]-lhdisc[1L])/(Q-1)
            if (gap == 0) rep(1, M)
            else round(((log(bandwidth) - log(sort(bandwidth)[1L]))/gap) + 1)
        } else rep(1, M)
    } else if (length(bandwidth) == 1L) {
        indic <- rep(1, M)
        Q <- 1L
        Lvec <- rep(floor(tau*bandwidth/delta), Q)
        hdisc <- rep(bandwidth, Q)
    } else
        stop("'bandwidth' must be a scalar or an array of length 'gridsize'")

    dimfkap <- 2L * sum(Lvec) + Q
    fkap <- rep(0, dimfkap)
    midpts <- rep(0, Q)
    ss <- matrix(0, M, ppp)
    uu <- matrix(0, M, ppp)
    Smat <- matrix(0, pp, pp)
    Umat <- matrix(0, pp, pp)
    work <- rep(0, pp)
    det <- rep(0, 2L)
    ipvt <- rep(0, pp)
    SSTd <- rep(0, M)

    SSTd <- .Fortran(F_sstdg, as.double(xcounts), as.double(delta),
                    as.double(hdisc), as.integer(Lvec), as.integer(indic),
                    as.integer(midpts), as.integer(M), as.integer(Q),
                    as.double(fkap), as.integer(pp), as.integer(ppp),
                    as.double(ss), as.double(uu), as.double(Smat),
                    as.double(Umat), as.double(work), as.double(det),
                    as.integer(ipvt), as.double(SSTd))[[19L]]

    list(x = gpoints, y = SSTd)
}

.onLoad <- function(libname, pkgname)
   packageStartupMessage("KernSmooth 2.23 loaded\nCopyright M. P. Wand 1997-2009")

.onUnload <- function(libpath)
    library.dynam.unload("KernSmooth",  libpath)
