#### Cluster - Internal Utilities
#### ============================ (new by Martin Mächler)

## This was size(); seems slightly useful in general
sizeDiss <- function(d)
{
    ## find 'n' for d == dissimilarity-like(<n obs.>), i.e. length(d)= n(n-1)/2
    discr <- 1 + 8 * length(d)
    sqrtdiscr <- round(sqrt(discr))
    if(sqrtdiscr^2 == discr) (1 + sqrtdiscr)/2 else NA
}

## used in  ./agnes.q, ./clara.q,  ./diana.q  und ./pam.q :

lower.to.upper.tri.inds <- function(n)
{
    n1 <- as.integer(n - 1)
    if(n1 < 1) stop("'n' must be >= 2")
    else if(n1 == 1) 1L
    else rep(1:n1, 1:n1) +
        c(0, unlist(lapply(2:n1, function(k) cumsum(c(0, (n - 2):(n - k))))))
}

upper.to.lower.tri.inds <- function(n)
{
    if((n2 <- as.integer(n - 2)) < 0) stop("'n' must be >= 2")
    rep(1 + cumsum(0:n2), (n - 1):1) +
	unlist(lapply(0:n2, function(k) cumsum(k:n2)))
}

#### consider to *not* export these when I will use a name space :

meanabsdev <- function(y) mean(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)
