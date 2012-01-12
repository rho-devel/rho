#### R-interface to  Agglomerative / Divisive coefficient
####
coef.twins <- function(object, ...)
{
    if(inherits(object, "agnes"))
	object$ac
    else if(inherits(object, "diana"))
	object$dc
    else
	stop("invalid 'twins' object")
}

coef.hclust <- function(object, ...)
{
    ## Author: Martin Maechler, Date: 27 Nov 2004
    ## Now "really" using $merge _and_ $height -- assuming they match!
    ht  <- object$height
    mrg <- object$merge
    nh <- length(ht)
    stopifnot(nh > 0, is.matrix(mrg), dim(mrg) == c(nh,2),
              is.numeric(ht), is.numeric(mrg),
              !is.unsorted(ht))# then they match with merge
    ## stopifnot(all.equal(1:n, sort(-mrg[mrg < 0])))

    1 - sum(rowSums(mrg < 0) * ht) / max(ht) / (nh+1)
}

if(FALSE){ ##-- experiments

## Note this is (the only!) direct interface to   bncoef(),
## ---- which is used internally both in agnes() and diana() :
coef2.hclust <- function(object, ...)
{
    ## Purpose: Compute agglomerative coefficient from hclust
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 27 Nov 2004
    nh <- length(ht <- object$height)
    stopifnot(nh > 0, is.numeric(ht))
    .Fortran(bncoef,
	     n =  as.integer(nh + 1),
	     ban= as.double(c(0., ht)),
	     cf = double(1))$cf
}

}
