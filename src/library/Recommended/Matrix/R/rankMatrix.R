#### Determine *the* rank of a matrix
#### --------------------------------
##
## As this is not such a well-defined problem as people think,
## we provide *some* possibilities here, including the Matlab one.
##
## Ideas by Martin Maechler (April 2007) and Ravi Varadhan (October 2007)

## (for now: use code & examples from /u/maechler/R/MM/NUMERICS/rankMat.R )

rankMatrix <- function(x, tol = NULL,
                       method = c("tolNorm2", "qrLINPACK", "useGrad", "maybeGrad"),
                       sval = svd(x, 0,0)$d)
{
    ## Purpose: rank of a matrix ``as Matlab'' or "according to Ravi V"
    ## ----------------------------------------------------------------------
    ## Arguments: x: a numerical matrix, maybe non-square
    ##          tol: numerical tolerance (compared to singular values)
    ##         sval: vector of non-increasing singular values of  x
    ##               (pass as argument if already known)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 7 Apr 2007, 16:16
    ## ----------------------------------------------------------------------
    ##
    ## maybeGrad (Ravi V.): This algorithm determines the rank based on the
    ##	"gradient" of the
    ## absolute, singular values, rather than enforcing a rigid
    ## tolerance criterion,
    ##
    ## Author: Ravi Varadhan, Date: 22 October 2007 // Tweaks: MM, Oct.23

    ## ----------------------------------------------------------------------

    d <- dim(x)
    p <- min(d)
    stopifnot(length(d) == 2, length(sval) == p,
              diff(sval) <= 0) # must be sorted non-increasingly: max = s..[1]
    absval <- abs(sval)
    method <- match.arg(method)

    if(method %in% c("useGrad", "maybeGrad")) {
        useGrad <- TRUE
        ln.av <- log(absval)
        diff1 <- diff(ln.av)
        if(method == "maybeGrad") {
            grad <- (min(ln.av) - max(ln.av)) / p
            useGrad <- (min(diff1) <= min(-3, 10 * grad))
        }
    } else useGrad <- FALSE

    if(!useGrad) {
        if(is.null(tol))# the "Matlab" default:
            tol <- max(d) * .Machine$double.eps * absval[1]
        else stopifnot(is.numeric(tol), tol >= 0)
    }

    structure(## rank :
	      if(useGrad) which.min(diff1)
	      else if(method == "qrLINPACK") {
		  q.r <- qr(x, tol=tol, LAPACK = FALSE)
		  if(is(q.r, "qr")) q.r$rank
		  else if(is(q.r,"sparseQR")) sum(diag(q.r@R) != 0)
		  else stop(gettextf(
			"method '%s' not applicable for qr() result class '%s'",
				     method, class(q.r)[1]))
	      } else sum(sval >= tol),
	      "method" = method,
	      "useGrad" = useGrad,
	      "tol" = if(useGrad) NA else tol)
}

## Ravi's plot of the absolute singular values:
if(FALSE) {
## if (plot.eigen) {
    plot(absval, type = "b", xlab = "Index", xaxt = "n",
         log = "y", ylab = "|singular value|   [log scaled]")
    axis(1, at = unique(c(axTicks(1), rank, p)))
    abline(v = rank, lty = 3)
    mtext(sprintf("rank = %d (used %s (%g))", rank,
                  if(use.grad)"'gradient'" else "fixed tol.",
                  if(use.grad) min(diff1)  else tol))
}

