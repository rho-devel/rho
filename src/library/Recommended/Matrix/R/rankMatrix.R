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
                       sval = svd(x, 0,0)$d, warn.t = TRUE)
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

    stopifnot(length(d <- dim(x)) == 2)
    p <- min(d)
    miss.meth <- missing(method)
    method <- match.arg(method)

    if(useGrad <- (method %in% c("useGrad", "maybeGrad"))) {
	stopifnot(length(sval) == p,
		  diff(sval) <= 0) # must be sorted non-increasingly: max = s..[1]
	ln.av <- log(abs(sval))
	diff1 <- diff(ln.av)
	if(method == "maybeGrad") {
	    grad <- (min(ln.av) - max(ln.av)) / p
	    useGrad <- (min(diff1) <= min(-3, 10 * grad))
	}#  -------
    }
    if(!useGrad) {
	x.dense <- is(x,"denseMatrix")
	findTol <- function() {         # the "Matlab" default:
	    stopifnot(diff(sval) <= 0)
	    max(d) * .Machine$double.eps * abs(sval[1])
	}
	if(method == "qrLINPACK") {
	    if(is.null(tol) && x.dense) tol <- findTol()
	    ## tol = NULL is fine for sparse QR
	} else { ## (method != "qrLINPACK")
	    if(is.null(tol)) {
		if(!x.dense && missing(sval) && prod(d) >= 100000L)
		    warning(gettextf("rankMatrix(<large sparse Matrix>, method = '%s') coerces to dense matrix.\n  Probably should rather use  method = 'qrLINPACK' !?",
				     method),
			    immediate.=TRUE, domain=NA)
		tol <- findTol()
	    } else stopifnot((tol <- as.numeric(tol)[[1]]) >= 0)
	}
    }

    structure(## rank :
	      if(useGrad) which.min(diff1)
	      else if(method == "qrLINPACK") {
		  if(do.t <- (d[1L] < d[2L]))
		      warning(gettextf(
			"rankMatrix(x, method='qrLINPACK'): computing t(x) as nrow(x) < ncol(x)"))
		  q.r <- qr(if(do.t) t(x) else x, tol=tol, LAPACK = FALSE)
		  if(is(q.r, "qr")) q.r$rank
		  else if(is(q.r,"sparseQR")) sum(diag(q.r@R) != 0)
		  else stop(gettextf(
			"method %s not applicable for qr() result class %s",
				     sQuote(method), dQuote(class(q.r)[1])),
			    domain=NA)
	      } else sum(sval >= tol),
	      "method" = method,
	      "useGrad" = useGrad,
	      "tol" = if(useGrad) NA else tol)
}

## Ravi's plot of the absolute singular values:
if(FALSE) {
## if (plot.eigen) {
    plot(abs(sval), type = "b", xlab = "Index", xaxt = "n",
         log = "y", ylab = "|singular value|   [log scaled]")
    axis(1, at = unique(c(axTicks(1), rank, p)))
    abline(v = rank, lty = 3)
    mtext(sprintf("rank = %d (used %s (%g))", rank,
                  if(use.grad)"'gradient'" else "fixed tol.",
                  if(use.grad) min(diff1)  else tol))
}

