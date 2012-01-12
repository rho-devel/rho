#### Methods for the sparse QR decomposition

setMethod("qr.R", signature(qr = "sparseQR"),
	  function(qr, complete = FALSE) {
	      r <- qr@R
	      ## FIXME: add option (e.g. argument 'backPermute = FALSE')
	      ##	to deal with this:
	      warning("qr.R(<sparse>) may differ from qr.R(<dense>) because of permutations")
	      if(!complete && {d <- dim(r); d[1] != d[2]})
		  as(r[seq.int(min(d)), , drop = FALSE], "triangularMatrix")
	      else
		  r
	  })

## The signature should change to y = "ddenseMatrix" later
setMethod("qr.qy", signature(qr = "sparseQR", y = "dgeMatrix"),
          function(qr, y) .Call(sparseQR_qty, qr, y, FALSE),
          valueClass = "dgeMatrix")

setMethod("qr.qy", signature(qr = "sparseQR", y = "matrix"),
          function(qr, y) .Call(sparseQR_qty, qr, y, FALSE),
          valueClass = "dgeMatrix")

setMethod("qr.qy", signature(qr = "sparseQR", y = "numeric"),
          function(qr, y) .Call(sparseQR_qty, qr, y, FALSE),
          valueClass = "dgeMatrix")

## The signature should change to y = "ddenseMatrix" later
setMethod("qr.qty", signature(qr = "sparseQR", y = "dgeMatrix"),
          function(qr, y) .Call(sparseQR_qty, qr, y, TRUE),
          valueClass = "dgeMatrix")

setMethod("qr.qty", signature(qr = "sparseQR", y = "matrix"),
          function(qr, y) .Call(sparseQR_qty, qr, y, TRUE),
          valueClass = "dgeMatrix")

setMethod("qr.qty", signature(qr = "sparseQR", y = "numeric"),
          function(qr, y) .Call(sparseQR_qty, qr, y, TRUE),
          valueClass = "dgeMatrix")

.coef.trunc <- function(qr, res) res[1:ncol(qr@R),,drop=FALSE]

## The signature should change to y = "ddenseMatrix" later
setMethod("qr.coef", signature(qr = "sparseQR", y = "dgeMatrix"),
          function(qr, y)
          .coef.trunc(qr, .Call(sparseQR_coef, qr, y)),
          valueClass = "dgeMatrix")

setMethod("qr.coef", signature(qr = "sparseQR", y = "matrix"),
          function(qr, y)
          .coef.trunc(qr, .Call(sparseQR_coef, qr, y)),
          valueClass = "dgeMatrix")

setMethod("qr.coef", signature(qr = "sparseQR", y = "numeric"),
          function(qr, y)
          .coef.trunc(qr, .Call(sparseQR_coef, qr, y)),
          valueClass = "dgeMatrix")

## The signature should change to y = "ddenseMatrix" later
setMethod("qr.resid", signature(qr = "sparseQR", y = "dgeMatrix"),
          function(qr, y)
          .Call(sparseQR_resid_fitted, qr, y, TRUE),
          valueClass = "dgeMatrix")

setMethod("qr.resid", signature(qr = "sparseQR", y = "matrix"),
          function(qr, y)
          .Call(sparseQR_resid_fitted, qr, y, TRUE),
          valueClass = "dgeMatrix")

setMethod("qr.resid", signature(qr = "sparseQR", y = "numeric"),
          function(qr, y)
          .Call(sparseQR_resid_fitted, qr, y, TRUE),
          valueClass = "dgeMatrix")

## The signature should change to y = "ddenseMatrix" later
setMethod("qr.fitted", signature(qr = "sparseQR", y = "dgeMatrix"),
          function(qr, y, k)
          .Call(sparseQR_resid_fitted, qr, y, FALSE),
          valueClass = "dgeMatrix")

setMethod("qr.fitted", signature(qr = "sparseQR", y = "matrix"),
          function(qr, y, k)
          .Call(sparseQR_resid_fitted, qr, y, FALSE),
          valueClass = "dgeMatrix")

setMethod("qr.fitted", signature(qr = "sparseQR", y = "numeric"),
          function(qr, y, k)
          .Call(sparseQR_resid_fitted, qr, y, FALSE),
          valueClass = "dgeMatrix")

##
setMethod("solve", signature(a = "sparseQR", b = "ANY"),
	  function(a, b, ...) qr.coef(a, b))
