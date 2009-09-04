#### Methods for the virtual class 'triangularMatrix' of triangular matrices
#### Note that specific methods are in (8 different) ./?t?Matrix.R

setAs("triangularMatrix", "symmetricMatrix",
      function(from) as(as(from, "generalMatrix"), "symmetricMatrix"))

setAs("dgeMatrix", "triangularMatrix", function(from) asTri(from, "dtrMatrix"))
setAs("lgeMatrix", "triangularMatrix", function(from) asTri(from, "ltrMatrix"))
setAs("ngeMatrix", "triangularMatrix", function(from) asTri(from, "ntrMatrix"))

.tril.tr <- function(x, k = 0, ...) {  # are always square
    k <- as.integer(k[1])
    dd <- dim(x)
    stopifnot(-dd[1] <= k, k <= dd[1])  # had k <= 0
    if(k == 0 && x@uplo == "L") x
    else { ## more to do
        if(x@diag == "U") x <- .diagU2N(x, class(x), checkDense = TRUE)
        callNextMethod()
    }
}

.triu.tr <- function(x, k = 0, ...) {  # are always square
    k <- as.integer(k[1])
    dd <- dim(x)
    stopifnot(-dd[1] <= k, k <= dd[1])  # had k >= 0
    if(k == 0 && x@uplo == "U") x
    else { ## more to do
        if(x@diag == "U") x <- .diagU2N(x, class(x), checkDense = TRUE)
        callNextMethod()
    }
}

## In order to evade method dispatch ambiguity (with [CTR]sparse* and ddense*),
## but still remain "general"
## we use this hack instead of signature  x = "triangularMatrix" :

trCls <- names(getClass("triangularMatrix")@subclasses)
trCls. <- trCls[grep(".t.Matrix", trCls)]  # not "*Cholesky", "*Kaufman" ..
for(cls in trCls.) {
    setMethod("tril", cls, .tril.tr)
    setMethod("triu", cls, .triu.tr)
}

## ditto here:

for(cls in trCls)
    setMethod("isTriangular", signature(object = cls),
              function(object, ...) TRUE)
## instead of just for ....   signature(object = "triangularMatrix")

rm(trCls)

