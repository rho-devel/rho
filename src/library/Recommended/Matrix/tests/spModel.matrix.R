library(Matrix)

## This is example(sp....) -- much extended

mEQ <- function(x,y) {
    ## first drop columns from y  which are all 0 :
    if(any(i0 <- colSums(abs(x)) == 0)) {
        message(gettextf("x had  %d  zero-columns", sum(i0)))
        x <- x[, !i0, drop=FALSE]
    }
    if(any(i0 <- colSums(abs(y)) == 0)) {
        message(gettextf("y had  %d  zero-columns", sum(i0)))
        y <- y[, !i0, drop=FALSE]
    }
    isTRUE(all.equal(x,y, tol=0))
}

##' Is  sparse.model.matrix() giving the "same" as dense model.matrix() ?
##' @param frml  formula
##' @param dat   data frame

##' @return logical
isEQsparseDense <-
    function(frml, dat,
             showFactors = isTRUE(getOption("verboseSparse")), ...)
{
    ## Author: Martin Maechler, Date: 21 Jul 2009
    stopifnot(inherits(frml, "formula"), is.data.frame(dat))
    if(showFactors)
        print(attr(terms(frml, data=dat), "factors"))
    mEQ(sparse.model.matrix(frml, dat, ...),
        Matrix(model.matrix(frml, dat, ...), sparse=TRUE))
}

### ------------ all the "datasets" we construct for use -------------
dd <- data.frame(a = gl(3,4), b = gl(4,1,12))# balanced 2-way
(dd3 <- cbind(dd, c = gl(2,6), d = gl(3,8)))
dd. <- dd3[- c(1, 13:15, 17), ]
set.seed(17)
dd4 <- cbind(dd, c = gl(2,6), d = gl(8,3))
dd4 <- cbind(dd4, x = round(rnorm(nrow(dd4)), 1))
dd4 <- dd4[- c(1, 13:15, 17), ]
##-> 'd' has unused levels
dM <- dd4
dM$X <- outer(10*rpois(nrow(dM), 2), 1:3)
dM$Y <- cbind(pmax(0, dM$x - .3), floor(4*rnorm(nrow(dM))))
str(dM)# contains *matrices*

options("contrasts") # the default:  "contr.treatment"
op <- options(sparse.colnames = TRUE) # for convenience

sparse.model.matrix(~ a + b, dd, contrasts = list(a="contr.sum"))
sparse.model.matrix(~ a + b, dd, contrasts = list(b="contr.SAS"))

## Sparse method is equivalent to the traditional one :
stopifnot(isEQsparseDense(~ a + b, dd),
          isEQsparseDense(~ a + b, dd, contrasts = list(a="contr.sum")),
          isEQsparseDense(~ a + b, dd, contrasts = list(a="contr.SAS")),
	  ## contrasts as *functions* or contrast *matrices* :
	  isEQsparseDense(~ a + b, dd,
			  contrasts = list(a=contr.sum, b=contr.treatment(4))),
	  isEQsparseDense(~ a + b, dd, contrasts =
			  list(a=contr.SAS(3),# << ok after 'contrasts<-' update
                               b = function(n, contr=TRUE, sparse=FALSE)
                               contr.sum(n=n, contr=contr, sparse=sparse))))

sm <- sparse.model.matrix(~a * b, dd,
                          contrasts = list(a= contr.SAS(3, sparse = TRUE)))
sm
stopifnot(all(sm == model.matrix( ~a * b, dd,
                                 contrasts= list(a= contr.SAS(3)))))


##
stopifnot(isEQsparseDense(~ a + b   + c + d, dd.))
stopifnot(isEQsparseDense(~ a + b:c + c + d, dd.))
## no intercept -- works too
stopifnot(isEQsparseDense(~ -1+ a + b   + c + d, dd.))
stopifnot(isEQsparseDense(~ 0 + a + b:c + c + d, dd.))



##
dim(mm <- Matrix(model.matrix(~ a + b + c + d, dd4), sparse=TRUE))
dim(sm <- sparse.model.matrix(~ a + b + c + d, dd4))
## dimension differ !!
stopifnot(mEQ(sm, mm)) ## but that's ok, since  mm has  all-0 column !
## look at this :
all(mm[,"d5"] == 0)  ## !!!! --- correct: a column of all 0  <--> dropped level!
stopifnot(all.equal(sm, mm[, - which("d5" == colnames(mm))])) ## indeed !
## i.e., sm has just dropped an all zero column --- which it should!

stopifnot(isEQsparseDense(~ 1 + sin(x) + b*c + a:x, dd4, show=TRUE))

stopifnot(isEQsparseDense(~    I(a) + b*c + a:x, dd4, show=TRUE))
## no intercept -- works too
stopifnot(isEQsparseDense(~ 0+ I(a) + b*c + a:x, dd4, show=TRUE))

f <- ~ 1 + a + b*c + a*x
attr(terms(f, data=dd4), "factors")
dim(mm <- Matrix(model.matrix(f, data=dd4), sparse=TRUE))
dim(sm <- sparse.model.matrix(f, data=dd4)) # ==
stopifnot(mEQ(sm, mm))

f <- ~ a*X + X*Y + a*c
attr(terms(f, data=dM), "factors")
dim(mm <- Matrix(model.matrix(f, data=dM), sparse=TRUE))
dim(sm <- sparse.model.matrix(f, data=dM))
stopifnot(mEQ(sm, mm))


f <- ~ 1 + a + b*c + a*x + b*d*x + b:c:d
attr(terms(f, data=dd4), "factors")
dim(mm <- Matrix(model.matrix(f, data=dd4), sparse=TRUE)) ## 19 100
dim(sm <- sparse.model.matrix(f, data=dd4)) # 19 88
stopifnot(mEQ(sm, mm))# {20 and 32  zero-columns ..}

## now get a bit courageous:
##

## stopifnot(isEQsparseDense(~ 1 + c + a:b:d,         dat=dd4))
dim(mm <- Matrix(model.matrix(~ 1 + a + b*c + a:b:c:d, data=dd4),
                 sparse=TRUE)) ## 19 202
dim(sm <- sparse.model.matrix(~ 1 + a + b*c + a:b:c:d, data=dd4)) # fails
stopifnot(mEQ(sm, mm))## {149 and 173 zero-columns !}

## stopifnot(isEQsparseDense(~ 1 + a + b*c + a:b:c:d, dat=dd4))
dim(mm <- Matrix(model.matrix(~ 1 + a + b:c + a:b:d, data=dd4),
                 sparse=TRUE)) ## 19 107
dim(sm <- sparse.model.matrix(~ 1 + a + b:c + a:b:d, data=dd4)) # fails
stopifnot(mEQ(sm, mm))


dim(mm <- Matrix(model.matrix(~ a*b*c +c*d, dd4), sparse=TRUE)) ## 19 38
dim(sm <- sparse.model.matrix(~ a*b*c +c*d, dd4))# 19 36
stopifnot(mEQ(sm, mm))


f1 <- ~ (a+b+c+d)^2 + (a+b):c:d + a:b:c:d
f2 <- ~ (a+b+c+d)^4 - a:b:c - a:b:d
    mm1 <- Matrix(model.matrix(f1, dd4), sparse=TRUE)
dim(mm2 <- Matrix(model.matrix(f2, dd4), sparse=TRUE))
    sm1 <- sparse.model.matrix(f1, dd4)
dim(sm2 <- sparse.model.matrix(f2, dd4))
stopifnot(identical(mm1,mm2),
          identical(sm1,sm2),
          mEQ(sm1, mm1))

cat('Time elapsed: ', proc.time(),'\n') # for ``statistical reasons''

if(!interactive()) warnings()
