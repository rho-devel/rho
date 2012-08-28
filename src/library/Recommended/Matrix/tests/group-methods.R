### Testing the group methods  --- some also happens in ./Class+Meth.R

library(Matrix)
source(system.file("test-tools.R", package = "Matrix"))# identical3() etc

set.seed(2001)

mm <- Matrix(rnorm(50 * 7), nc = 7)
xpx <- crossprod(mm)# -> "factors" in mm !
round(xpx, 3) # works via "Math2"

y <- rnorm(nrow(mm))
xpy <- crossprod(mm, y)
res <- solve(xpx, xpy)
signif(res, 4) # 7 x 1 Matrix

stopifnot(all(signif(res) == signif(res, 6)),
	  all(round (xpx) == round (xpx, 0)))

## exp(): component wise
signif(dd <- (expm(xpx) - exp(xpx)) / 1e34, 3)# 7 x 7

stopifnot(validObject(xpx),
          validObject(xpy),
          validObject(dd))

## "Math" also, for log() and [l]gamma() which need special treatment
stopifnot(identical(exp(res)@x, exp(res@x)),
          identical(log(abs(res))@x, log(abs((res@x)))),
          identical(lgamma(res)@x, lgamma(res@x)))


###--- sparse matrices ---------

m <- Matrix(c(0,0,2:0), 3,5)
(mC <- as(m, "dgCMatrix"))
sm <- sin(mC)
stopifnot(class(sm) == class(mC), class(mC) == class(mC^2),
          dim(sm) == dim(mC),
          class(0 + 100*mC) == class(mC),
          all.equal(0.1 * ((0 + 100*mC)/10), mC),
          all.equal(sqrt(mC ^ 2), mC),
          all.equal(m^m, mC^mC),
          identical(mC^2, mC * mC),
          identical(mC*2, mC + mC)
          )

x <- Matrix(rbind(0,cbind(0, 0:3,0,0,-1:2,0),0))
x # sparse
(x2 <- x + 10*t(x))
stopifnot(is(x2, "sparseMatrix"),
          identical(x2, t(x*10 + t(x))),
	  identical(x, as((x + 10) - 10, class(x))))

(px <- Matrix(x^x - 1))#-> sparse again
stopifnot(px@i == c(3,4,1,4),
          px@x == c(3,26,-2,3))

## From: "Florent D." .. Thu, 23 Feb 2012 -- bug report
##---> MM:  Make a regression test:
tst <- function(n, i = 1) {
    stopifnot(i >= 1, n >= i)
    D <- .sparseDiagonal(n)
    ee <- numeric(n) ; ee[i] <- 1
    stopifnot(all(D - ee == diag(n) - ee),
              all(D * ee == diag(n) * ee),
              all(ee - D == ee - diag(n)),
              {C <- (ee / D == ee / diag(n)); all(is.na(C) | C)},
              TRUE)
}
tmp <- sapply(1:16, tst)
i <- sapply(1:16, function(i) sample(i,1))
tmp <- mapply(tst, n= 1:16, i= i)


###----- Compare methods ---> logical Matrices ------------
l3 <- upper.tri(matrix(, 3, 3))
(ll3 <- Matrix(l3))
dt3 <- (99* Diagonal(3) + (10 * ll3 + Diagonal(3)))/10
(dsc <- crossprod(ll3))
stopifnot(validObject(ll3), validObject(dsc),
          identical(ll3, t(t(ll3))),
          identical(dsc, t(t(dsc))),
          isValid(dsc + 3 * Diagonal(nrow(dsc)), "dsCMatrix"),
          isValid(dt3, "triangularMatrix"),   # remained triangular
          isValid(dt3 > 0, "triangularMatrix")# ditto
          )

(lm1 <- dsc >= 1) # now ok
(lm2 <- dsc == 1) # now ok
nm1 <- as(lm1, "nMatrix")
(nm2 <- as(lm2, "nMatrix"))

stopifnot(validObject(lm1), validObject(lm2),
          validObject(nm1), validObject(nm2),
          identical(dsc, as(dsc * as(lm1, "dMatrix"), "dsCMatrix")))

crossprod(lm1) # lm1: "lsC*"
cnm1 <- crossprod(nm1)
stopifnot(is(cnm1, "symmetricMatrix"), ## whereas the %*% is not:
	  Q.eq(cnm1, nm1 %*% nm1))
dn1 <- as(nm1, "denseMatrix")
stopifnot(all(dn1 == nm1))

dsc[2,3] <- NA ## now has an NA (and no longer is symmetric)
##          ----- and "everything" is different
## also add "non-structural 0":
dsc@x[1] <- 0
dsc
dsc/ 5
dsc + dsc
dsc - dsc
dsc + 1 # -> no longer sparse
Tsc <- as(dsc, "TsparseMatrix")
dsc. <- drop0(dsc)
stopifnot(identical(dsc., Matrix((dsc + 1) -1)),
	  identical(as(-Tsc,"CsparseMatrix"), (-1) * Tsc),
	  identical(-dsc., (-1) * dsc.),
	  identical3(-Diagonal(3), Diagonal(3, -1), (-1) * Diagonal(3)),
	  identical(dsc., Matrix((Tsc + 1) -1)), # ok (exact arithmetic)
	  Q.eq(0 != dsc, dsc != Matrix(0, 3, 3)),
	  Q.eq(0 != dsc, dsc != c(0,0)) # with a warning ("not multiple ..")
	  )
str(lm1 <- dsc >= 1) # now ok (NA in proper place, however:
lm1 ## NA used to print as ' ' , now 'N'
(lm2 <- dsc == 1)# ditto

ddsc <- kronecker(Diagonal(7), dsc)
isValid(ddv <- rowSums(ddsc, sparse=TRUE), "sparseVector")
sv <- colSums(kC <- kronecker(mC,kronecker(mC,mC)), sparse=TRUE)
EQ <- ddv == rowSums(ddsc)
na.ddv <- is.na(ddv)
sM <- Matrix(pmax(0, round(rnorm(50*15, -1.5), 2)), 50,15)
stopifnot(sv == colSums(kC), is.na(as.vector(ddv)) == na.ddv,
          isValid(sM/(-7:7), "CsparseMatrix"),
	  all(EQ | na.ddv))

## Just for print "show":
z <- round(rnorm(77), 2)
z[sample(77,10)] <- NA
(D <- Matrix(z, 7)) # dense
z[sample(77,15)] <- 0
(D <- Matrix(z, 7)) # sparse
abs(D) >= 0.5       # logical sparse

stopifnot(identical(crossprod(lm1),# "lgC": here works!
                    crossprod(as(lm1, "dMatrix"))
                    ))

## Systematically look at all "Ops" group generics for "all" Matrix classes
## -------------- Main issue: Detect infinite recursion problems
cl <- sapply(ls(), function(.) class(get(.)))
Mcl <- c(grep("Matrix$", cl, value=TRUE),
         grep("sparseVector", cl, value=TRUE))
table(Mcl)
M.objs <- names(Mcl[!duplicated(Mcl)])

cat("Checking all group generics for a set of arguments:\n",
    "---------------------------------------------------\n", sep='')
for(gr in getGroupMembers("Ops")) {
    cat(gr,"\n",paste(rep.int("=",nchar(gr)),collapse=""),"\n", sep='')
    for(f in getGroupMembers(gr)) {
	cat(sprintf("%9s : ", dQuote(f)))
	for(nm in M.objs) {
	    M <- get(nm, inherits=FALSE)
            nm <- NROW(M)
	    cat("o")
	    for(x in list(TRUE, -3.2, 0L, seq_len(nm))) {
                cat(".")
		validObject(r1 <- do.call(f, list(M,x)))
		validObject(r2 <- do.call(f, list(x,M)))
		stopifnot(dim(r1) == dim(M), dim(r2) == dim(M))
	    }
            ## M  o  <sparseVector>
            x <- numeric(nm)
            x[c(1,length(x))] <- 1:2
            sv <- as(x, "sparseVector")
            cat("s.")
            validObject(r3 <- do.call(f, list(M, sv)))
            stopifnot(dim(r3) == dim(M))
	}
	cat("\n")
    }
}

if(FALSE) {## These are not yet there
lm1 & lm2
lm1 | lm2
}


cat('Time elapsed: ', proc.time(),'\n') # for ``statistical reasons''
