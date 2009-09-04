### Testing the group methods  --- some also happens in ./Class+Meth.R

library(Matrix)
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

###----- Compare methods ---> logical Matrices ------------
l3 <- upper.tri(matrix(, 3, 3))
(ll3 <- Matrix(l3))
(dsc <- crossprod(ll3))
stopifnot(validObject(ll3), validObject(dsc),
          identical(ll3, t(t(ll3))),
          identical(dsc, t(t(dsc)))
          )

(lm1 <- dsc >= 1) # now ok
(lm2 <- dsc == 1) # now ok
nm1 <- as(lm1, "nMatrix")
(nm2 <- as(lm2, "nMatrix"))

stopifnot(validObject(lm1), validObject(lm2),
          validObject(nm1), validObject(nm2),
          identical(dsc, as(dsc * as(lm1, "dMatrix"), "dsCMatrix")))

crossprod(lm1) # lm1: "lsC*"
crossprod(nm1)

dsc[2,3] <- NA ## now has an NA
##          ----- end "everything" is different
dsc
dsc/ 5
dsc + dsc
dsc - dsc
dsc + 1 # -> no longer sparse
stopifnot(identical(dsc, Matrix((dsc + 1) -1))) # ok (exact arithmetic)

str(lm1 <- dsc >= 1) # now ok (NA in proper place, however:
lm1 ## NA used to print as ' ' , now 'N'
(lm2 <- dsc == 1)# ditto

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

if(FALSE) {## These are not yet there
lm1 & lm2
lm1 | lm2
}


cat('Time elapsed: ', proc.time(),'\n') # for ``statistical reasons''
