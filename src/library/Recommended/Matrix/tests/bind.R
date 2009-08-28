#### Testing  cBind() & rBind()

library(Matrix)

source(system.file("test-tools.R", package = "Matrix"))# identical3() etc

### --- Dense Matrices ---

m1 <- m2 <- m <- Matrix(1:12, 3,4)
dimnames(m2) <- list(LETTERS[1:3],
                     letters[1:4])
dimnames(m1) <- list(NULL,letters[1:4])

stopifnot(identical(cBind ( m, 10*m) -> R,
                    cbind2( m, 10*m))); R
stopifnot(identical(cBind (m1,100+m1) -> R,
                    cbind2(m1,100+m1))); R
stopifnot(identical(cBind (m1, 10*m2) -> R,
                    cbind2(m1, 10*m2))); R
stopifnot(identical(cBind (m2, m1+m2) -> R,
                    cbind2(m2, m1+m2))); R

cBind(m1, MM = -1)
rBind(R1 = 10:11, m1)
cBind(0, Matrix(0+0:1, 1,2), 3:2)# FIXME? should warn - as with matrix()

as(rBind(0, Matrix(0+0:1, 1,2), 3:2),
   "sparseMatrix")
cBind(m2, 10*m2[nrow(m2):1 ,])# keeps the rownames from the first

(im <- cBind(I = 100, m))
str(im)
(mi <- cBind(m2, I = 1000))
str(mi)
(m1m <- cBind(m,I=100,m2))

### --- Diagonal / Sparse - had bugs

D4 <- Diagonal(4)
(D4T <- as(D4, "TsparseMatrix"))
D4C <- as(D4T, "CsparseMatrix")
c1 <- Matrix(0+0:3, 4, sparse=TRUE) ; r1 <- t(c1); r1

d4 <- rBind(Diagonal(4), 0:3)
m4 <- cBind(Diagonal(x=-1:2), 0:3)
c4. <- cBind(Diagonal(4), c1)
c.4 <- cBind(c1, Diagonal(4))
r4. <- rBind(Diagonal(4), r1)
r.4 <- rBind(r1, Diagonal(4))
assert.EQ.mat(d4, rBind(diag(4),    0:3))
assert.EQ.mat(m4, cBind(diag(-1:2), 0:3))
stopifnot(identical(Matrix(cbind(diag(3),0)), cbind2(Diagonal(3),0)),
	  is(d4, "sparseMatrix"), is(m4, "sparseMatrix"),
	  identical(t(d4), cBind(Diagonal(4),     0:3)),
	  identical(t(m4), rBind(Diagonal(x=-1:2), 0:3)))

### --- Sparse Matrices ---

identical4(cBind(diag(4), diag(4)),
           cBind(D4C, D4C),
           cBind(D4T, D4C),
           cBind(D4C, D4T))
nr <- 4
m. <- matrix(c(0, 2:-1),  nr ,6)
M <- Matrix(m.)
(mC <- as(M, "dgCMatrix"))
(mT <- as(M, "dgTMatrix"))
stopifnot(identical(mT, as(mC, "dgTMatrix")),
          identical(mC, as(mT, "dgCMatrix")))

for(v in list(0, 2, 1:0))
    for(fnam in c("cBind", "rBind")) {
        cat(fnam,"(m, v=", deparse(v),"), class(m) :")
        FUN <- get(fnam)
        for(m in list(M, mC, mT)) {
            cat("", class(m),"")
            assert.EQ.mat(FUN(v, m), FUN(v, m.)) ; cat(",")
            assert.EQ.mat(FUN(m, v), FUN(m., v)) ; cat(".")
        }
        cat("\n")
    }

cBind(0, mC); cBind(mC, 0)
cBind(0, mT); cBind(mT, 2)
cBind(diag(nr), mT)
stopifnot(identical(t(cBind(diag(nr),   mT)),
                      rBind(diag(nr), t(mT))))
(cc <- cBind(mC, 0,7,0, diag(nr), 0))
stopifnot(identical3(cc, cBind(mT, 0,7,0, diag(nr), 0),
                     as( cBind( M, 0,7,0, diag(nr), 0), "dgCMatrix")))

cBind(mC, 1, 100*mC, 0, 0:2)
cBind(mT, 1, 0, mT+10*mT, 0, 0:2)

## print() / show() of  non-structural zeros:
(m <- Matrix(c(0, 0, 2:0), 3, 5))
(m2 <- cBind(m,m))
(m4 <- rBind(m2,m2))
diag(m4)
for(i in 1:6) {
    m4[i, i ] <- i
    m4[i,i+1] <- 0
}
m4 ## now show some non-structural zeros:


cat('Time elapsed: ', proc.time(),'\n') # for ``statistical reasons''
