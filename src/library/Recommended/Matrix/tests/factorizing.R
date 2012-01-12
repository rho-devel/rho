#### Matrix Factorizations  --- of all kinds

library(Matrix)

source(system.file("test-tools.R", package = "Matrix"))# identical3() etc


### "sparseQR" : Check consistency of methods
##   --------
data(KNex); mm <- KNex$mm; y <- KNex$y
stopifnot(is((Y <- Matrix(y)), "dgeMatrix"))
md <- as(mm, "matrix")                  # dense

system.time(mmq <- qr(mm))
system.time(mdq <- qr(md))# much (~ 150 x) slower

## qr.qy and qr.qty should be inverses
stopifnot(all.equal(qr.qy (mmq, qr.qty(mmq, y))@x, y),
          all.equal(qr.qty(mmq, qr.qy (mmq, y))@x, y),
          all.equal(qr.qty(mmq, y), qr.qty(mmq, Y)) )

## consistency of results dense and sparse
stopifnot(is.all.equal3(qr.coef  (mdq, y), qr.coef  (mmq,y)@x, qr.coef  (mmq,Y)@x) ,
          is.all.equal3(qr.resid (mdq, y), qr.resid (mmq,y)@x, qr.resid (mmq,Y)@x) ,
          is.all.equal3(qr.fitted(mdq, y), qr.fitted(mmq,y)@x, qr.fitted(mmq,Y)@x) )


### "denseLU"

## Testing expansions of factorizations {was ./expand.R, then in simple.R }
## new: [m x n]  where m and n  may differ
x. <- c(2^(0:5),9:1,-3:8, round(sqrt(0:16)))
set.seed(1)
for(nnn in 1:100) {
    y <- sample(x., replace=TRUE)
    m <- sample(2:6, 1)
    n <- sample(2:7, 1)
    x <- suppressWarnings(matrix(y, m,n))
    lux <- lu(x)# occasionally a warning about exact singularity
    xx <- with(expand(lux), (P %*% L %*% U))
    print(dim(xx))
    assert.EQ.mat(xx, x, tol = 16*.Machine$double.eps)
}

### "sparseLU"
por1 <- readMM(system.file("external/pores_1.mtx", package = "Matrix"))
lu1 <- lu(por1)
pm <- as(por1, "CsparseMatrix")
(pmLU <- lu(pm)) # -> show(<MatrixFactorization>)
xp <- expand(pmLU)
## permute rows and columns of original matrix
ppm <- pm[pmLU@p + 1:1, pmLU@q + 1:1]
Ppm <- pmLU@L %*% pmLU@U
## identical only as long as we don't keep the original class info:
stopifnot(identical3(lu1, pmLU, pm@factors$LU),# TODO === por1@factors$LU
	  identical(ppm, with(xp, P %*% pm %*% t(Q))),
	  sapply(xp, is, class="Matrix"))
## make sure 'factors' are *NOT* kept, when they should not:
spm <- solve(pm)
stopifnot(abs(as.vector(solve(Diagonal(30, x=10) %*% pm) / spm) - 1/10) < 1e-7,
	  abs(as.vector(solve(rep.int(4, 30)	  *  pm) / spm) - 1/ 4) < 1e-7)


## these two should be the same, and `are' in some ways:
assert.EQ.mat(ppm, as(Ppm, "matrix"), tol = 1e-14)
## *however*
length(ppm@x)# 180
length(Ppm@x)# 317 !
table(Ppm@x == 0)# (194, 123) - has 123 "zero" and 14 ``almost zero" entries

##-- determinant() and det() --- working via LU ---
m <- matrix(c(0, NA, 0, NA, NA, 0, 0, 0, 1), 3,3)
m0 <- rbind(0,cbind(0,m))
M <- as(m,"Matrix"); M ## "dsCMatrix" ...
M0 <- rBind(0, cBind(0, M))
dM  <- as(M, "denseMatrix")
dM0 <- as(M0,"denseMatrix")
if(FALSE) # FIXME "near-singular A"
lum <- lu(M)
if(FALSE) # FIXME "near-singular A"
lum0 <- lu(M0)
stopifnot(is.na(det(M)), is.na(det(dM)),
	  TRUE ## FIXME !! det(M0) == 0, det(dM0) == 0
	  )

###________ Cholesky() ________

##--------  LDL' ---- small exact examples

set.seed(1)
for(n in c(5:12)) {
    cat("\nn = ",n,"\n-------\n")
    rr <- mkLDL(n)
    ##    -------- from 'test-tools.R'
    stopifnot(all(with(rr, A == as(L %*% D %*% t(L),
                           "symmetricMatrix"))))
    d <- rr$d.half
    A <- rr$A
    R <- chol(A)
    print(d. <- diag(R))
    D. <- Diagonal(x= d.^2)
    L. <- t(R) %*% Diagonal(x = 1/d.)
    stopifnot(all.equal(as.matrix(D.), as.matrix(rr$ D)),
              all.equal(as.matrix(L.), as.matrix(rr$ L)))
    ##
    CAp <- Cholesky(A)# perm=TRUE --> Permutation:
    p <- CAp@perm + 1L
    P <- as(p, "pMatrix")
    ## the inverse permutation:
    invP <- solve(P)@perm
    lDet <- sum(2* log(d))# the "true" value
    ldet <- Matrix:::.diag.dsC(Chx = CAp, res.kind = "sumLog")
    ##
    CA	<- Cholesky(A,perm=FALSE)
    ldet2 <- Matrix:::.diag.dsC(Chx = CA, res.kind = "sumLog")
    ## not printing CAp : ends up non-integer for n >= 11
    mCAp <- as(CAp,"sparseMatrix")
    print(mCA  <- drop0(as(CA, "sparseMatrix")))
    stopifnot(identical(A[p,p], as(P %*% A %*% t(P),
				   "symmetricMatrix")),
	      all.equal(lDet, sum(log(Matrix:::.diag.dsC(Chx= CAp,res.kind="diag")))),
	      relErr(d.^2, Matrix:::.diag.dsC(Chx= CA, res.kind="diag")) < 1e-14,
	      all.equal(lDet, ldet),
	      all.equal(lDet, ldet2),
	      relErr(A[p,p], tcrossprod(mCAp)) < 1e-14)
}## for()

set.seed(17)
(rr <- mkLDL(4))
(CA <- Cholesky(rr$A))
stopifnot(all.equal(determinant(rr$A),
		    determinant(as(rr$A, "matrix"))))
A12 <- mkLDL(12, 1/10)

(r12 <- allCholesky(A12$A))
aCh.hash <- r12$r.all %*% (2^(2:0))
if(FALSE)## if(require("sfsmisc"))
split(rownames(r12$r.all), Duplicated(aCh.hash))

## TODO: find cases for both choices when we leave it to CHOLMOD to chose
for(n in 1:50) { ## used to seg.fault at n = 10 !
    mkA <- mkLDL(1+rpois(1, 30), 1/10)
    cat(sprintf("n = %3d, LDL-dim = %d x %d ", n, nrow(mkA$A), ncol(mkA$A)))
    r <- allCholesky(mkA$A, silentTry=TRUE)
    ## Compare .. apart from the NAs that happen from (perm=FALSE, super=TRUE)
    iNA <- apply(is.na(r$r.all), 1, any)
    cat(sprintf(" -> %3s NAs\n", if(any(iNA)) format(sum(iNA)) else "no"))
    stopifnot(aCh.hash[!iNA] == r$r.all[!iNA,] %*% (2^(2:0)))
##     cat("--------\n")
}


## This is a relatively small "critical example" :
A. <-
    new("dsCMatrix", Dim = c(25L, 25L), uplo = "U"
	, i = as.integer(
          c(0, 1, 2, 3, 4, 2, 5, 6, 0, 8, 8, 9, 3, 4, 10, 11, 6, 12, 13, 4,
            10, 14, 15, 1, 2, 5, 16, 17, 0, 7, 8, 18, 9, 19, 10, 11, 16, 20,
            0, 6, 7, 16, 17, 18, 20, 21, 6, 9, 12, 14, 19, 21, 22, 9, 11, 19,
            20, 22, 23, 1, 16, 24))
	##
	, p = c(0:6, 8:10, 12L, 15:16, 18:19, 22:23, 27:28, 32L, 34L, 38L, 46L, 53L, 59L, 62L)
	##
	, x = c(1, 1, 1, 1, 2, 100, 2, 40, 1, 2, 100, 6700, 100, 100, 13200,
	  1, 50, 4100, 1, 5, 400, 20, 1, 40, 100, 5600, 9100, 5000, 5,
	  100, 100, 5900, 100, 6200, 30, 20, 9, 2800, 1, 100, 8, 10, 8000,
	  100, 600, 23900, 30, 100, 2800, 50, 5000, 3100, 15100, 100, 10,
	  5600, 800, 4500, 5500, 7, 600, 18200))
validObject(A.)
## A1: the same pattern as  A.   just simply filled with '1's :
A1 <- A.; A1@x[] <- 1; A1@factors <- list()
A1.8 <- A1; diag(A1.8) <- 8
##
nT. <- as(AT <- as(A., "TsparseMatrix"),"nMatrix")
stopifnot(all(nT.@i <= nT.@j),
	  identical(qr(A1.8), qr(as(A1.8, "dgCMatrix"))))
CA <- Cholesky(A.)
stopifnot(isValid(CAinv <- solve(CA), "dsCMatrix"))
MA <- as(CA, "Matrix") # with a confusing warning -- FIXME!
isValid(MAinv <- solve(MA), "dtCMatrix")
## comparing MAinv with some solve(CA, system="...") .. *not* trivial? - TODO
##
CAinv2 <- solve(CA, Diagonal(nrow(A.)))
CAinv2 <- as(CAinv2, "symmetricMatrix")
stopifnot(identical(CAinv, CAinv2))

## FINALLY fix this "TODO":
try(    tc <- Cholesky(nT.)  )

for(p in c(FALSE,TRUE))
    for(L in c(FALSE,TRUE))
        for(s in c(FALSE,TRUE, NA)) {
            cat(sprintf("p,L,S = (%2d,%2d,%2d): ", p,L,s))
            r <- tryCatch(Cholesky(A., perm=p, LDL=L, super=s),
                          error = function(e)e)
            cat(if(inherits(r, "error")) " *** E ***" else
                sprintf("%3d", r@type),"\n", sep="")
        }
str(A., max=3) ## look at the 'factors'

facs <- A.@factors
names(facs) <- sub("Cholesky$", "", names(facs))
facs <- facs[order(names(facs))]

sapply(facs, class)
str(lapply(facs, slot, "type"))
## super = TRUE  currently always entails  LDL=FALSE :
## hence isLDL is TRUE for ("D" and not "S"):
sapply(facs, isLDL)

chkCholesky <- function(chmf, A) {
    stopifnot(is(chmf, "CHMfactor"),
              is(A, "Matrix"), isSymmetric(A))
    if(!is(A, "dsCMatrix"))
        A <- as(A, "dsCMatrix")
    L <- drop0(zapsmall(L. <- as(chmf, "Matrix")))
    cat("no. nonzeros in L {before / after drop0(zapsmall(.))}: ",
        c(nnzero(L.), nnzero(L)), "\n") ## 112, 95
    ecc <- expand(chmf)
    A... <- with(ecc, crossprod(crossprod(L,P)))
    stopifnot(all.equal(L., ecc$L, tol = 1e-14),
              all.equal(A,  A...,  tol = 1e-14, factorsCheck = FALSE))
    invisible(ecc)
}

c1.8 <- try(Cholesky(A1.8, super = TRUE))# works "always", interestingly ...
chkCholesky(c1.8, A1.8)



## --- now a "large" (712 x 712) real data example ---------------------------

data(KNex)
mtm <- with(KNex, crossprod(mm))
ld.3 <- .Call("dsCMatrix_LDL_D", mtm, perm=TRUE,  "sumLog")
stopifnot(names(mtm@factors) == "sPDCholesky")
ld.4 <- .Call("dsCMatrix_LDL_D", mtm, perm=FALSE, "sumLog")# clearly slower
stopifnot(names(mtm@factors) == paste(c("sPD", "spD"),"Cholesky", sep=''))
c2 <- Cholesky(mtm, super = TRUE)
stopifnot(names(mtm@factors) == paste(c("sPD", "spD", "SPd"),
               "Cholesky", sep=''))

r <- allCholesky(mtm)
r

## is now taken from cache
c1 <- Cholesky(mtm)

bv <- 1:nrow(mtm) # even integer
b <- matrix(bv)
## solve(c2, b) by default solves  Ax = b, where A = c2'c2 !
x <- solve(c2,b)
stopifnot(identical3(x, solve(c2, bv), solve(c2, b, system = "A")),
          all.equal(x, solve(mtm, b)))
for(sys in c("A", "LDLt", "LD", "DLt", "L", "Lt", "D", "P", "Pt")) {
    x <- solve(c2, b,  system = sys)
    cat(sys,":\n"); print(head(x))
    stopifnot(dim(x) == c(712, 1),
              identical(x, solve(c2, bv, system = sys)))
}

## log(|LL'|) - check if super = TRUE and simplicial give same determinant
ld1 <- .Call("CHMfactor_ldetL2", c1)
ld2 <- .Call("CHMfactor_ldetL2", c2)
(ld1. <- determinant(mtm))
## experimental
ld3 <- .Call("dsCMatrix_LDL_D", mtm, TRUE, "sumLog")
ld4 <- .Call("dsCMatrix_LDL_D", mtm, FALSE, "sumLog")
stopifnot(all.equal(ld1, ld2),
	  is.all.equal3(ld2, ld3, ld4),
	  all.equal(ld.3, ld3, tol = 1e-14),
	  all.equal(ld.4, ld4, tol = 1e-14),
	  all.equal(ld1, as.vector(ld1.$modulus), tol = 1e-14))

## Some timing measurements
mtm <- with(KNex, crossprod(mm))
I <- .symDiagonal(n=nrow(mtm))
set.seed(101); r <- runif(100)

system.time(D1 <- sapply(r, function(rho) Matrix:::ldet1.dsC(mtm + (1/rho) * I)))
## 0.842 on fast cmath-5
system.time(D2 <- sapply(r, function(rho) Matrix:::ldet2.dsC(mtm + (1/rho) * I)))
## 0.819
system.time(D3 <- sapply(r, function(rho) Matrix:::ldet3.dsC(mtm + (1/rho) * I)))
## 0.810
stopifnot(is.all.equal3(D1,D2,D3, tol = 1e-13))

## Updating LL'  should remain LL' and not become  LDL' :
if(FALSE) {
    data(Dyestuff, package = "lme4")
    Zt <- as(Dyestuff$Batch, "sparseMatrix")
} else {
    Zt <- new("dgCMatrix", Dim = c(6L, 30L), x = rep(1, 30),
              i = rep(0:5, each=5),
              p = 0:30, Dimnames = list(LETTERS[1:6], NULL))
}
Ut <- 0.78 * Zt
L <- Cholesky(tcrossprod(Ut), LDL = FALSE, Imult = 1)
L1 <- update(L, tcrossprod(Ut), mult = 1)
stopifnot(all.equal(L, L1))


## Schur() ----------------------
checkSchur <- function(A, SchurA = Schur(A), tol = 1e-14) {
    stopifnot(is(SchurA, "Schur"),
              isOrthogonal(Q <- SchurA@Q),
              all.equal(as.mat(A),
                        as.mat(Q %*% SchurA@T %*% t(Q)), tol = tol))
}

SH <- Schur(H5 <- Hilbert(5))
checkSchur(H5, SH)
checkSchur(Diagonal(x = 9:3))

p <- 4L
uTp <- new("dtpMatrix", x=c(2, 3, -1, 4:6, -2:1), Dim = c(p,p))
(uT <- as(uTp, "dtrMatrix"))
## Schur ( <general> )  <--> Schur( <triangular> )
Su <- Schur(uT) ;   checkSchur(uT, Su)
gT <- as(uT,"generalMatrix")
Sg <- Schur(gT) ;   checkSchur(gT, Sg)
Stg <- Schur(t(gT));checkSchur(t(gT), Stg)
Stu <- Schur(t(uT));checkSchur(t(uT), Stu)

stopifnot(identical3(Sg@T, uT, Su@T),
          identical(Sg@Q, as(diag(p), "dgeMatrix")),
          identical(Stg@T, as(t(gT[,p:1])[,p:1], "triangularMatrix")),
          identical(Stg@Q, as(diag(p)[,p:1], "dgeMatrix")),
          identical(Stu@T, Stg@T))
assert.EQ.mat(Stu@Q, as(Stg@Q,"matrix"), tol=0)

## the pedigreemm example where solve(.) failed:
p <- new("dtCMatrix", i = c(2L, 3L, 2L, 5L, 4L, 4:5), p = c(0L, 2L, 4:7, 7L),
	 Dim = c(6L, 6L), Dimnames = list(as.character(1:6), NULL),
	 x = rep.int(-0.5, 7), uplo = "L", diag = "U")
Sp <- Schur(p)
Sp. <- Schur(as(p,"generalMatrix"))
Sp.p <- Schur(crossprod(p))
## the last two failed
ip <- solve(p)
assert.EQ.mat(solve(ip), as(p,"matrix"))


## chol2inv() for a traditional matrix
assert.EQ.mat(     crossprod(chol2inv(chol(Diagonal(x = 5:1)))),
              C <- crossprod(chol2inv(chol(    diag(x = 5:1)))))
stopifnot(all.equal(C, diag((5:1)^-2)))
## failed in some versions because of a "wrong" implicit generic
