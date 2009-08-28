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

set.seed(1)
(m1 <- round(Matrix(rnorm(25), 5), 2))
str(lu1 <- lu(m1))
(luX <- expand(lu1))
stopifnot(all.equal(as(m1, "matrix"),
                    as(luX$P %*% (luX$L %*% luX$U), "matrix")))

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
stopifnot(identical(lu1, pmLU),
	  identical(ppm, with(xp, P %*% pm %*% t(Q))),
	  sapply(xp, is, class="Matrix"))


## these two should be the same, and `are' in some ways:
assert.EQ.mat(ppm, as(Ppm, "matrix"), tol = 1e-14)
## *however*
length(ppm@x)# 180
length(Ppm@x)# 317 !
table(Ppm@x == 0)# (194, 123) - has 123 "zero" and 14 ``almost zero" entries


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

## --- now a "large" (712 x 712) real data example

data(KNex)
mtm <- with(KNex, crossprod(mm))
ld.3 <- .Call("dsCMatrix_LDL_D", mtm, perm=TRUE,  "sumLog")
stopifnot(names(mtm@factors) == "sPDCholesky")
ld.4 <- .Call("dsCMatrix_LDL_D", mtm, perm=FALSE, "sumLog")# clearly slower
stopifnot(names(mtm@factors) == paste(c("sPD", "spD"),"Cholesky", sep=''))
c2 <- Cholesky(mtm, super = TRUE)
stopifnot(names(mtm@factors) == paste(c("sPD", "spD", "SPd"),
               "Cholesky", sep=''))

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
ip <- solve(p)
assert.EQ.mat(solve(ip), as(p,"matrix"))
