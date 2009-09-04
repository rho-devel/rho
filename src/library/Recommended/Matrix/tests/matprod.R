library(Matrix)

### Matrix Products including  cross products

source(system.file("test-tools.R", package = "Matrix"))

m5 <- 1 + as(diag(-1:4)[-5,], "dgeMatrix")
## named dimnames:
dimnames(m5) <- list(Rows= LETTERS[1:5], paste("C", 1:6, sep=""))
m. <- as(m5, "matrix")
stopifnot(dim(m5) == 5:6,
          class(cm5 <- crossprod(m5)) == "dpoMatrix")
assert.EQ.mat((c.m5 <- t(m5) %*% m5), as(cm5, "matrix"))
stopifnot(as.vector(rep(1,6) %*% cm5) == colSums(cm5),
	  as.vector(cm5 %*% rep(1,6)) == rowSums(cm5))

## crossprod() with numeric vector RHS and LHS
## not sensical for tcrossprod() because of 'vec' --> cbind(vec) promotion:
assert.EQ.mat( crossprod(rep(1,5), m5),  rbind( colSums(m5)))
assert.EQ.mat( crossprod(rep(1,5), m.),  rbind( colSums(m5)))
assert.EQ.mat( crossprod(m5, rep(1,5)),  cbind( colSums(m5)))
assert.EQ.mat( crossprod(m., rep(1,5)),  cbind( colSums(m5)))

## classes differ
tc.m5 <- m5 %*% t(m5)    # "dge*", no dimnames (FIXME)
(tcm5 <- tcrossprod(m5)) # "dpo*"  w/ dimnames
assert.EQ.mat(tc.m5, mm5 <- as(tcm5, "matrix"))
## tcrossprod(x,y) :
assert.EQ.mat(tcrossprod(m5, m5), mm5)
assert.EQ.mat(tcrossprod(m5, m.), mm5)
assert.EQ.mat(tcrossprod(m., m5), mm5)

## simple cases with 'scalars' treated as 1x1 matrices:
d <- Matrix(1:5)
d %*% 2
10 %*% t(d)
assertError(3 %*% d)             # must give an error , similar to
assertError(5 %*% as.matrix(d))  # -> error

## right and left "numeric" and "matrix" multiplication:
(p1 <- m5 %*% c(10, 2:6))
(p2 <- c(10, 2:5) %*% m5)
(pd1 <- m5 %*% diag(1:6))
(pd. <- m5 %*% Diagonal(x = 1:6))
(pd2 <- diag (10:6)        %*% m5)
(pd..<- Diagonal(x = 10:6) %*% m5)
stopifnot(dim(crossprod(t(m5))) == c(5,5),
          c(class(p1),class(p2),class(pd1),class(pd2),
            class(pd.),class(pd..)) == "dgeMatrix")
assert.EQ.mat(p1, cbind(c(20,30,33,38,54)))
assert.EQ.mat(pd1, m. %*% diag(1:6))
assert.EQ.mat(pd2, diag(10:6) %*% m.)
assert.EQ.mat(pd., as(pd1,"matrix"))
assert.EQ.mat(pd..,as(pd2,"matrix"))

## check that 'solve' and '%*%' are inverses
set.seed(1)
A <- Matrix(rnorm(25), nc = 5)
y <- rnorm(5)
all.equal((A %*% solve(A, y))@x, y)
Atr <- new("dtrMatrix", Dim = A@Dim, x = A@x, uplo = "U")
all.equal((Atr %*% solve(Atr, y))@x, y)

### ------ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Sparse Matrix products
### ------
## solve() for dtC*
mc <- round(chol(crossprod(A)), 2)
B <- A[1:3,] # non-square on purpose
stopifnot(all.equal(sum(rowSums(B %*% mc)), 5.82424475145),
	  identical(tcrossprod(B, mc), t(tcrossprod(mc, B))))

m <- kronecker(Diagonal(2), mc)
stopifnot(is(mc, "Cholesky"),
	  is(m, "sparseMatrix"))
im <- solve(m)
round(im, 3)
itm <- solve(t(m))
iim <- solve(im) # should be ~= 'm' of course
iitm <- solve(itm)
I <- Diagonal(nrow(m))
(del <- c(mean(abs(as.numeric(im %*% m - I))),
	  mean(abs(as.numeric(m %*% im - I))),
	  mean(abs(as.numeric(im  - t(itm)))),
	  mean(abs(as.numeric( m  - iim))),
	  mean(abs(as.numeric(t(m)- iitm)))))
stopifnot(is(m, "triangularMatrix"), is(m, "sparseMatrix"),
          is(im, "dtCMatrix"), is(itm, "dtCMatrix"), is(iitm, "dtCMatrix"),
	  del < 1e-15)

t.crossprod <- function(x, y=NULL)
    if(getRversion() < "2.9.1" && R.version$`svn rev` < 48575)
    ## tcrossprod() bug in "base R"
    x %*% t(y) else tcrossprod(x, y)
## crossprod(.,.) & tcrossprod(),  mixing dense & sparse
v <- c(0,0,2:0)
(V <- Matrix(v, 5,1, sparse=TRUE))
sv <- as(v, "sparseVector")
a <- as.matrix(A)
cav <-  crossprod(a,v)
tva <- t.crossprod(v,a)
assert.EQ.mat(crossprod(A, V), cav) # gave infinite recursion
assert.EQ.mat(crossprod(A,sv), cav)
assert.EQ.mat(tcrossprod( sv, A), tva)
assert.EQ.mat(tcrossprod(t(V),A), tva)

M <- Matrix(0:5, 2,3) ; sM <- as(M, "sparseMatrix"); m <- as(M, "matrix")
v <- 1:3; v2 <- 2:1
sv  <- as( v, "sparseVector")
sv2 <- as(v2, "sparseVector")
tvm <- t.crossprod(v, m)
assert.EQ.mat(tcrossprod( v, M), tvm)
assert.EQ.mat(tcrossprod( v,sM), tvm)
assert.EQ.mat(tcrossprod(sv,sM), tvm)
assert.EQ.mat(tcrossprod(sv, M), tvm)
assert.EQ.mat(crossprod(M, sv2), crossprod(m, v2))
stopifnot(identical(tcrossprod(v, M), v %*% t(M)),
	  identical(tcrossprod(v,sM), v %*% t(sM)),
	  identical(tcrossprod(v, M), crossprod(v, t(M))),
	  identical(tcrossprod(sv,sM), sv %*% t(sM)),
	  identical(crossprod(sM, sv2), t(sM) %*% sv2),
	  identical(crossprod(M, v2), t(M) %*% v2))

## *unit* triangular :
t1 <- new("dtTMatrix", x= c(3,7), i= 0:1, j=3:2, Dim= as.integer(c(4,4)))
## from  0-diagonal to unit-diagonal {low-level step}:
tu <- t1 ; tu@diag <- "U"
cu <- as(tu, "dtCMatrix")
cl <- t(cu) # unit lower-triangular
cl10 <- cl %*% Diagonal(4, x=10)
assert.EQ.mat(cl10, as(cl, "matrix") %*% diag(4, x=10))
stopifnot(is(cl,"dtCMatrix"), cl@diag == "U")
(cu2 <- cu %*% cu)
cl2 <- cl %*% cl
validObject(cl2)
cl2
cu2. <- Diagonal(4) + Matrix(c(rep(0,9),14,0,0,6,0,0,0), 4,4)
stopifnot(all(cu2 == cu2.),# was wrong for ver. <= 0.999375-4
	  is(cu2, "dtCMatrix"), is(cl2, "dtCMatrix"), # triangularity preserved
	  cu2@diag == "U", cl2@diag == "U",# UNIT-triangularity preserved
	  all.equal(Diagonal(4, x=10) %*% cu,
		    Diagonal(4, x=10) %*% as.matrix(cu)),
          identical(t(cl2), cu2), # !!
          identical( crossprod(cu), Matrix( crossprod(as.matrix(cu)),sparse=TRUE)),
          identical(tcrossprod(cu), Matrix(tcrossprod(as.matrix(cu)),sparse=TRUE)))
tr8 <- kronecker(rbind(c(2,0),c(1,4)), cl2)
T8 <- tr8 %*% (tr8/2) # triangularity preserved?
T8.2 <- (T8 %*% T8) / 4
stopifnot(is(T8, "triangularMatrix"), T8@uplo == "L", is(T8.2, "dtCMatrix"))
mr8 <- as(tr8,"matrix")
m8. <- (mr8 %*% mr8 %*% mr8 %*% mr8)/16
assert.EQ.mat(T8.2, m8.)

data(KNex); mm <- KNex$mm
M <- mm[1:500, 1:200]
MT <- as(M, "TsparseMatrix")
cpr   <- t(mm) %*% mm
cpr.  <- crossprod(mm)
cpr.. <- crossprod(mm, mm)
stopifnot(is(cpr., "symmetricMatrix"),
          identical3(cpr, as(cpr., class(cpr)), cpr..))
## with dimnames:
m <- Matrix(c(0, 0, 2:0), 3, 5)
dimnames(m) <- list(LETTERS[1:3], letters[1:5])
m
p1 <- t(m) %*% m
(p1. <- crossprod(m))
t1 <- m %*% t(m)
(t1. <- tcrossprod(m))
stopifnot(isSymmetric(p1.),
          isSymmetric(t1.),
          identical(p1, as(p1., class(p1))),
          identical(t1, as(t1., class(t1))),
          identical(dimnames(p1), dimnames(p1.)),
          identical(dimnames(p1), list(colnames(m), colnames(m))),
          identical(dimnames(t1), dimnames(t1.))
          )

showMethods("%*%", class=class(M))

v1 <- rep(1, ncol(M))
str(r <-  M %*% Matrix(v1))
str(rT <- MT %*% Matrix(v1))
stopifnot(identical(r, rT))
str(r. <- M %*% as.matrix(v1))
stopifnot(identical4(r, r., rT, M %*% as(v1, "matrix")))

v2 <- rep(1,nrow(M))
r2 <- t(Matrix(v2)) %*% M
r2T <- v2 %*% MT
str(r2. <- v2 %*% M)
stopifnot(identical3(r2, r2., t(as(v2, "matrix")) %*% M))


## Sparse Cov.matrices from  Harri Kiiveri @ CSIRO
a <- matrix(0,5,5)
a[1,2] <- a[2,3] <- a[3,4] <- a[4,5] <- 1
a <- a + t(a) + 2*diag(5)
b <- as(a, "dsCMatrix") ## ok, but we recommend to use Matrix() ``almost always'' :
(b. <- Matrix(a, sparse = TRUE))
stopifnot(identical(b, b.))

## calculate conditional variance matrix ( vars 3 4 5 given 1 2 )
(B2 <- b[1:2, 1:2])
bb <- b[1:2, 3:5]
stopifnot(is(B2, "dsCMatrix"), # symmetric indexing keeps symmetry
	  identical(as.mat(bb), rbind(0, c(1,0,0))),
	  ## TODO: use fully-sparse cholmod_spsolve() based solution :
	  is(z.s <- solve(B2, bb), "sparseMatrix"))
assert.EQ.mat(B2 %*% z.s, as(bb, "matrix"))
## -> dense RHS and dense result
z. <- solve(as(B2, "dgCMatrix"), bb)
z  <- solve( B2, as(bb,"dgeMatrix"))
stopifnot(identical(z, z.))
## finish calculating conditional variance matrix
v <- b[3:5,3:5] - crossprod(bb,z)
stopifnot(all.equal(as.mat(v),
		    matrix(c(4/3, 1:0, 1,2,1, 0:2), 3), tol = 1e-14))


###--- "logical" Matrices : ---------------------

##__ FIXME __ now works for lsparse* and nsparse* but not yet for  lge* and nge* !

## Robert's Example, a bit more readable
fromTo <- rbind(c(2,10),
                c(3, 9))
N <- 10
nrFT <- nrow(fromTo)
rowi <- rep.int(1:nrFT, fromTo[,2]-fromTo[,1] + 1) - 1:1
coli <- unlist(lapply(1:nrFT, function(x) fromTo[x,1]:fromTo[x,2])) - 1:1

## "n" --- nonzero pattern Matrices
sM  <- new("ngTMatrix", i = rowi, j=coli, Dim=as.integer(c(N,N)))
sM # nice

sm <- as(sM, "matrix")
sM %*% sM
assert.EQ.mat(sM %*% sM,        sm %*% sm)
assert.EQ.mat(t(sM) %*% sM,
              (t(sm) %*% sm) > 0, tol=0)
crossprod(sM)
tcrossprod(sM)
stopifnot(identical(as( crossprod(sM), "ngCMatrix"), t(sM) %*%   sM),
          identical(as(tcrossprod(sM), "ngCMatrix"),  sM  %*% t(sM)))

assert.EQ.mat( crossprod(sM),  crossprod(sm) > 0)
assert.EQ.mat(tcrossprod(sM), as(tcrossprod(sm),"matrix") > 0)

## "l" --- logical Matrices -- use usual 0/1 arithmetic
nsM <- sM
sM  <- as(sM, "lMatrix")
sm <- as(sM, "matrix")
stopifnot(identical(sm, as.matrix(nsM)))
sM %*% sM
assert.EQ.mat(sM %*% sM,        sm %*% sm)
assert.EQ.mat(t(sM) %*% sM,
              t(sm) %*% sm, tol=0)
crossprod(sM)
tcrossprod(sM)
stopifnot(identical( crossprod(sM), as(t(sM) %*% sM, "symmetricMatrix")),
          identical(tcrossprod(sM), forceSymmetric(sM %*% t(sM))))
assert.EQ.mat( crossprod(sM),  crossprod(sm))
assert.EQ.mat(tcrossprod(sM), as(tcrossprod(sm),"matrix"))


## A sparse example - with *integer* matrix:
M <- Matrix(cbind(c(1,0,-2,0,0,0,0,0,2.2,0),
                  c(2,0,0,1,0), 0, 0, c(0,0,8,0,0),0))
t(M)
(-4:5) %*% M
stopifnot(as.vector(print(t(M %*% 1:6))) ==
          c(as(M,"matrix") %*% 1:6))
(M.M <- crossprod(M))
MM. <- tcrossprod(M)
stopifnot(class(MM.) == "dsCMatrix",
          class(M.M) == "dsCMatrix")

M3 <- Matrix(c(rep(c(2,0),4),3), 3,3, sparse=TRUE)
I3 <- as(Diagonal(3), "CsparseMatrix")
m3 <- as.matrix(M3)
iM3 <- solve(m3)
stopifnot(all.equal(iM3, matrix(c(3/2,0,-1,0,1/2,0,-1,0,1), 3)))
assert.EQ.mat(solve(as(M3, "sparseMatrix")), iM3)
assert.EQ.mat(solve(I3,I3), diag(3))
assert.EQ.mat(solve(M3, I3), iM3)# was wrong because I3 is unit-diagonal
assert.EQ.mat(solve(m3, I3), iM3)# gave infinite recursion in (<=) 0.999375-10


## even simpler
m <- matrix(0, 4,7); m[c(1, 3, 6, 9, 11, 22, 27)] <- 1
(mm <- Matrix(m))
(cm <- Matrix(crossprod(m)))
stopifnot(identical(crossprod(mm), cm))
(tm1 <- Matrix(tcrossprod(m))) #-> had bug in 'Matrix()' !
(tm2 <- tcrossprod(mm))
Im2 <- solve(tm2[-4,-4])
P <- as(as.integer(c(4,1,3,2)),"pMatrix")
p <- as(P, "matrix")
P %*% mm
assertError(mm %*% P) # dimension mismatch
assertError(m  %*% P) # ditto
assertError(crossprod(t(mm), P)) # ditto
stopifnot(isValid(tm1, "dsCMatrix"),
          all.equal(tm1, tm2, tol=1e-15),
	  identical(drop0(Im2 %*% tm2[1:3,]), Matrix(cbind(diag(3),0))),
          identical(p, as.matrix(P)),
	  identical(P %*% m, as.matrix(P) %*% m),
	  all(P %*% mm	==  P %*% m),
	  all(P %*% mm	-   P %*% m == 0),
	  all(t(mm) %*% P	==  t(m) %*% P),
	  identical(crossprod(m, P),
		    crossprod(mm, P)),
	  TRUE)

cat('Time elapsed: ', proc.time(),'\n') # for ``statistical reasons''
