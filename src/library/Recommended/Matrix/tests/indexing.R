## For both 'Extract' ("[") and 'Replace' ("[<-") Method testing

library(Matrix)

source(system.file("test-tools.R", package = "Matrix"))# identical3() etc

if(interactive()) {
    options(error = recover, warn = 1)
} else options(Matrix.verbose = TRUE, warn = 1)

### Dense Matrices

m <- Matrix(1:28 +0, nrow = 7)
validObject(m)
stopifnot(identical(m, m[]),
	  identical(m[2, 3],  16), # simple number
	  identical(m[2, 3:4], c(16,23)), # simple numeric of length 2
	  identical(m[NA,NA], as(Matrix(NA, 7,4), "dMatrix")))

m[2, 3:4, drop=FALSE] # sub matrix of class 'dgeMatrix'
m[-(4:7), 3:4]        # ditto; the upper right corner of 'm'

## rows or columns only:
m[1,]     # first row, as simple numeric vector
m[,2]     # 2nd column
m[,1:2]   # sub matrix of first two columns
m[-(1:6),, drop=FALSE] # not the first 6 rows, i.e. only the 7th
m[integer(0),] #-> 0 x 4 Matrix
m[2:4, numeric(0)] #-> 3 x 0 Matrix

## logical indexing
stopifnot(identical(m[2,3], m[(1:nrow(m)) == 2, (1:ncol(m)) == 3]),
          identical(m[2,], m[(1:nrow(m)) == 2, ]),
          identical(m[,3:4], m[, (1:4) >= 3]))

## dimnames indexing:
mn <- m
dimnames(mn) <- list(paste("r",letters[1:nrow(mn)],sep=""),
                     LETTERS[1:ncol(mn)])
checkMatrix(mn)
mn["rd", "D"]
stopifnot(identical(mn["rc", "D"], mn[3,4]), mn[3,4] == 24,
          identical(mn[, "A"], mn[,1]), mn[,1] == 1:7,
          identical(mn[c("re", "rb"), "B"], mn[c(5,2), 2])
          )

mo <- m
m[2,3] <- 100
m[1:2, 4] <- 200
m[, 1] <- -1
m[1:3,]

m. <- as.matrix(m)

## m[ cbind(i,j) ] indexing:
iN <- ij <- cbind(1:6, 2:3)
iN[2:3,] <- iN[5,2] <- NA
stopifnot(identical(m[ij], m.[ij]),
	  identical(m[iN], m.[iN]))

## testing operations on logical Matrices rather more than indexing:
g10 <- m [ m > 10 ]
stopifnot(18 == length(g10))
stopifnot(10 == length(m[ m <= 10 ]))
sel <- (20 <  m) & (m <  150)
sel.<- (20 <  m.)& (m.<  150)
nsel <-(20 >= m) | (m >= 150)
(ssel <- as(sel, "sparseMatrix"))
stopifnot(is(sel, "lMatrix"), is(ssel, "lsparseMatrix"),
	  identical3(as.mat(sel.), as.mat(sel), as.mat(ssel)),
	  identical3(!sel, !ssel, nsel), # !<sparse> is typically dense
	  identical3(m[ sel],  m[ ssel], as.matrix(m)[as.matrix( ssel)]),
	  identical3(m[!sel],  m[!ssel], as.matrix(m)[as.matrix(!ssel)])
	  )

## more sparse Matrices --------------------------------------

m <- 1:800
set.seed(101) ; m[sample(800, 600)] <- 0
m <- Matrix(m, nrow = 40)
mm <- as(m, "matrix")
dimnames(mm) <- NULL ## << workaround: as(<sparse>, "matrix") has NULL dimnames
str(mC <- as(m, "dgCMatrix"))
str(mT <- as(m, "dgTMatrix"))
stopifnot(identical(mT, as(mC, "dgTMatrix")),
	  identical(mC, as(mT, "dgCMatrix")))

mC[,1]
mC[1:2,]
mC[7,  drop = FALSE]
assert.EQ.mat(mC[1:2,], mm[1:2,])

## *repeated* (aka 'duplicated') indices - did not work at all ...
i <- rep(8:10,2)
j <- c(2:4, 4:3)
assert.EQ.mat(mC[i,], mm[i,])
assert.EQ.mat(mC[,j], mm[,j])
## FIXME? assert.EQ.mat(mC[,NA], mm[,NA]) -- mC[,NA] is all 0 "instead" of all NA
## MM currently thinks we should  NOT  allow  <sparse>[ <NA> ]
assert.EQ.mat(mC[i, 2:1], mm[i, 2:1])
assert.EQ.mat(mC[c(4,1,2:1), j], mm[c(4,1,2:1), j])
assert.EQ.mat(mC[i,j], mm[i,j])
set.seed(7)
for(n in 1:50) {
    i <- sample(sample(nrow(mC), 7), 20, replace = TRUE)
    j <- sample(sample(ncol(mC), 6), 17, replace = TRUE)
    assert.EQ.mat(mC[i,j], mm[i,j])
}

##---- Symmetric indexing of symmetric Matrix ----------
m. <- mC; m.[, c(2, 7:12)] <- 0
validObject(S <- crossprod(add.simpleDimnames(m.) %% 100))
ss <- as(S, "matrix")
ds <- as(S, "denseMatrix")
## NA-indexing of *dense* Matrices: should work as traditionally
assert.EQ.mat(ds[NA,NA], ss[NA,NA])
assert.EQ.mat(ds[NA,  ], ss[NA,])
assert.EQ.mat(ds[  ,NA], ss[,NA])
stopifnot(identical(ds[2 ,NA], ss[2,NA]),
          identical(ds[NA, 1], ss[NA, 1]))
T <- as(S, "TsparseMatrix")
## non-repeated indices:
i <- c(7:5, 2:4);assert.EQ.mat(T[i,i], ss[i,i])
## NA in indices  -- check that we get a helpful error message:
i[2] <- NA
er <- tryCatch(T[i,i], error = function(e)e)
stopifnot(as.logical(grep("indices.*sparse Matrices", er$message)))

N <- nrow(T)
set.seed(11)
for(n in 1:50) {
    i <- sample(N, max(2, sample(N,1)), replace = FALSE)
    validObject(Tii <- T[i,i])
    stopifnot(is(Tii, "dsTMatrix"), # remained symmetric Tsparse
              identical(t(Tii), t(T)[i,i]))
    assert.EQ.mat(Tii, ss[i,i])
}

## repeated ones ``the challenge'' (to do smartly):
j <- c(4, 4, 9, 12, 9, 4, 17, 3, 18, 4, 12, 18, 4, 9)
assert.EQ.mat(T[j,j], ss[j,j])
## and another two sets  (a, A) &  (a., A.) :
a <- matrix(0, 6,6)
a[upper.tri(a)] <- (utr <- c(2, 0,-1, 0,0,5, 7,0,0,0, 0,0,-2,0,8))
ta <- t(a); ta[upper.tri(a)] <- utr; a <- t(ta)
diag(a) <- c(0,3,0,4,6,0)
A <- as(Matrix(a), "TsparseMatrix")
A. <- A
diag(A.) <- 10 * (1:6)
a. <- as(A., "matrix")
## More testing {this was not working for a long time..}
set.seed(1)
for(n in 1:100) {
    i <- sample(1:nrow(A), 3+2*rpois(1, lam=3), replace=TRUE)
    Aii  <- A[i,i]
    A.ii <- A.[i,i]
    stopifnot(class(Aii) == class(A),
              class(A.ii) == class(A.))
    assert.EQ.mat(Aii , a [i,i])
    assert.EQ.mat(A.ii, a.[i,i])
    assert.EQ.mat(T[i,i], ss[i,i])
}


stopifnot(all.equal(mC[,3], mm[,3]),
	  identical(mC[ij], mm[ij]),
	  identical(mC[iN], mm[iN]))

assert.EQ.mat(mC[7, , drop=FALSE], mm[7, , drop=FALSE])
identical    (mC[7,   drop=FALSE], mm[7,   drop=FALSE]) # *vector* indexing

stopifnot(dim(mC[numeric(0), ]) == c(0,20), # used to give warnings
          dim(mC[, integer(0)]) == c(40,0),
          identical(mC[, integer(0)], mC[, FALSE]))
validObject(print(mT[,c(2,4)]))
stopifnot(all.equal(mT[2,], mm[2,]),
          ## row or column indexing in combination with t() :
          identical(mT[2,], t(mT)[,2]),
          identical(mT[-2,], t(t(mT)[,-2])),
          identical(mT[c(2,5),], t(t(mT)[,c(2,5)]))
          )
assert.EQ.mat(mT[4,, drop = FALSE], mm[4,, drop = FALSE])
stopifnot(identical3(mm[,1], mC[,1], mT[,1]),
	  identical3(mm[3,], mC[3,], mT[3,]),
	  identical3(mT[2,3], mC[2,3], 0),
	  identical(mT[], mT),
          identical4(       mm[c(3,7), 2:4],  as.mat( m[c(3,7), 2:4]),
                     as.mat(mT[c(3,7), 2:4]), as.mat(mC[c(3,7), 2:4]))
          )

x.x <- crossprod(mC)
stopifnot(class(x.x) == "dsCMatrix",
          class(x.x. <- round(x.x / 10000)) == "dsCMatrix",
          identical(x.x[cbind(2:6, 2:6)],
                    diag(x.x [2:6, 2:6])))
head(x.x.) # Note the *non*-structural 0's printed as "0"
tail(x.x., -3) # all but the first three lines

lx.x <- as(x.x, "lsCMatrix") # FALSE only for "structural" 0
(l10 <- lx.x[1:10, 1:10])# "lsC"
(l3 <-  lx.x[1:3, ])
m.x <- as.mat(x.x) # as.mat() *drops* (NULL,NULL) dimnames
stopifnot(class(l10) == "lsCMatrix", # symmetric indexing -> symmetric !
          identical(as.mat(lx.x), m.x != 0),
          identical(as.logical(lx.x), as.logical(m.x)),
          identical(as.mat(l10), m.x[1:10, 1:10] != 0),
          identical(as.mat(l3 ), m.x[1:3, ] != 0)
          )

##-- Sub*assignment* with repeated / duplicated index:
A <- Matrix(0,4,3) ; A[c(1,2,1), 2] <- 1 ; A
B <- A;              B[c(1,2,1), 2] <- 1:3; B; B. <- B
B.[3,] <- rbind(4:2)
diag(B.) <- 10 * diag(B.)
C <- B.; C[,2] <- C[,2];  C[1,] <- C[1,]; C[2:3,2:1] <- C[2:3,2:1]
stopifnot(identical(unname(as.matrix(A)),
		    local({a <- matrix(0,4,3); a[c(1,2,1), 2] <-  1 ; a})),
	  identical(unname(as.matrix(B)),
		    local({a <- matrix(0,4,3); a[c(1,2,1), 2] <- 1:3; a})),
	  identical(C, drop0(B.)))


## used to fail
n <- 5 ## or much larger
sm <- new("dsTMatrix", i=as.integer(1),j=as.integer(1),
          Dim=as.integer(c(n,n)), x = 1)
(cm <- as(sm, "CsparseMatrix"))
sm[2,]
stopifnot(sm[2,] == c(0:1, rep.int(0,ncol(sm)-2)),
	  sm[2,] == cm[2,],
	  sm[,3] == sm[3,],
	  all(sm[,-(1:3)] == t(sm[-(1:3),])), # all(<lge.>)
	  all(sm[,-(1:3)] == 0)
	  )

m0 <- Diagonal(5)
stopifnot(identical(m0[2,], m0[,2]),
	  identical(m0[,1], c(1,0,0,0,0)))
### Diagonal -- Sparse:
(m1 <- as(m0, "TsparseMatrix"))  # dtTMatrix
(m2 <- as(m0, "CsparseMatrix")) # dtCMatrix
m1g <- as(m1, "generalMatrix")
stopifnot(is(m1g, "dgTMatrix"))
assert.EQ.mat(m2[1:3,],    diag(5)[1:3,])
assert.EQ.mat(m2[,c(4,1)], diag(5)[,c(4,1)])
stopifnot(identical(m2[1:3,], as(m1[1:3,], "CsparseMatrix")),
          identical(Matrix:::uniqTsparse(m1[, c(4,2)]),
                    Matrix:::uniqTsparse(as(m2[, c(4,2)], "TsparseMatrix")))
          )## failed in 0.9975-11

(uTr <- new("dtTMatrix", Dim = c(3L,3L), diag="U"))
uTr[1,] <- 0
assert.EQ.mat(uTr, cbind(0, rbind(0,diag(2))))

M <- m0; M[1,] <- 0
stopifnot(identical(M, Diagonal(x=c(0, rep(1,4)))))
M <- m0; M[,3] <- 3 ; M ; stopifnot(is(M, "sparseMatrix"), M[,3] == 3)
checkMatrix(M)
M <- m0; M[1:3, 3] <- 0 ;M
T <- m0; T[1:3, 3] <- 10
stopifnot(identical(M, Diagonal(x=c(1,1, 0, 1,1))),
          is(T, "triangularMatrix"), identical(T[,3], c(10,10,10,0,0)))

M <- m1; M[1,] <- 0 ; M ; assert.EQ.mat(M, diag(c(0,rep(1,4))), tol=0)
M <- m1; M[,3] <- 3 ; stopifnot(is(M,"sparseMatrix"), M[,3] == 3)
checkMatrix(M)
M <- m1; M[1:3, 3] <- 0 ;M
assert.EQ.mat(M, diag(c(1,1, 0, 1,1)), tol=0)
T <- m1; T[1:3, 3] <- 10; checkMatrix(T)
stopifnot(is(T, "dtTMatrix"), identical(T[,3], c(10,10,10,0,0)))

M <- m2; M[1,] <- 0 ; M ; assert.EQ.mat(M, diag(c(0,rep(1,4))), tol=0)
M <- m2; M[,3] <- 3 ; stopifnot(is(M,"sparseMatrix"), M[,3] == 3)
checkMatrix(M)
M <- m2; M[1:3, 3] <- 0 ;M
assert.EQ.mat(M, diag(c(1,1, 0, 1,1)), tol=0)
T <- m2; T[1:3, 3] <- 10; checkMatrix(T)
stopifnot(is(T, "dtCMatrix"), identical(T[,3], c(10,10,10,0,0)))


## "Vector indices" -------------------
.iniDiag.example <- expression({
    D <- Diagonal(6)
    M <- as(D,"dgeMatrix")
    m <- as(D,"matrix")
    s <- as(D,"TsparseMatrix")
    S <- as(s,"CsparseMatrix")
})
eval(.iniDiag.example)
i <- c(3,1,6); v <- c(10,15,20)
## (logical,value) which both are recycled:
L <- c(TRUE, rep(FALSE,8)) ; z <- c(50,99)

## vector subassignment, both with integer & logical
## these now work correctly {though not very efficiently; hence warnings}
m[i] <- v # the role model: only first column is affected
M[i] <- v; assert.EQ.mat(M,m) # dge
D[i] <- v; assert.EQ.mat(D,m) # ddi -> dtT -> dgT
s[i] <- v; assert.EQ.mat(s,m) # dtT -> dgT
S[i] <- v; assert.EQ.mat(S,m); S # dtC -> dtT -> dgT -> dgC
stopifnot(identical(s,D))
## logical
eval(.iniDiag.example)
m[L] <- z
M[L] <- z; assert.EQ.mat(M,m)
D[L] <- z; assert.EQ.mat(D,m)
s[L] <- z; assert.EQ.mat(s,m)
S[L] <- z; assert.EQ.mat(S,m) ; S

## indexing [i]  vs  [i,] --- now ok
eval(.iniDiag.example)
stopifnot(identical5(m[i], M[i], D[i], s[i], S[i]))
stopifnot(identical5(m[L], M[L], D[L], s[L], S[L]))
## bordercase ' drop = .' *vector* indexing {failed till 2009-04-..)
stopifnot(identical5(m[i,drop=FALSE], M[i,drop=FALSE], D[i,drop=FALSE],
		     s[i,drop=FALSE], S[i,drop=FALSE]))
stopifnot(identical5(m[L,drop=FALSE], M[L,drop=FALSE], D[L,drop=FALSE],
		     s[L,drop=FALSE], S[L,drop=FALSE]))
##
assert.EQ.mat(D[i,], m[i,])
assert.EQ.mat(M[i,], m[i,])
assert.EQ.mat(s[i,], m[i,])
assert.EQ.mat(S[i,], m[i,])

assert.EQ.mat(D[,i], m[,i])
assert.EQ.mat(M[,i], m[,i])
assert.EQ.mat(s[,i], m[,i])
assert.EQ.mat(S[,i], m[,i])


## --- negative indices ----------
mc <- mC[1:5, 1:7]
mt <- mT[1:5, 1:7]
## sub matrix
assert.EQ.mat(mC[1:2, 0:3], mm[1:2, 0:3]) # test 0-index
stopifnot(identical(mc[-(3:5), 0:2], mC[1:2, 0:2]),
          identical(mt[-(3:5), 0:2], mT[1:2, 0:2]),
          identical(mC[2:3, 4],      mm[2:3, 4]))
assert.EQ.mat(mC[1:2,], mm[1:2,])
## sub vector
stopifnot(identical4(mc[-(1:4), ], mC[5, 1:7],
                     mt[-(1:4), ], mT[5, 1:7]))
stopifnot(identical4(mc[-(1:4), -(2:4)], mC[5, c(1,5:7)],
                     mt[-(1:4), -(2:4)], mT[5, c(1,5:7)]))

## mixing of negative and positive must give error
assertError(mT[-1:1,])

## Sub *Assignment* ---- now works (partially):
mt0 <- mt
mt[1, 4] <- -99
mt[2:3, 1:6] <- 0
mt
m2 <- mt+mt
m2[1,4] <- -200
m2[c(1,3), c(5:6,2)] <- 1:6
stopifnot(m2[1,4] == -200,
          as.vector(m2[c(1,3), c(5:6,2)]) == 1:6)
mt[,3] <- 30
mt[2:3,] <- 250
mt[1:5 %% 2 == 1, 3] <- 0
mt[3:1, 1:7 > 5] <- 0
mt

tt <- as(mt,"matrix")
ii <- c(0,2,5)
jj <- c(2:3,5)
tt[ii, jj] <- 1:6 # 0 is just "dropped"
mt[ii, jj] <- 1:6
assert.EQ.mat(mt, tt)

mt[1:5, 2:6]
as((mt0 - mt)[1:5,], "dsparseMatrix")# [1,5] and lines 2:3

mt[c(2,4), ] <- 0; stopifnot(as(mt[c(2,4), ],"matrix") == 0)
mt[2:3, 4:7] <- 33
checkMatrix(mt)
mt

mc[1,4] <- -99 ; stopifnot(mc[1,4] == -99)
mc[1,4] <-  00 ; stopifnot(mc[1,4] ==  00)
mc[1,4] <- -99 ; stopifnot(mc[1,4] == -99)
mc[1:2,4:3] <- 4:1; stopifnot(as.matrix(mc[1:2,4:3]) == 4:1)

mc[-1, 3] <- -2:1 # 0 should not be entered; 'value' recycled
mt[-1, 3] <- -2:1
stopifnot(mc@x != 0, mt@x != 0,
	  mc[-1,3] == -2:1, mt[-1,3] == -2:1) ## failed earlier

mc0 <- mc
mt0 <- as(mc0, "TsparseMatrix")
m0  <- as(mc0, "matrix")
set.seed(1)
for(i in 1:50) {
    mc <- mc0; mt <- mt0 ; m <- m0
    ev <- 1:5 %% 2 == round(runif(1))# 0 or 1
    j <- sample(ncol(mc), 1 + round(runif(1)))
    nv <- rpois(sum(ev) * length(j), lambda = 1)
    mc[ev, j] <- nv
     m[ev, j] <- nv
    mt[ev, j] <- nv
    if(i %% 10 == 1) print(mc[ev,j, drop = FALSE])
    stopifnot(as.vector(mc[ev, j]) == nv, ## failed earlier...
              as.vector(mt[ev, j]) == nv)
    validObject(mc) ; assert.EQ.mat(mc, m)
    validObject(mt) ; assert.EQ.mat(mt, m)
}

mc # no longer has non-structural zeros
mc[ii, jj] <- 1:6
mc[c(2,5), c(3,5)] <- 3.2
checkMatrix(mc)
m. <- mc
mc[4,] <- 0
mc

S <- as(Diagonal(5),"TsparseMatrix")
H <- Hilbert(9)
Hc <- as(round(H, 3), "dsCMatrix")# a sparse matrix with no 0 ...
(trH <- tril(Hc[1:5, 1:5]))
stopifnot(is(trH, "triangularMatrix"), trH@uplo == "L",
          is(S, "triangularMatrix"))

## triangular assignment
## the slick (but inefficient in case of sparse!) way to assign sub-diagonals:
## equivalent to tmp <- `diag<-`(S[,-1], -2:1); S[,-1] <- tmp
## which dispatches to (x="TsparseMatrix", i="missing",j="index", value="replValue")
diag(S[,-1]) <- -2:1 # used to give a wrong warning
S <- as(S,"triangularMatrix")
assert.EQ.mat(S, local({s <- diag(5); diag(s[,-1]) <- -2:1; s}))

trH[c(1:2,4), c(2:3,5)] <- 0 # gave an *error* upto Jan.2008
trH[ lower.tri(trH) ] <- 0   # ditto, because of callNextMethod()

m <- Matrix(0+1:28, nrow = 4)
m[-3,c(2,4:5,7)] <- m[ 3, 1:4] <- m[1:3, 6] <- 0
mT <- as(m, "dgTMatrix")
stopifnot(identical(mT[lower.tri(mT)],
                    m [lower.tri(m) ]))
lM <- upper.tri(mT, diag=TRUE)
mT[lM] <- 0
 m[lM] <- 0
assert.EQ.mat(mT, as(m,"matrix"))
mT[lM] <- -1:0
 m[lM] <- -1:0
assert.EQ.mat(mT, as(m,"matrix"))
(mT <- drop0(mT))

i <- c(1:2, 4, 6:7); j <- c(2:4,6)
H[i,j] <- 0
(H. <- round(as(H, "sparseMatrix"), 3)[ , 2:7])
Hc. <- Hc
Hc.[i,j] <- 0 ## now "works", but setting "non-structural" 0s
stopifnot(as.matrix(Hc.[i,j]) == 0)
Hc.[, 1:6]

## an example that failed for a long time
sy3 <- new("dsyMatrix", Dim = as.integer(c(2, 2)), x = c(14, -1, 2, -7))
checkMatrix(dm <- kronecker(Diagonal(2), sy3))# now sparse with new kronecker
dm <- Matrix(as.matrix(dm))# -> "dsyMatrix"
(s2 <- as(dm, "sparseMatrix"))
checkMatrix(st <- as(s2, "TsparseMatrix"))
stopifnot(is(s2, "symmetricMatrix"),
	  is(st, "symmetricMatrix"))
checkMatrix(s.32  <- st[1:3,1:2]) ## 3 x 2 - and *not* dsTMatrix
checkMatrix(s2.32 <- s2[1:3,1:2])
I <- c(1,4:3)
stopifnot(is(s2.32, "generalMatrix"),
          is(s.32,  "generalMatrix"),
          identical(as.mat(s.32), as.mat(s2.32)),
          identical3(dm[1:3,-1], asD(s2[1:3,-1]), asD(st[1:3,-1])),
          identical4(2, dm[4,3], s2[4,3], st[4,3]),
          identical3(diag(dm), diag(s2), diag(st)),
          is((cI <- s2[I,I]), "dsCMatrix"),
          is((tI <- st[I,I]), "dsTMatrix"),
          identical4(as.mat(dm)[I,I], as.mat(dm[I,I]), as.mat(tI), as.mat(cI))
          )

## now sub-assign  and check for consistency
## symmetric subassign should keep symmetry
st[I,I] <- 0; checkMatrix(st); stopifnot(is(st,"symmetricMatrix"))
s2[I,I] <- 0; checkMatrix(s2); stopifnot(is(s2,"symmetricMatrix"))
##
m <- as.mat(st)
 m[2:1,2:1] <- 4:1
st[2:1,2:1] <- 4:1
s2[2:1,2:1] <- 4:1
stopifnot(identical(m, as.mat(st)),
	  1:4 == as.vector(s2[1:2,1:2]),
	  identical(m, as.mat(s2)))

## now a slightly different situation for 's2' (had bug)
s2 <- as(dm, "sparseMatrix")
s2[I,I] <- 0; diag(s2)[2:3] <- -(1:2)
stopifnot(is(s2,"symmetricMatrix"), diag(s2) == c(0:-2,0))
t2 <- as(s2, "TsparseMatrix")
m <- as.mat(s2)
s2[2:1,2:1] <- 4:1
t2[2:1,2:1] <- 4:1
 m[2:1,2:1] <- 4:1
assert.EQ.mat(t2, m)
assert.EQ.mat(s2, m)
## and the same (for a different s2 !)
s2[2:1,2:1] <- 4:1
t2[2:1,2:1] <- 4:1
assert.EQ.mat(t2, m)# ok
assert.EQ.mat(s2, m)# failed in 0.9975-8


## m[cbind(i,j)] <- value:
m.[ cbind(3:5, 1:3) ] <- 1:3
stopifnot(m.[3,1] == 1, m.[4,2] == 2)
x.x[ cbind(2:6, 2:6)] <- 12:16
stopifnot(isValid(x.x, "dsCMatrix"),
	  12:16 == as.mat(x.x)[cbind(2:6, 2:6)])
(ne1 <- (mc - m.) != 0)
stopifnot(identical(ne1, 0 != abs(mc - m.)))
(ge <- m. >= mc) # contains "=" -> result is dense
ne. <- mc != m.  # was wrong (+ warning)
stopifnot(identical(!(m. < mc), m. >= mc),
	  identical(m. < mc, as(!ge, "sparseMatrix")),
	  identical(ne., drop0(ne1)))

d6 <- Diagonal(6)
ii <- c(1:2, 4:5)
d6[cbind(ii,ii)] <- 7*ii
stopifnot(is(d6, "ddiMatrix"), identical(d6, Diagonal(x=c(7*1:2,1,7*4:5,1))))

for(j in 3:6) { ## even and odd j used to behave differently
    M <- Matrix(0, j,j); m <- matrix(0, j,j)
    T  <- as(M, "TsparseMatrix")
    TG <- as(T, "generalMatrix")
    G <-  as(M, "generalMatrix")
    id <- cbind(1:j,1:j)
    i2 <- cbind(1:j,j:1)
    m[id] <- 1:j
    M[id] <- 1:j ; stopifnot(is(M,"symmetricMatrix"))
    T[id] <- 1:j ; stopifnot(is(T,"symmetricMatrix"))
    G[id] <- 1:j
    TG[id]<- 1:j
    m[i2] <- 10
    M[i2] <- 10 ; stopifnot(is(M,"symmetricMatrix"))
    T[i2] <- 10 ; stopifnot(is(T,"symmetricMatrix"))
    G[i2] <- 10
    TG[i2]<- 10
    ##
    assert.EQ.mat(M, m)
    assert.EQ.mat(T, m)
    assert.EQ.mat(G, m)
    assert.EQ.mat(TG,m)
}


## drop, triangular, ...
(M3 <- Matrix(upper.tri(matrix(, 3, 3)))) # ltC; indexing used to fail
T3 <- as(M3, "TsparseMatrix")
stopifnot(identical(drop(M3), M3),
	  identical4(drop(M3[,2, drop = FALSE]), M3[,2, drop = TRUE],
		     drop(T3[,2, drop = FALSE]), T3[,2, drop = TRUE]),
	  is(T3, "triangularMatrix"),
	  !is(T3[,2, drop=FALSE], "triangularMatrix")
	  )

(T6 <- as(as(kronecker(Matrix(c(0,0,1,0),2,2), t(T3)), "lMatrix"),
	  "triangularMatrix"))
T6[1:4, -(1:3)] # failed (trying to coerce back to ltTMatrix)
stopifnot(identical(T6[1:4, -(1:3)][2:3, -3],
		    spMatrix(2,2, i=c(1,2,2), j=c(1,1,2), x=rep(TRUE,3))))

M <- Diagonal(4); M[1,2] <- 2
M. <- as(M, "CsparseMatrix")
(R <- as(M., "RsparseMatrix"))
(Ms <- symmpart(M.))
Rs <- as(Ms, "RsparseMatrix")
stopifnot(isValid(M, "triangularMatrix"),
          isValid(M.,"triangularMatrix"),
          isValid(Ms, "dsCMatrix"),
          isValid(R,  "dtRMatrix"),
          isValid(Rs, "dsRMatrix") )
stopifnot(dim(M[2:3, FALSE]) == c(2,0),
          dim(R[2:3, FALSE]) == c(2,0),
          identical(M [2:3,TRUE], M [2:3,]),
          identical(M.[2:3,TRUE], M.[2:3,]),
          identical(R [2:3,TRUE], R [2:3,]),
          dim(R[FALSE, FALSE]) == c(0,0))

n <- 50000L
Lrg <- new("dgTMatrix", Dim = c(n,n))
diag(Lrg) <- 1:n
dLrg <- as(Lrg, "diagonalMatrix")
stopifnot(identical(Diagonal(x = 1:n), dLrg))
diag(dLrg) <- 1 + diag(dLrg)
Clrg <- as(Lrg,"CsparseMatrix")
Ctrg <- as(Clrg, "triangularMatrix")
diag(Ctrg) <- 1 + diag(Ctrg)
stopifnot(identical(Diagonal(x = 1+ 1:n), dLrg),
          identical(Ctrg, as(dLrg,"CsparseMatrix")))

cc <- capture.output(show(dLrg))# show(<diag>) used to error for large n

## Large Matrix indexing / subassignment
## ------------------------------------- (from ex. by Imran Rashid)
n <- 7000000
m <-  100000
nnz <- 20000

set.seed(12)
f <- sparseMatrix(i = sample(n, size=nnz, replace=TRUE),
                  j = sample(m, size=nnz, replace=TRUE))
str(f)
dim(f) # 6999863 x 99992
prod(dim(f)) # 699930301096 == 699'930'301'096  (~ 700'000 millions)
str(thisCol <-  f[,5000])# logi [~ 7 mio....]
sv <- as(thisCol, "sparseVector")
str(sv) ## "empty" !
validObject(spCol <- f[,5000, drop=FALSE])
## *not* identical(): as(spCol, "sparseVector")@length is "double"prec:
stopifnot(all.equal(as(spCol, "sparseVector"),
                    as(sv,   "nsparseVector"), tol=0))
f[,5762] <- thisCol # now "fine" <<<<<<<<<< FIXME uses LARGE objects
## is using  replCmat() in ../R/Csparse.R, then
##           replTmat() in ../R/Tsparse.R

fx <- sparseMatrix(i = sample(n, size=nnz, replace=TRUE),
                   j = sample(m, size=nnz, replace=TRUE),
                   x = round(10*rnorm(nnz)))
class(fx)## dgCMatrix
fx[,6000] <- (tC <- rep(thisCol, length=nrow(fx)))
thCol <- fx[,2000]
fx[,5762] <- thCol
stopifnot(is(f, "ngCMatrix"), is(fx, "dgCMatrix"),
	  identical(thisCol, f[,5762]),# perfect
	  identical(as.logical(fx[,6000]), tC),
	  identical(thCol,  fx[,5762]))

cat('Time elapsed: ', (.pt <- proc.time()),'\n') # "stats"
##
cat("checkMatrix() of all: \n---------\n")
Sys.setlocale("LC_COLLATE", "C")# to keep ls() reproducible
for(nm in ls()) if(is(.m <- get(nm), "Matrix")) {
    cat(nm, "\n")
    checkMatrix(.m, verbose = FALSE)
}
cat('Time elapsed: ', proc.time() - .pt,'\n') # "stats"

if(!interactive()) warnings()

