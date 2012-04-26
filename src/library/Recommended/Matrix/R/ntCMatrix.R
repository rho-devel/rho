#### Logical Sparse Triangular Matrices in Compressed column-oriented format

setAs("ntCMatrix", "matrix",
      function(from) as(as(from, "denseMatrix"), "matrix"))
setAs("matrix", "ntCMatrix",
      function(from) as(as(from, "dtCMatrix"), "ntCMatrix"))

setAs("ntCMatrix", "TsparseMatrix",
      function(from) .Call(Csparse_to_Tsparse, from, TRUE))

setAs("ntCMatrix", "ngCMatrix",
      function(from) copyClass(diagU2N(from), "ngCMatrix"))


## --- these now happen using  "nCsparseMatrix" -- in ./ngCMatrix.R
##
## .ntC2d <- function(from)
##     new("dtCMatrix", i = from@i, p = from@p,
## 	x = rep.int(1, length(from@i)), uplo = from@uplo,
## 	diag = from@diag, Dim = from@Dim, Dimnames = from@Dimnames)

## .ntC2l <- function(from)
##     new("ltCMatrix", i = from@i, p = from@p,
## 	x = rep.int(TRUE, length(from@i)), uplo = from@uplo,
## 	diag = from@diag, Dim = from@Dim, Dimnames = from@Dimnames)

## Not needed, once we use "nCsparseMatrix" (-> ./ngCMatrix.R ):
setAs("ntCMatrix", "dMatrix", nC2d)
setAs("ntCMatrix", "dsparseMatrix", nC2d)
setAs("ntCMatrix", "dtCMatrix", nC2d)
##
setAs("ntCMatrix", "lMatrix", nC2l)
setAs("ntCMatrix", "lsparseMatrix", nC2l)
setAs("ntCMatrix", "ltCMatrix", nC2l)

## rm(.ntC2d,.ntC2l) # don't even keep "hidden"

setAs("ngCMatrix", "ntCMatrix", # to triangular, needed for triu,..
      function(from) as(as(as(from, "TsparseMatrix"),
                           "ntTMatrix"), "ntCMatrix"))

## setAs("ntCMatrix", "generalMatrix",
##       function(from) ......)

## setMethod("t", signature(x = "ntCMatrix"),
##           function(x) .Call(ntCMatrix_trans, x),
##           valueClass = "ntCMatrix")
