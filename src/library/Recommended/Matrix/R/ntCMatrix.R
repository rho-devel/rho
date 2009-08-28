#### Logical Sparse Triangular Matrices in Compressed column-oriented format

setAs("ntCMatrix", "matrix",
      function(from) as(as(from, "denseMatrix"), "matrix"))
setAs("matrix", "ntCMatrix",
      function(from) as(as(from, "dtCMatrix"), "ntCMatrix"))

setAs("ntCMatrix", "TsparseMatrix",
      function(from) .Call(Csparse_to_Tsparse, from, TRUE))

setAs("ntCMatrix", "ngCMatrix",
      function(from) copyClass(diagU2N(from), "ngCMatrix"))


.ntC2d <- function(from)
    new("dtCMatrix", i = from@i, p = from@p,
	x = rep.int(1, length(from@i)), uplo = from@uplo,
	diag = from@diag, Dim = from@Dim, Dimnames = from@Dimnames)

.ntC2l <- function(from)
    new("ltCMatrix", i = from@i, p = from@p,
	x = rep.int(TRUE, length(from@i)), uplo = from@uplo,
	diag = from@diag, Dim = from@Dim, Dimnames = from@Dimnames)

setAs("ntCMatrix", "dMatrix", .ntC2d)# < instead of "dtCMatrix"
setAs("ntCMatrix", "dsparseMatrix", .ntC2d)
setAs("ntCMatrix", "dtCMatrix", .ntC2d)

setAs("ntCMatrix", "lMatrix", .ntC2l)
setAs("ntCMatrix", "lsparseMatrix", .ntC2l)
setAs("ntCMatrix", "ltCMatrix", .ntC2l)

rm(.ntC2d,.ntC2l) # don't even keep "hidden"

setAs("ngCMatrix", "ntCMatrix", # to triangular, needed for triu,..
      function(from) as(as(as(from, "TsparseMatrix"),
                           "ntTMatrix"), "ntCMatrix"))

## setAs("ntCMatrix", "generalMatrix",
##       function(from) ......)

## setMethod("t", signature(x = "ntCMatrix"),
##           function(x) .Call(ntCMatrix_trans, x),
##           valueClass = "ntCMatrix")
