expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(structure(c(0, 1, 1, 2, 2, 4, NA), .Names = c(\"ddiMatrix\", \"diagonalMatrix\", \"dMatrix\", \"sparseMatrix\", \"Matrix\", \"mMatrix\", \"ANY\")), structure(c(0, 1, 1, 1, 2, 2, 2, 3, 4, NA), .Names = c(\"dgCMatrix\", \"CsparseMatrix\", \"dsparseMatrix\", \"generalMatrix\", \"dMatrix\", \"sparseMatrix\", \"compMatrix\", \"Matrix\", \"mMatrix\", \"ANY\"))), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

