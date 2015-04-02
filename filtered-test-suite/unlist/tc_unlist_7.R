expected <- eval(parse(text="structure(c(1, 1, 2, 2, 4), .Names = c(\"diagonalMatrix\", \"lMatrix\", \"sparseMatrix\", \"Matrix\", \"mMatrix\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(diagonalMatrix = 1, lMatrix = 1, sparseMatrix = 2, Matrix = 2, mMatrix = 4), .Names = c(\"diagonalMatrix\", \"lMatrix\", \"sparseMatrix\", \"Matrix\", \"mMatrix\")), FALSE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

