expected <- eval(parse(text="c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"ANY\", \"abIndex\", \"ddenseMatrix\", \"diagonalMatrix\", \"dsparseMatrix\", \"lMatrix\", \"nMatrix\", \"nsparseVector\", \"pMatrix\", \"sparseVector\"), \"ANY\", NA_integer_, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

