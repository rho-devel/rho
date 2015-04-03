expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Names = c(\"dtrMatrix\", \"MatrixFactorization\", \"ddenseMatrix\", \"triangularMatrix\", \"dMatrix\", \"denseMatrix\", \"Matrix\", \"mMatrix\")), \"factor\", FALSE)"));         
.Internal(`inherits`(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

