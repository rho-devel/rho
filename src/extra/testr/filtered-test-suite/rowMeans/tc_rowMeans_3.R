expected <- eval(parse(text="c(-1.5, -1.5, NaN, 2, -1.5, -1.5, -1.5, -1.5)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(2, 2, NA, 2, 2, 2, 2, 2, -5, -5, NA, NA, -5, -5, -5, -5), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\"))), 8, 2, TRUE)"));      
.Internal(rowMeans(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

