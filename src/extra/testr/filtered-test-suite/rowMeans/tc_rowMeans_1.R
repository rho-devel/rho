expected <- eval(parse(text="c(3.5, 3, NaN, 3, 2.5, 3, 3.5, 4)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(3, 3, NA, 3, 3, 3, 3, 3, 4, 3, NA, NA, 2, 3, 4, 5), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\"))), 8, 2, TRUE)"));      
.Internal(rowMeans(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

