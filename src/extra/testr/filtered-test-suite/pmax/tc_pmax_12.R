expected <- eval(parse(text="c(2, 3, 4, 4, 5, 6, 7)"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE, 1:7, structure(c(2, 3, 4, 2, 2, 2), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"\"))))"));            
.Internal(pmax(argv[[1]], argv[[2]], argv[[3]]));            
}, o=expected);            

