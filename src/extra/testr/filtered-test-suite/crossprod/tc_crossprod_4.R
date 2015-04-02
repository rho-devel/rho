expected <- eval(parse(text="structure(c(4, 10, 16), .Dim = c(3L, 1L))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1, 2, 3, 4, 5, 6), .Dim = 2:3), c(2, 1))"));         
.Internal(crossprod(argv[[1]], argv[[2]]));         
}, o=expected);         

