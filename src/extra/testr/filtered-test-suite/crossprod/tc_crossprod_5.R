expected <- eval(parse(text="structure(c(22, 28), .Dim = 1:2)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1, 2, 3), structure(c(1, 3, 5, 2, 4, 6), .Dim = c(3L, 2L)))"));         
.Internal(crossprod(argv[[1]], argv[[2]]));         
}, o=expected);         

