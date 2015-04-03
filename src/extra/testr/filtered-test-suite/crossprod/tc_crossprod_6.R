expected <- eval(parse(text="structure(c(0, 2), .Dim = c(2L, 1L))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(0, 0, 1, 0), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL)), c(2, 3))"));         
.Internal(crossprod(argv[[1]], argv[[2]]));         
}, o=expected);         

