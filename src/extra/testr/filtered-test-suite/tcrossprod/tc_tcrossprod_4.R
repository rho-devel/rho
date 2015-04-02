expected <- eval(parse(text="structure(c(3, 0), .Dim = 1:2)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(2, 3), structure(c(0, 0, 1, 0), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL)))"));       
.Internal(tcrossprod(argv[[1]], argv[[2]]));       
}, o=expected);       

