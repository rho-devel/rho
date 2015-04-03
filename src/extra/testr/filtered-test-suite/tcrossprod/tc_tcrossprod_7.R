expected <- eval(parse(text="structure(0, .Dim = c(1L, 1L))"));       
test(id=0, code={       
argv <- eval(parse(text="list(0, 0)"));       
.Internal(tcrossprod(argv[[1]], argv[[2]]));       
}, o=expected);       

