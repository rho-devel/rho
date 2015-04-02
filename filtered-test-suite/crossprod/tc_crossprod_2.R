expected <- eval(parse(text="structure(0, .Dim = c(1L, 1L))"));         
test(id=0, code={         
argv <- eval(parse(text="list(numeric(0), numeric(0))"));         
.Internal(crossprod(argv[[1]], argv[[2]]));         
}, o=expected);         

