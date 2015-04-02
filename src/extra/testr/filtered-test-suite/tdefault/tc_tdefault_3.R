expected <- eval(parse(text="structure(NA, .Dim = c(1L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(NA, .Dim = c(1L, 1L)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

