expected <- eval(parse(text="structure(list(), .Dim = c(1L, 0L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(), .Dim = 0L))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

