expected <- eval(parse(text="structure(logical(0), .Dim = c(0L, 0L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 0L)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

