expected <- eval(parse(text="structure(\"Seed\", .Dim = c(1L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(\"Seed\", .Dim = c(1L, 1L)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

