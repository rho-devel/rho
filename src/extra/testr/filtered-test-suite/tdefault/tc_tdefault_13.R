expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0, 0, 3.95252516672997e-323, 0, 0, 0, 0, 0), .Dim = c(1L, 12L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 3.95252516672997e-323, 0, 0, 0, 0, 0), .Dim = c(12L, 1L)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

