expected <- eval(parse(text="structure(list(3, 3, 3, 3, 3, \"fred\"), .Dim = c(3L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(3, 3, 3, 3, 3, \"fred\"), .Dim = 2:3))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

