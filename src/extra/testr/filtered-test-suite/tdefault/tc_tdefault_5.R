expected <- eval(parse(text="structure(c(NA, NA, NA, NA, 0, 0, 0, 0, NA, NA, NA, NA, 0, 0, 0, 0), .Dim = c(4L, 4L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(NA, 0, NA, 0, NA, 0, NA, 0, NA, 0, NA, 0, NA, 0, NA, 0), .Dim = c(4L, 4L)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

