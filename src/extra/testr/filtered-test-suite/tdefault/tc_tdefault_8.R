expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(4L, 4L), .Dimnames = list(NULL, NULL))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(4L, 4L), .Dimnames = list(NULL, NULL)))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

