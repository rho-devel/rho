expected <- eval(parse(text="structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), .Dim = c(4L, 3L))"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(4L, 3L))"));       
.Internal(row(argv[[1]]));       
}, o=expected);       

