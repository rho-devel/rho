expected <- eval(parse(text="structure(numeric(0), .Dim = c(4L, 0L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(0, 0, 0, 0), 4L, 0L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

