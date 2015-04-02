expected <- eval(parse(text="structure(0L, .Dim = c(1L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(0L, 1L, 1L, TRUE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

