expected <- eval(parse(text="structure(c(1, 1, 1, 1, 1, 1), .Dim = c(6L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1, 1, 1, 1, 1, 1), 1, 1, FALSE, NULL, TRUE, TRUE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

