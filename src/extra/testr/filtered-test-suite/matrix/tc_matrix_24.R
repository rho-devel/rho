expected <- eval(parse(text="structure(c(TRUE, FALSE, FALSE, TRUE), .Dim = c(2L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(TRUE, FALSE, FALSE, TRUE), 2L, 2L, TRUE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

