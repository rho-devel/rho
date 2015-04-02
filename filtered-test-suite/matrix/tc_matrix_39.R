expected <- eval(parse(text="structure(c(-1, 4, 4, 9, 5, 1, 4, 8, 8, 2), .Dim = c(2L, 5L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(-1, 4, 4, 9, 5, 1, 4, 8, 8, 2, 6, 0, 2, 3, 8, 8, 4, 4, 2, 3, 4, 0, -1, 7, 2, 4, 2, 3, 5, 6, 6, 5, 4, 3, 7, -1, 3, 1, -1, 2, 32, 1, 4, 4), 2L, 5L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

