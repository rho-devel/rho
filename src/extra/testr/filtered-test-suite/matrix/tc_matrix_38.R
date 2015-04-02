expected <- eval(parse(text="structure(c(1, 3, 2, -2, 16, 4, 3, 3, 1, 32, -3, 2, 3, 2, 5, 1), .Dim = c(4L, 4L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1, 3, 2, -2, 16, 4, 3, 3, 1, 32, -3, 2, 3, 2, 5, 1, 3, 4, 9, 0, 4, 16, 3, 5, 1, -1, 3, 2, -3, 4, 1, 2, 8, 0, 8, 5, 4, 2, 6, 5, 3, 1, 3, 1), 4L, 4L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

