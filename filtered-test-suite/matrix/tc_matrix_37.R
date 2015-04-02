expected <- eval(parse(text="structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 2L, 3L, 4L, 5L), .Dim = 3:4)"));             
test(id=0, code={             
argv <- eval(parse(text="list(1:7, 3, 4, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

