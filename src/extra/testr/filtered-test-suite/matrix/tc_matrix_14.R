expected <- eval(parse(text="structure(c(0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i), .Dim = c(7L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(0+0i, 7L, 2L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

