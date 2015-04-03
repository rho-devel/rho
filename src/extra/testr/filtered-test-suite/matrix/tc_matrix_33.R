expected <- eval(parse(text="structure(c(1+2i, 5+0i, 3-4i, -6+0i), .Dim = c(2L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1+2i, 3-4i, 5+0i, -6+0i), 2L, 2L, TRUE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

