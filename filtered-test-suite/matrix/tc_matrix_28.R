expected <- eval(parse(text="structure(c(NA_complex_, NA_complex_, NA_complex_, NA_complex_, NA_complex_), .Dim = c(5L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(NA_complex_, 5L, 1L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

