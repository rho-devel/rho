expected <- eval(parse(text="structure(numeric(0), .Dim = 0:1)"));             
test(id=0, code={             
argv <- eval(parse(text="list(numeric(0), 1, 1L, FALSE, NULL, TRUE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

