expected <- eval(parse(text="structure(integer(0), .Dim = c(0L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c(\"row\", \"col\"))), 0, 2, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

