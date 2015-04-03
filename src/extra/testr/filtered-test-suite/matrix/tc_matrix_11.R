expected <- eval(parse(text="structure(c(420.223016031624, 290.609964753365, 290.609964753365, 200), .Dim = c(2L, 2L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(420.223016031624, 290.609964753365, 290.609964753365, 200), 2, 2, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

