expected <- eval(parse(text="structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(list(1, 1, 1), 3L, 3L)"));           
.Internal(diag(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

