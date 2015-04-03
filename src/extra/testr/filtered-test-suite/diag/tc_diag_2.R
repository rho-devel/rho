expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 0L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(NULL, 0L, 0L)"));           
.Internal(diag(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

