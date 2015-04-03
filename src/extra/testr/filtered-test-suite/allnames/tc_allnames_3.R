expected <- eval(parse(text="character(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)), TRUE, -1L, FALSE)"));   
.Internal(all.names(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));   
}, o=expected);   

