expected <- eval(parse(text="numeric(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)), 0, 0, FALSE)"));         
.Internal(colMeans(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));         
}, o=expected);         

