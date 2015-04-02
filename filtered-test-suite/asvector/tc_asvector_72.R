expected <- eval(parse(text="structure(list(), .Dim = 0L)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(), .Dim = 0L), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

