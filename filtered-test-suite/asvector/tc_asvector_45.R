expected <- eval(parse(text="list()"));               
test(id=0, code={               
argv <- eval(parse(text="list(integer(0), \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

