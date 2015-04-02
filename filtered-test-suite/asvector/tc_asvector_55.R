expected <- eval(parse(text="integer(0)"));               
test(id=0, code={               
argv <- eval(parse(text="list(NULL, \"integer\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

