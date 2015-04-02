expected <- eval(parse(text="list(2)"));               
test(id=0, code={               
argv <- eval(parse(text="list(2, \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

