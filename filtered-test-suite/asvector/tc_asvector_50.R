expected <- eval(parse(text="list(\"1.3\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"1.3\", \"pairlist\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

