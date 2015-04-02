expected <- eval(parse(text="list(3.14159265358979)"));               
test(id=0, code={               
argv <- eval(parse(text="list(3.14159265358979, \"pairlist\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

