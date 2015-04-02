expected <- eval(parse(text="list(1L)"));               
test(id=0, code={               
argv <- eval(parse(text="list(1L, \"pairlist\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

