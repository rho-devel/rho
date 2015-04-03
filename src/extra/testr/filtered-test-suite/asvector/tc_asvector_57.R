expected <- eval(parse(text="list(1L)"));               
test(id=0, code={               
argv <- eval(parse(text="list(1L, \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

