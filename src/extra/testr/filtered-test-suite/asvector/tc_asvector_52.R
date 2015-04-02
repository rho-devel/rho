expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(NULL, \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

