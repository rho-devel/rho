expected <- eval(parse(text="list(NA)"));               
test(id=0, code={               
argv <- eval(parse(text="list(NA, \"list\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

