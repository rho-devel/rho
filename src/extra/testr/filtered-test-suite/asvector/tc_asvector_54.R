expected <- eval(parse(text="list(FALSE)"));               
test(id=0, code={               
argv <- eval(parse(text="list(FALSE, \"pairlist\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

