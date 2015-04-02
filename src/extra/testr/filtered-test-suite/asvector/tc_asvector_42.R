expected <- eval(parse(text="logical(0)"));               
test(id=0, code={               
argv <- eval(parse(text="list(NULL, \"logical\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

