expected <- eval(parse(text="1"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\"), \"double\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

