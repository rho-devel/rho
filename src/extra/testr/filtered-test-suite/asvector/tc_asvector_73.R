expected <- eval(parse(text="\"1\""));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(\"1\", .Tsp = c(1, 1, 1), class = \"ts\"), \"character\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

