expected <- eval(parse(text="c(200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 1e+05, 2e+05, 5e+05)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 1e+05, 2e+05, 5e+05), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

