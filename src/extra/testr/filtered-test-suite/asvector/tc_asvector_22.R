expected <- eval(parse(text="c(-1, 3, 1, 1, 5, 1)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(-1, 3, 1, 1, 5, 1), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

