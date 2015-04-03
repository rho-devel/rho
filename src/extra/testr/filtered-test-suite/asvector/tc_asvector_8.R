expected <- eval(parse(text="c(-1L, -2L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(-1L, -2L), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

