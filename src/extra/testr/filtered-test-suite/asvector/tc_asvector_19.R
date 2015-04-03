expected <- eval(parse(text="numeric(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(NULL, \"double\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

