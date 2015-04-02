expected <- eval(parse(text="c(NA_integer_, NA_integer_)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(NA, NaN), \"integer\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

