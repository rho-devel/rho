expected <- eval(parse(text="c(NA, \"NaN\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(NA, NaN), \"character\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

