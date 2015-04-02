expected <- eval(parse(text="c(\" 1000\", \"1e+07\", \"    1\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1000, 1e+07, 1), \"double\", 5, 4L, \"g\", \"\", c(12L, 12L, 12L))"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

