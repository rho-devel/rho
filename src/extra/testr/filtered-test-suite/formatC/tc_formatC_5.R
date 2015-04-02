expected <- eval(parse(text="c(\"-3\", \"-2\", \"-1\", \"0\", \"1\", \"2\", \"3\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(-3, -2, -1, 0, 1, 2, 3), \"double\", 1L, 4L, \"g\", \"\", c(12L, 12L, 12L, 12L, 12L, 12L, 12L))"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

