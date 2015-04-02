expected <- eval(parse(text="c(\"     3.142\", \"     3.142\", \"         1\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(3.14159265358979, 3.1415926535898, 1), \"double\", 10, 4L, \"g\", \"\", c(12L, 12L, 12L))"));   
.Internal(`formatC`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

