expected <- eval(parse(text="c(\"0\", \"25\", \"50\", \"75\", \"100\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(0, 25, 50, 75, 100), \"double\", 1, 6L, \"fg\", \"\", c(14L, 13L, 13L, 13L, 13L))"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

