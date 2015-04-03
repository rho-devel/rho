expected <- eval(parse(text="c(\"0\", \"25\", \"50\", \"75\", \"100\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(0, 25, 50, 75, 100), \"double\", 1, 7L, \"fg\", \"\", c(16L, 15L, 15L, 15L, 15L))"));   
.Internal(`formatC`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

