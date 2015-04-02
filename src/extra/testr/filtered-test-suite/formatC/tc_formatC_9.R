expected <- eval(parse(text="c(\"0.060\", \"0.0060\", \"0.00060\", \"0.000060\", \"0.0000060\", \"0.00000060\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(0.0599, 0.00599, 0.000599, 5.99e-05, 5.99e-06, 5.99e-07), \"double\", 3, -2, \"fg\", \"#\", c(10, 11, 12, 13, 14, 15))"));   
.Internal(`formatC`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

