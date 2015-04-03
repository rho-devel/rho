expected <- eval(parse(text="structure(list(l = -1e-05, u = 1e-05, n = 2L), .Names = c(\"l\", \"u\", \"n\"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(-6.64448090063514e-06, 6.64454021993011e-06, 1, 0, 0.75, c(1.5, 2.75), 0)"));          
.Internal(pretty(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));          
}, o=expected);          

