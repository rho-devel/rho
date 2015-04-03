expected <- eval(parse(text="\"1       \""));        
test(id=0, code={        
argv <- eval(parse(text="list(1, \"double\", 8, 5, \"g\", \"-\", 13)"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

