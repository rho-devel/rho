expected <- eval(parse(text="\" 5\""));        
test(id=0, code={        
argv <- eval(parse(text="list(5L, \"integer\", 2, 2L, \"d\", \"\", 10L)"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

