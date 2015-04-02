expected <- eval(parse(text="\"003\""));        
test(id=0, code={        
argv <- eval(parse(text="list(3L, \"integer\", 3, 2L, \"d\", \"0\", 10L)"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

