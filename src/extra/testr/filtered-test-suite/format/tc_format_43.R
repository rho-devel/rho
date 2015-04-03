expected <- eval(parse(text="\"1.2e+07\""));            
test(id=0, code={            
argv <- eval(parse(text="list(1.2e+07, FALSE, NULL, 9L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

