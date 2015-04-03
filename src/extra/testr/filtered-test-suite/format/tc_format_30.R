expected <- eval(parse(text="\"0+1i\""));            
test(id=0, code={            
argv <- eval(parse(text="list(0+1i, TRUE, NULL, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

