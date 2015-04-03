expected <- eval(parse(text="\"3.1410000000000\""));            
test(id=0, code={            
argv <- eval(parse(text="list(3.141, FALSE, NULL, 13L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

