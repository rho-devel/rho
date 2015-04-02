expected <- eval(parse(text="c(\"2.5\", \"97.5\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(2.5, 97.5), TRUE, 3, 0L, NULL, 3L, TRUE, FALSE)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

