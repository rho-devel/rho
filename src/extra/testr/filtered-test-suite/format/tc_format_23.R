expected <- eval(parse(text="c(\"172\", \"88\", \"88\", \"55\", \"92\", \"92\", \"72\", \"72\", \"63\", \"63\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(172, 88, 88, 55, 92, 92, 72, 72, 63, 63), TRUE, NULL, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

