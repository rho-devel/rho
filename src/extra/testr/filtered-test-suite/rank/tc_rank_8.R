expected <- eval(parse(text="c(2, 1, 3, 4, 5)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(2, 1, 3, 4, 5), 5L, \"average\")"));        
.Internal(rank(argv[[1]], argv[[2]], argv[[3]]));        
}, o=expected);        

