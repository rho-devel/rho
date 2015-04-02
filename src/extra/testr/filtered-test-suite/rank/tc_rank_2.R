expected <- eval(parse(text="numeric(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(), 0L, \"average\")"));        
.Internal(rank(argv[[1]], argv[[2]], argv[[3]]));        
}, o=expected);        

