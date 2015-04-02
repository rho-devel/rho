expected <- eval(parse(text="c(1.5, 1.5)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(FALSE, FALSE), 2L, \"average\")"));        
.Internal(rank(argv[[1]], argv[[2]], argv[[3]]));        
}, o=expected);        

